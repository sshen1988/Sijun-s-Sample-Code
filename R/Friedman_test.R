rm(list=ls()) 
library(dplyr)
library(sas7bdat)
library(devtools)
library(geohash)
library(glmnet)
library(pROC)
library(corrplot)
library(e1071)
library(caret)
library(gbm)
library(iml)
library(PRROC)

options(digit=9)
set.seed(123)


# function to get lasso results
get_lasso_results <- function(dataSet, features, train_index, prescale = FALSE) {
    if (prescale) {
        dataSet <- dataSet %>% mutate_at(features[features != "resp_var"], ~(scale(.) %>% as.vector))
    }
    
    y <- dataSet$resp_var
    x <- model.matrix(resp_var~.,dataSet[, features])[,-1]
    
    train <- train_index
    test <- (-train)
    
    cv.out <- cv.glmnet(x[train,],y[train], alpha=1, type.measure="auc", family = "binomial", standardize = !prescale)
    best_lambda <- cv.out$lambda.min
    best_model <- glmnet(x[train,], y[train], alpha = 1, family = "binomial", standardize = !prescale,
                         lambda = best_lambda)
    
    print("##########Lasso coefficients#############")
    print(paste("Best lambda:", best_lambda))
    print(coef(best_model))
    
    # predicted_prob <- best_model %>% predict(newx = x[test,], type="response") %>% as.vector()
    
    return(best_model)
}

# predict values
get_prediction <- function(model, X, y) {
    predicted_prob <- model %>% predict(newx = X, type="response") %>% as.vector()
    return(data.frame(y, predicted_prob))
    
}


get_lasso_AUC <- function(dataSet, features, train_index, prescale=FALSE) {
    # function used to get the lasso AUC
    if (prescale) {
        dataSet <- dataSet %>% mutate_at(features[features != "Resp_var"], ~(scale(.) %>% as.vector))
    }
    
    model <- get_lasso_results(dataSet, features, train_index, prescale)
    
    test <- (-train_index)
    x <- model.matrix(resp_var~.,dataSet[, features])[,-1]
    y <- dataSet$resp_var
    
    outcome <- get_prediction(model, x[test,], y[test]) # get predictions
    
    res <- roc(outcome[, 1], outcome[, 2], plot=F, legacy.axes=T, 
               xlab="False Positive Percentage", ylab="True Positive Percentage", 
               print.auc=T, col=1, main="Lasso")
    
    return(res$auc)
    
}

# friedman_test
friedman_test_data <- function(dataSet, feature_list, k, f) {
    # function to conduct friedman_test
    # dataSet: dataSet
    # feature_list: features used to compare
    # k: number of splits of the dataSet
    # f: the machine learning algorithms
    
    result <- matrix(nrow = k, ncol = length(feature_list), -1) # matrix to store the AUC values
    
    m <- nrow(dataSet)
    dataSet <- dataSet[sample(1:m, m), ] # shuffle the data
    
    set.seed(123)
    for(i in 1:k) {
        print(paste("Dataet:", i))
        
        data_temp <- dataSet[round(m / k * (i - 1)):round(m / k * i), ]
        train_index <- sample(1:nrow(data_temp), nrow(data_temp) * 0.7)
        
        for (j in 1:length(feature_list)) {
            result[i, j] <- f(data_temp, feature_list[[j]], train_index)
        }
        
    }
    
    return(result)    
    
}

get_long_data <- function(AUC, k) {
    AUC <- as.vector(t(AUC))
    D <- rep(1:k, each=length(AUC) / k)
    feat <- rep(c("tele", "crash", "tele+crash"), k)
    
    result <- data.frame(D, feat, AUC)
    result <- result[order(result$D), ]
    result$rank <- unlist(with(result, tapply(-AUC,D,rank)))
    
    print(friedman.test(rank ~ feat | D, data = result))
    
    return(result)
    
}

get_BDT_results <- function(dataSet, features, train_index, test=T) {
    # function to get the best boosted decision tree
    data_analysis <- dataSet[, features]
    data_analysis$resp_var <- data_analysis$resp_var %>% as.factor()
    levels(data_analysis$resp_var) <- c("no_crash", "crash")
    
    if (test) {
        hyper_grid <- expand.grid(
            shrinkage = c(.006, 0.008, 0.01, 0.012),
            interaction.depth = c(1, 3, 5, 7),
            n.trees = c(350, 400, 450, 500),
            n.minobsinnode = c(30, 50, 70)
        )
    } else {
        hyper_grid <- expand.grid(
            shrinkage = c(0.01, 0.012, 0.014, 0.016),
            interaction.depth = c(5, 7, 9 , 11),
            n.trees = c(450, 500, 550),
            n.minobsinnode = c(30, 50, 70)
        )
    }
    
    
    ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                         repeats=5,         # do 5 repetitions of cv
                         summaryFunction=twoClassSummary,   # Use AUC to pick the best model
                         classProbs=T)
    
    train.gbm <- train(as.factor(resp_var)~.,
                       data = data_analysis[train_index, ],
                       method="gbm",
                       metric="ROC",
                       verbose=F,
                       tuneGrid = hyper_grid,
                       trControl=ctrl)
    
    
    data_analysis <- dataSet[, features]
    boost_res <- gbm(resp_var~., 
                     data=data_analysis[train_index, ], 
                     distribution='bernoulli', 
                     n.trees=train.gbm$bestTune$n.trees, 
                     interaction.depth=train.gbm$bestTune$interaction.depth, 
                     shrinkage = train.gbm$bestTune$shrinkage
                     , n.minobsinnode = train.gbm$bestTune$n.minobsinnode
    )
    print(paste("Best No.trees:", train.gbm$bestTune$n.trees))
    print(paste("Best interaction depth:", train.gbm$bestTune$interaction.depth))
    print(paste("Best shrinkage value:", train.gbm$bestTune$shrinkage))
    print(paste("Best mininum number of observations:", train.gbm$bestTune$n.minobsinnode))
    
    # importance of the variables
    # summary(boost_res, las = 2)
    
    # predicted_prob <- boost_res %>% predict(data_analysis[-train_index, ],
    # n.trees = train.gbm$bestTune$n.trees, type="response") %>% as.vector()
    
    return(boost_res)
}

get_BDT_AUC <- function(dataSet, features, train_index) {
    # function to get the AUC for boosted decision tree
    boosted_model <- get_BDT_results(dataSet, features, train_index)
    
    test <- (-train_index)
    X <- dataSet[test, features]
    y <- dataSet$resp_var[test]
    
    predicted_prob <- boosted_model %>% predict(X, boosted_model$n.trees, type="response")
    
    outcome <-data.frame(y, predicted_prob)
    
    res <- roc(outcome[, 1], outcome[, 2], plot=F, legacy.axes=T, 
               xlab="False Positive Percentage", ylab="True Positive Percentage", 
               print.auc=T, col=1, main="BDT")
    
    return(res$auc)
}

##############################################################################
# read dataset in r
dataSet <- read.csv("./Data/data_analysis_neighbor.csv", header=T, stringsAsFactors=F)
crash_2017 <- read.csv("./Data/crash_count_2017_bymonth.csv", header = T, stringsAsFactors=F)
crash_2018 <- read.csv("./Data/crash_count_2018_bymonth.csv", header = T, stringsAsFactors=F)
crash_2019 <- read.csv("./Data/crash_count_2019_bymonth.csv", header = T, stringsAsFactors=F)

###############################
# merge by month data
crash_2017 <- filter(crash_2017, month > 4)
crash_2018_1 <- filter(crash_2018, month <= 4)
crash_2018_2 <- filter(crash_2018, month > 4)
crash_2019 <- filter(crash_2019, month <= 4)

crash_2017 <- crash_2017 %>% 
                group_by(geohash) %>% 
                summarise(n_crash_2017_5_12 = sum(n_crashes))
crash_2018_1 <- crash_2018_1 %>%
                group_by(geohash) %>%
                summarise(n_crash_2018_1_4 = sum(n_crashes))
crash_2018_2 <- crash_2018_2 %>%
                    group_by(geohash) %>%
                    summarise(n_crash_2018_5_12 = sum(n_crashes))
crash_2019 <- crash_2019 %>% 
    group_by(geohash) %>% summarise(n_crash_2019_1_4 = sum(n_crashes))
    

dataSet <- list(dataSet, crash_2017, crash_2018_1, crash_2018_2, crash_2019) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="geohash"), .)
dataSet[is.na(dataSet)] <- 0

dataSet$new_predictor <- dataSet$n_crash_2017_5_12 + dataSet$n_crash_2018_1_4
dataSet$new_preded_var <- dataSet$n_crash_2018_5_12 + dataSet$n_crash_2019_1_4


names(dataSet)[1] <- "ones"

dataSet$resp_var <- dataSet$new_preded_var
dataSet$resp_var <- ifelse(dataSet$resp_var > 0, 1, 0) 
dataSet$n_crash_1617 <- dataSet$n_crash_2016 + dataSet$n_crash_2017

dataSet <- dataSet %>% mutate(hacount_mean = hacount_sum / num_neighbor, 
                              hbcount_mean = hbcount_sum / num_neighbor,
                              hccount_mean = hccount_sum / num_neighbor)

feature_list <- list()
feature_list[[1]] <- c("resp_var", "hacount", "hbcount", "hccount", "magnitudeavgavg",
                       "hacount_sum", "hbcount_sum", "hccount_sum", "magnitudeavgavg_max")
feature_list[[2]] <- c("resp_var", "n_crash_2018_1_4", "ones")
feature_list[[3]] <- c("resp_var", "n_crash_2018_1_4", "hacount", "hbcount", "hccount", "magnitudeavgavg",
                       "hacount_sum", "hbcount_sum", "hccount_sum", "magnitudeavgavg_max")

summary(dataSet[, feature_list[[3]]])

for (feat in feature_list[[3]]) {
    m <- mean(dataSet[, feat])
    s <- sd(dataSet[, feat])
    print(paste(feat, "mean:", round(m, 1), "sd:", round(s, 1)))
}

for (feat in feature_list[[3]]) {
    s <- min(dataSet[, feat])
    m <- median(dataSet[, feat])
    l <- max(dataSet[, feat])
    print(paste(feat, "min:", round(s, 1), "median:", round(m, 1), "max: ", round(l, 1)))
}


###################################################################################################
# Friedman test
pdf("Nemenyi_test_Lasso_BDT_3.pdf", width=12, height=6)
par(mar = c(5, 8, 3, 2), mfrow=c(1, 2))
# get lasso result
N <- 10 # number of datasets
AUC <- friedman_test_data(dataSet, feature_list, N, get_lasso_AUC)
result <- get_long_data(AUC, N) # p-value < 4.54e-05 for lasso, p-value = 0.0005005 for BDT

# post-hoc test is needed 
alpha <- 0.05
k <- 3
CD <- qtukey(1 - alpha, k, Inf) / sqrt(2) * sqrt(k * (k + 1) / (6 * N))

Nemenyi_test <- result %>% group_by(feat) %>% summarise(mean_rank = mean(rank))
print(Nemenyi_test)
plot(1, type="n", xlab="Mean rank", ylab="", xlim=c(0.5, 4), ylim=c(0.4, 1.8), yaxt='n', xaxt='n')
axis(1, at=c(1, 2, 3), labels=c(1.0, 2.0, 3.0))
axis(2, at=c(1.5, 1, 0.5), labels=c("telemetric + crash", "crash", "telemetric"), las=1)

points(Nemenyi_test$mean_rank[1], 1, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[1] - CD / 2, Nemenyi_test$mean_rank[1] + CD / 2), c(1, 1), lwd=2)
# abline(v=Nemenyi_test$mean_rank[1] + CD / 2, lty=2, col="gray")
# abline(v=Nemenyi_test$mean_rank[1] - CD / 2, lty=2, col="gray")
# text(Nemenyi_test$mean_rank[1] , 1.6, "Critical difference", col = "red")

points(Nemenyi_test$mean_rank[2], 0.5, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[2] - CD / 2, Nemenyi_test$mean_rank[2] + CD / 2), c(0.5, 0.5), lwd=2)

points(Nemenyi_test$mean_rank[3], 1.5, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[3] - CD / 2, Nemenyi_test$mean_rank[3] + CD / 2), c(1.5, 1.5), lwd=2)
abline(v=Nemenyi_test$mean_rank[3] + CD / 2, lty=2, col="gray")
text(Nemenyi_test$mean_rank[3] , 1.55, "Critical difference", col = "red", cex=0.8)

title(main="(a) Nemenyi test for RLR", adj=0, line=1, font.main=1)

# AUC <- friedman_test_data(dataSet, feature_list, N, get_BDT_AUC)
# result <- get_long_data(AUC, N) # p-value < 4.54e-05 for lasso, p-value = 0.0005005 for BDT

# post-hoc test is needed
result <- read.csv("./Data/Frideman_test_BDT.csv", header=T)
alpha <- 0.05
k <- 3
CD <- qtukey(1 - alpha, k, Inf) / sqrt(2) * sqrt(k * (k + 1) / (6 * N))

Nemenyi_test <- result %>% group_by(feat) %>% summarise(mean_rank = mean(rank))
print(Nemenyi_test)
plot(1, type="n", xlab="Mean rank", ylab="", xlim=c(0.5, 4), ylim=c(0.4, 1.8), yaxt='n', xaxt='n')
axis(1, at=c(1, 2, 3), labels=c(1.0, 2.0, 3.0))
axis(2, at=c(1.5, 1, 0.5), labels=c("telemetric + crash", "crash", "telemetric"), las=1)

points(Nemenyi_test$mean_rank[1], 1, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[1] - CD / 2, Nemenyi_test$mean_rank[1] + CD / 2), c(1, 1), lwd=2)
# abline(v=Nemenyi_test$mean_rank[1] + CD / 2, lty=2, col="gray")
# abline(v=Nemenyi_test$mean_rank[1] - CD / 2, lty=2, col="gray")
# text(Nemenyi_test$mean_rank[1] , 1.6, "Critical difference", col = "red")

points(Nemenyi_test$mean_rank[2], 0.5, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[2] - CD / 2, Nemenyi_test$mean_rank[2] + CD / 2), c(0.5, 0.5), lwd=2)

points(Nemenyi_test$mean_rank[3], 1.5, type='p', pch=16)
lines(c(Nemenyi_test$mean_rank[3] - CD / 2, Nemenyi_test$mean_rank[3] + CD / 2), c(1.5, 1.5), lwd=2)
abline(v=Nemenyi_test$mean_rank[3] + CD / 2, lty=2, col="gray")
text(Nemenyi_test$mean_rank[3] , 1.55, "Critical difference", col = "red", cex=0.8)

title(main="(b) Nemenyi test for BDT", adj=0, line=1, font.main=1)
dev.off()

################################################################
# 5 * 2 cv paired t test
get_train_index <- function(dataSet) {
    return(sample(1:nrow(dataSet), nrow(dataSet) / 2))
}

fivebytwo_t_test <- function(pA1s, pA2s, pB1s, pB2s) {
    # function to run t test
    p1 <- pA1s - pB1s
    p2 <- pA2s - pB2s
    p_bar <- (p1 + p2) / 2
    s_square <- (p1 - p_bar) ^ 2 + (p2 - p_bar) ^ 2
    
    t <- p1[1] / sqrt(1 / 5 * sum(s_square))
    
    return(2 * pt(-abs(t),df = 5))
}


pA1s <- rep(-1, 5)
pA2s <- rep(-1, 5)
pB1s <- rep(-1, 5)
pB2s <- rep(-1, 5)
s_square <- rep(-1, 5)

set.seed(123)
for (i in 1:5) {
    train_index <- get_train_index(dataSet)
    pA1s[i] <- get_lasso_AUC(dataSet, feature_list[[3]], train_index)
    pA2s[i] <- get_lasso_AUC(dataSet, feature_list[[3]], -train_index)
    
    pB1s[i] <- get_BDT_AUC(dataSet, feature_list[[3]], train_index)
    pB2s[i] <- get_BDT_AUC(dataSet, feature_list[[3]], -train_index)
}

fivebytwo_t_test(pA1s, pA2s, pB1s, pB2s) # p value = 0.01418182

# > pA1s
# [1] 0.7616192 0.7667538 0.7573443 0.7601961 0.7619032
# > pA2s
# [1] 0.7624712 0.7578668 0.7672116 0.7640872 0.7616489
# > pB1s
# [1] 0.7692998 0.7767018 0.7689152 0.7716237 0.7742579
# > pB2s
# [1] 0.7743857 0.7699588 0.7777354 0.7734611 0.7700611

#####################################################################################
# draw ROC curves for lasso
res <- NULL
AUCs <- rep(-1, length(feature_list))
counter <- 1
set.seed(123)
train_index <- sample(1:nrow(dataSet), nrow(dataSet) * 0.7)

jpeg(file="ROC_plot_Lasso_2.jpg")
for(features in feature_list){
    
    # get the best lasso result
    model <- get_lasso_results(dataSet, features, train_index, prescale=F)
    
    test <- (-train_index)
    x <- model.matrix(resp_var~.,dataSet[, features])[,-1]
    y <- dataSet$resp_var
    outcome <- get_prediction(model, x[test,], y[test])
    
    # draw ROC plots
    if (is.null(res)) {
        par(pty='s', mfrow=c(1, 1))
        res <- roc(outcome[, 1], outcome[,2], plot=T, legacy.axes=T, 
                   xlab="False Positive Percentage", ylab="True Positive Percentage", 
                   print.auc=F, col=counter, lty=counter, lwd=counter)
    } else {
        res <- plot.roc(outcome[, 1], outcome[,2], col=counter, print.auc=F, add=T, print.auc.y=0.4, 
                        lty=counter, lwd=counter)
        
    }
    
    AUCs[counter] <- res$auc
    
    
    counter = counter + 1
}

# title(main="(a) ROCs for RLR", font.main=1, line=-1)
legend.txt <- c("Telemetric variables", "Crash variable", "Telemetric + Crash variables")
legend(0.7, 0.2, paste(legend.txt, round(AUCs, 2), sep=": "), col=1:3, lty=1:3, lwd=1:3, bty = "n")
dev.off()

#########################################################################################
# develop boosted decision tree
set.seed(123)
train_index <- sample(1:nrow(dataSet), nrow(dataSet) * 0.7)
models <- list()
AUCs <- rep(-1, 3)
counter <- 1
res <- NULL

jpeg(file="ROC_plot_BDT.jpg")

for(features in feature_list){
    print(counter)
    
    # get the best lasso result
    boosted_model <- get_BDT_results(dataSet, features, train_index, test=F)
    
    models[[counter]] <- boosted_model
    # boosted_model <- models[[counter]]
    
    test <- (-train_index)
    X <- dataSet[test, features]
    y <- dataSet$resp_var[test]
    
    predicted_prob <- boosted_model %>% predict(X, boosted_model$n.trees, type="response")
    
    outcome <-data.frame(y, predicted_prob)
    
    # draw ROC plots
    if (is.null(res)) {
        par(pty='s', mfrow=c(1, 1))
        res <- roc(outcome[, 1], outcome[,2], plot=T, legacy.axes=T, 
                   xlab="False Positive Percentage", ylab="True Positive Percentage", 
                   print.auc=F, col=counter, lty=counter, lwd=counter)
    } else {
        res <- plot.roc(outcome[, 1], outcome[,2], col=counter, print.auc=F, add=T, print.auc.y=0.4, 
                        lty=counter, lwd=counter)
        
    }
    
    AUCs[counter] <- res$auc
    
    
    counter = counter + 1
}

# title(main="(b) ROCs for BDT", adj=0, line=1, font.main=1)
legend.txt <- c("Telemetric variables", "Crash variable", "Telemetric + Crash variables")
legend(0.7, 0.2, paste(legend.txt, round(AUCs, 2), sep=": "), col=1:3, lty=1:3, lwd=1:3, bty = "n")
dev.off()

for (i in 1:length(models)) {
    print(paste("Best shrinkage value:", models[[i]]$shrinkage))
    print(paste("Best No.trees:", models[[i]]$n.trees))
    print(paste("Best interaction depth:", models[[i]]$interaction.depth))
    print(paste("Best mininum number of observations:", models[[i]]$n.minobsinnode))
}










# 
# ###################################################################################
# # plot the importance plots
set.seed(123)
train_index <- sample(1:nrow(dataSet), nrow(dataSet) * 0.7)
features <- c("resp_var", "n_crash_2018", "hacount", "hbcount", "hccount", "magnitudeavgavg",
              "hacount_sum", "hbcount_sum", "hccount_sum", "magnitudeavgavg_max")

# get the best lasso result
model <- get_lasso_results(dataSet, features, train_index, prescale=T)
importance <- coef(model) %>% as.vector()
importance <- importance[-1]

model <- get_lasso_results(dataSet, features, train_index, prescale=F)
importance <- coef(model) %>% as.vector()
output_table <- data.frame(coef(model) %>% as.matrix(), exp(coef(model)) %>% as.matrix())
write.csv(output_table, file="Lasso.csv")

##########################################################################################
# develop boosted decision tree
set.seed(123)
train_index <- sample(1:nrow(dataSet), nrow(dataSet) * 0.7)
models <- list()
AUCs <- rep(-1, 3)
counter <- 1
res <- NULL

pdf(file="ROC_plot_BDT_2.pdf", 
    width=6, height=6)
for(features in feature_list){
    print(counter)
    
    # get the best lasso result
    boosted_model <- get_BDT_results(dataSet, features, train_index)
    
    models[[counter]] <- boosted_model
    
    test <- (-train_index)
    X <- dataSet[test, features]
    y <- dataSet$resp_var[test]
    
    predicted_prob <- boosted_model %>% predict(X, boosted_model$n.trees, type="response")
    
    outcome <-data.frame(y, predicted_prob)
    
    # draw ROC plots
    if (is.null(res)) {
        par(pty='s', mfrow=c(1, 1))
        res <- roc(outcome[, 1], outcome[,2], plot=T, legacy.axes=T, 
                   xlab="False Positive Percentage", ylab="True Positive Percentage", 
                   print.auc=F, col=counter, lty=counter, lwd=counter)
    } else {
        res <- plot.roc(outcome[, 1], outcome[,2], col=counter, print.auc=F, add=T, print.auc.y=0.4, 
                        lty=counter, lwd=counter)
        
    }
    
    AUCs[counter] <- res$auc
    
    
    counter = counter + 1
}

title(main="ROCs for BDT models", line=3)
legend.txt <- c("Telemetric data only", "Crash data only", "Telemetric + crash data")
legend(0.7, 0.2, paste(legend.txt, round(AUCs, 2), sep=": "), col=1:3, lty=1:3, lwd=1:3, bty = "n")
dev.off()

################################################################################################
# Test importance for lasso and BDT model
model <- get_lasso_results(dataSet, features, train_index, prescale=T)
lasso_importance <- coef(model) %>% as.vector()
lasso_importance <- data.frame(lasso_importance[-1], feature_list[[3]][-1])
BDT_importance <- summary(models[[3]], las = 2, ylab="")

names(lasso_importance) <- c("lasso_importance", "feature")
names(BDT_importance) <- c("feature", "BDT_importance")
importance <- inner_join(lasso_importance, BDT_importance, by="feature")

importance$lasso_importance <- abs(importance$lasso_importance)
k1_lasso <- 100 / sum(importance$lasso_importance)
k2_BDT <- 100 / sum(importance$BDT_importance)
importance <- importance %>%
    mutate(lasso_norm = k1_lasso * lasso_importance)
importance <- importance %>%
    mutate(BDT_norm = k2_BDT * BDT_importance)

jpeg("Importance.jpg")
par(mar = c(5, 8, 3, 2))
importance$y <- 4.4 - seq(from = 0.2, by=0.5, length.out = nrow(importance))

plot(importance$lasso_norm, importance$y, pch=1, xlim=c(0, 100), 
     ylab="", yaxt="n", xlab="Variable importance", col="blue")
axis(2, at=importance$y, labels=c("n_crash", "hacount", "hbcount", "hccount",
                                  "avg_magnitude", "hacount_neigh", 
                                  "hbcount_neigh", "hccount_neigh", "magnitude_neigh"), las=2)
points(importance$BDT_norm, importance$y, pch=4, col="red")
abline(h=importance$y, col = "lightgray", lty = "dotted")
legend.txt <- c("RLR", "BDT")
legend(70, 2.4, legend.txt, pch=c(1, 4), col=c("blue", "red"))
dev.off()

############################################################################
# variable importance
data_analysis <- dataSet[, feature_list[[3]]]
# H-statistics
variables <- names(data_analysis)[names(data_analysis) != "resp_var"]
# variables <- c("n_crash", "hacount", "hbcount", "hccount",
#                "avg_magnitude", "hacount_neigh", 
#                "hbcount_neigh", "hccount_neigh", "magnitude_neigh")
n <- length(variables)
result <- NULL
r_num <- 1

for (i in 1:(length(variables))) {
    for (j in i : length(variables)) {
        temp <- interact.gbm(models[[3]], data=data_analysis[train_index, ], 
                             i.var=c(variables[i], variables[j]), 
                             n.trees = models[[3]]$n.trees)
        result <- rbind(result, c(variables[c(i, j)], temp))
        r_num <- r_num + 1
    }
    
    print(i)
}

# create a H_stat_matrix
H_stat_matrix <- matrix(nrow=n, ncol=n, 1)
colnames(H_stat_matrix) <- variables
rownames(H_stat_matrix) <- variables

for(i in 1:nrow(result)) {
    var1 <- result[i, 1]
    var2 <- result[i, 2]
    val <- result[i, 3] %>% as.numeric()
    
    H_stat_matrix[var1, var2] <- val
    H_stat_matrix[var2, var1] <- val
}

colnames(H_stat_matrix) <- c("n_crash", "hacount", "hbcount", "hccount",
                                            "avg_magnitude", "hacount_neigh", 
                                            "hbcount_neigh", "hccount_neigh", "magnitude_neigh")
rownames(H_stat_matrix) <- c("n_crash", "hacount", "hbcount", "hccount",
                                             "avg_magnitude", "hacount_neigh", 
                                             "hbcount_neigh", "hccount_neigh", "magnitude_neigh")

pdf("interaction_plot.pdf", width=6, height=6)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(H_stat_matrix, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()





# 
# features <- feature_list[[3]]
# data_analysis <- dataSet[, features]
# data_analysis$resp_var <- data_analysis$resp_var %>% as.factor()
# levels(data_analysis$resp_var) <- c("no_crash", "crash")
# 
# # hyper_grid <- expand.grid(
# #     shrinkage = c(.007, .01, .013),
# #     interaction.depth = c(4, 5, 6, 7),
# #     n.minobsinnode = c(4, 5, 6, 7),
# #     n.trees = c(250, 300, 350, 400)
# # )
# 
# hyper_grid <- expand.grid(
#     shrinkage = c(.01, .1, .3),
#     interaction.depth = c(3, 5, 7),
#     n.minobsinnode = c(5, 10, 15),
#     n.trees = c(250, 350, 450)
# ) # n.trees = 250, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode=15
# 
# 
# hyper_grid <- expand.grid(
#     shrinkage = c(.07, .1, .13, 0.15),
#     interaction.depth = c(1, 3, 5, 7, 9),
#     n.minobsinnode = c(45, 55, 60, 75, 90, 95, 100, 105),
#     n.trees = c(400, 500, 550, 600, 650)
# )


# best tune

# > train.gbm$bestTune for feature_list[[1]]
# n.trees interaction.depth shrinkage n.minobsinnode
# 2     500                 5      0.03             90           6      0.03             35

# > train.gbm$bestTune for feature_list[[2]]
# n.trees interaction.depth shrinkage n.minobsinnode
# 82     500                 7       0.1             75


# 
# > train.gbm$bestTune for feature_list[[3]]
# n.trees interaction.depth shrinkage n.minobsinnode
# 21     400                 1      0.07             90
# ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
#                      repeats=5,         # do 5 repetitions of cv
#                      summaryFunction=twoClassSummary,   # Use AUC to pick the best model
#                      classProbs=T)
# 
# train.gbm <- train(as.factor(resp_var)~.,
#                    data = data_analysis[train_index, ],
#                    method="gbm",
#                    metric="ROC",
#                    verbose=F,
#                    tuneGrid = hyper_grid,
#                    trControl=ctrl)

# interaction.depth  n.trees  ROC        Sens       Spec     
# 1                   50      0.7418618  0.9566749  0.2369339
# 1                  100      0.7429333  0.9564432  0.2416869
# 1                  150      0.7436387  0.9559157  0.2428616
# 2                   50      0.7437918  0.9587081  0.2346826
# 2                  100      0.7456038  0.9583350  0.2408070
# 2                  150      0.7462338  0.9573440  0.2447242
# 3                   50      0.7450417  0.9597630  0.2346824
# 3                  100      0.7467436  0.9577559  0.2446782
# 3                  150      0.7472211  0.9569580  0.2484966




# data_analysis <- dataSet[, features]
# boost_res <- gbm(I_crash2017~., 
#                  data=data_analysis[train_index, ], 
#                  distribution='bernoulli', 
#                  n.trees=train.gbm$bestTune$n.trees, 
#                  interaction.depth=train.gbm$bestTune$interaction.depth, 
#                  shrinkage = train.gbm$bestTune$shrinkage, 
#                  n.minobsinnode = train.gbm$bestTune$n.minobsinnode)

# importance of the variables
# summary(boost_res, las = 2)

# predicted_prob <- boost_res %>% predict(data_analysis[-train_index, ],
# n.trees = train.gbm$bestTune$n.trees, type="response") %>% as.vector()

################################################################################################
# Test importance for lasso
model <- get_lasso_results(dataSet, features, train_index, prescale=T)
lasso_importance <- coef(model) %>% as.vector()
lasso_importance <- data.frame(lasso_importance[-1], feature_list[[3]][-1])
BDT_importance <- summary(models[[3]], las = 2, ylab="")

names(lasso_importance) <- c("lasso_importance", "feature")
names(BDT_importance) <- c("feature", "BDT_importance")
importance <- inner_join(lasso_importance, BDT_importance, by="feature")

importance$lasso_importance <- abs(importance$lasso_importance)
importance <- importance %>%
                    mutate(lasso_norm = 
                    (lasso_importance - min(lasso_importance)) / (max(lasso_importance) - min(lasso_importance)))
importance <- importance %>%
    mutate(BDT_norm = 
               (BDT_importance - min(BDT_importance)) / (max(BDT_importance) - min(BDT_importance)))

pdf("Importance.pdf", width=6, height=6)
par(mar = c(5, 8, 3, 2))
importance$y <- 4.4 - seq(from = 0.2, by=0.5, length.out = nrow(importance))

plot(importance$lasso_norm, importance$y, pch=1, ylab="", yaxt="n", xlab="importance", main="Importance", col="blue")
axis(2, at=importance$y, labels=c("No. crash", "hacount", "hbcount", "hccount",
                                  "avg_magnitude", "hacount_neigh", 
                                  "hbcount_neigh", "hccount_neigh", "magnitude_neigh"), las=2)
points(importance$BDT_norm, importance$y, pch=4, col="red")
abline(h=importance$y, col = "lightgray", lty = "dotted")
legend.txt <- c("Lasso", "BDT")
legend(0.6, 2.4, legend.txt, pch=c(1, 4), col=c("blue", "red"))
dev.off()

