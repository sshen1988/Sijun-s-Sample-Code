import numpy as np
from collections import Counter
import time
import random
import itertools


class DecisionNode:
    """Class to represent a single node in a decision tree."""

    def __init__(self, left, right, decision_function, class_label=None):
        """Create a decision function to select between left and right nodes.
        Note: In this representation 'True' values for a decision take us to
        the left. This is arbitrary but is important for this assignment.
        Args:
            left (DecisionNode): left child node.
            right (DecisionNode): right child node.
            decision_function (func): function to decide left or right node.
            class_label (float): label for leaf node. Default is None.
        """

        self.left = left
        self.right = right
        self.decision_function = decision_function
        self.class_label = class_label

    def decide(self, feature):
        """Get a child node based on the decision function.
        Args:
            feature (list(int)): vector for feature.
        Return:
            Class label if a leaf node, otherwise a child node.
        """

        if self.class_label is not None:
            return self.class_label

        elif self.decision_function(feature):
            return self.left.decide(feature)

        else:
            return self.right.decide(feature)


def load_csv(data_file_path, class_index=-1):
    """Load csv data in a numpy array.
    Args:
        data_file_path (str): path to data file.
        class_index (int): slice output by index.
    Returns:
        features, classes as numpy arrays if class_index is specified,
            otherwise all as nump array.
    """

    handle = open(data_file_path, 'r')
    contents = handle.read()
    handle.close()
    rows = contents.split('\n')
    out = np.array([[float(i) for i in r.split(',')] for r in rows if r])

    if(class_index == -1):
        classes= out[:,class_index]
        features = out[:,:class_index]
        return features, classes
    elif(class_index == 0):
        classes= out[:, class_index]
        features = out[:, 1:]
        return features, classes

    else:
        return out


def build_decision_tree():
    """Create a decision tree capable of handling the sample data.
    Tree is built fully starting from the root.
    Returns:
        The root node of the decision tree.
    """

    decision_tree_root = None

    # TODO: finish this.
    # raise NotImplemented()
    decision_tree_root = DecisionNode(None, None, lambda feature:feature[0] == 1)
    a4 = DecisionNode(None, None, lambda feature:feature[3] == 0)

    decision_tree_root.left = DecisionNode(None, None, None, 1)
    decision_tree_root.right = a4

    a3 = DecisionNode(None, None, lambda feature:feature[2] == 0)
    a2 = DecisionNode(None, None, lambda feature:feature[1] == 1)

    a4.left = a3
    a4.right = a2

    a3.left = DecisionNode(None, None, None, 1)
    a3.right = DecisionNode(None, None, None, 0)

    a2.left = DecisionNode(None, None, None, 0)
    a2.right = DecisionNode(None, None, None, 1)


    return decision_tree_root


def confusion_matrix(classifier_output, true_labels):
    """Create a confusion matrix to measure classifier performance.
    Output will in the format:
        [[true_positive, false_negative],
         [false_positive, true_negative]]
    Args:
        classifier_output (list(int)): output from classifier.
        true_labels: (list(int): correct classified labels.
    Returns:
        A two dimensional array representing the confusion matrix.
    """

    # TODO: finish this.

    cm = np.zeros((2, 2))

    for true_val, pred_val in zip(true_labels, classifier_output):
        true_val = (true_val + 1) % 2
        pred_val = (pred_val + 1) % 2

        cm[true_val][pred_val] += 1

    return cm



def precision(classifier_output, true_labels):
    """Get the precision of a classifier compared to the correct values.
    Precision is measured as:
        true_positive/ (true_positive + false_positive)
    Args:
        classifier_output (list(int)): output from classifier.
        true_labels: (list(int): correct classified labels.
    Returns:
        The precision of the classifier output.
    """

    # TODO: finish this.
    cm = confusion_matrix(classifier_output, true_labels)
    return cm[0, 0] / (cm[0, 0] + cm[1, 0])


def recall(classifier_output, true_labels):
    """Get the recall of a classifier compared to the correct values.
    Recall is measured as:
        true_positive/ (true_positive + false_negative)
    Args:
        classifier_output (list(int)): output from classifier.
        true_labels: (list(int): correct classified labels.
    Returns:
        The recall of the classifier output.
    """

    # TODO: finish this.
    cm = confusion_matrix(classifier_output, true_labels)
    return cm[0, 0] / (cm[0, 0] + cm[0, 1])

def accuracy(classifier_output, true_labels):
    """Get the accuracy of a classifier compared to the correct values.
    Accuracy is measured as:
        correct_classifications / total_number_examples
    Args:
        classifier_output (list(int)): output from classifier.
        true_labels: (list(int): correct classified labels.
    Returns:
        The accuracy of the classifier output.
    """

    # TODO: finish this.
    classifier_output = np.array(classifier_output)
    true_labels = np.array(true_labels)

    return (classifier_output == true_labels).sum() / len(classifier_output)


def gini_impurity(class_vector):
    """Compute the gini impurity for a list of classes.
    This is a measure of how often a randomly chosen element
    drawn from the class_vector would be incorrectly labeled
    if it was randomly labeled according to the distribution
    of the labels in the class_vector.
    It reaches its minimum at zero when all elements of class_vector
    belong to the same class.
    Args:
        class_vector (list(int)): Vector of classes given as 0 or 1.
    Returns:
        Floating point number representing the gini impurity.
    """
    n = len(class_vector)

    if n == 0: # length of class_vector is 0
        return 0

    _, counts = np.unique(class_vector, return_counts=True)

    prob_square = [(count / n) ** 2 for count in counts]

    return 1 - sum(prob_square)


def gini_gain(previous_classes, current_classes):
    """Compute the gini impurity gain between the previous and current classes.
    Args:
        previous_classes (list(int)): Vector of classes given as 0 or 1.
        current_classes (list(list(int): A list of lists where each list has
            0 and 1 values).
    Returns:
        Floating point number representing the information gain.
    """
    prev_gini_impurity = gini_impurity(previous_classes)
    n = len(previous_classes)

    # for current_class in current_classes:
    #     res -= len(current_class) / n * gini_impurity(current_class)

    cur_gini_impurity = [len(current_class) / n * gini_impurity(current_class) for current_class in current_classes]

    return prev_gini_impurity - sum(cur_gini_impurity)


class DecisionTree:
    """Class for automatic tree-building and classification."""

    def __init__(self, depth_limit=float('inf')):
        """Create a decision tree with a set depth limit.
        Starts with an empty root.
        Args:
            depth_limit (float): The maximum depth to build the tree.
        """

        self.root = None
        self.depth_limit = depth_limit

    def fit(self, features, classes):
        """Build the tree from root using __build_tree__().
        Args:
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
        """

        self.root = self.__build_tree__(features, classes)

    def gini_impurity_2(self, counts):
        n = sum(counts)

        prob_square = [(count / n) ** 2 for count in counts]

        return 1 - sum(prob_square)

    def gini_gain_2(self, counts, left_counts):
        prev_gini_impurity = self.gini_impurity_2(counts)

        left_gini_impurity = sum(left_counts) / sum(counts) * self.gini_impurity_2(left_counts)

        right_counts = [counts[0] - left_counts[0], counts[1] - left_counts[1]]
        right_gini_impurity = sum(right_counts) / sum(counts) * self.gini_impurity_2(right_counts)

        return prev_gini_impurity - left_gini_impurity - right_gini_impurity


    def __build_tree__(self, features, classes, depth=0):
        """Build tree that automatically finds the decision functions.
        Args:
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
            depth (int): depth to build tree to.
        Returns:
            Root node of decision tree.
        """

        # TODO: finish this.
        if len(np.unique(classes)) == 1:
            return DecisionNode(None, None, None, classes[0])


        if depth == self.depth_limit:
            values, counts = np.unique(classes, return_counts=True)
            output = int(values[np.argmax(counts)])
            return DecisionNode(None, None, None, output)

        m, n = features.shape

        values, counts = np.unique(classes, return_counts=True)
        counts = [count for _, count in sorted(zip(values, counts), key=lambda pair:pair[0])] # sort counts by values\
        # so count[0] = counts of zero and count[1] = counts of one

        best_split_feat = None
        best_split_point = None
        best_gini_gain = 0
        for i in range(n):
            feature = features[:, i]
            temp_sorted_pair = [(feat_val, category) for feat_val, category
                           in sorted(zip(feature, classes), key=lambda pair:pair[0])] # sort features

            best_feat_split_point = None
            best_feat_gini_gain = 0

            left_counts = [0, 0] # save the counts for value 0 and 1
            for j in range(m - 1): # loop through every potential split point
                if temp_sorted_pair[j][1] == temp_sorted_pair[j + 1][1]: # no need to check for the information gain
                    left_counts[int(temp_sorted_pair[j][1])] += 1
                    continue

                temp_split_point = (temp_sorted_pair[j][0] + temp_sorted_pair[j + 1][0]) / 2

                # current_classes = []
                # current_classes.append([category for _, category in temp_sorted_pair[:j + 1]])
                # current_classes.append([category for _, category in temp_sorted_pair[j + 1:]])

                left_counts[int(temp_sorted_pair[j][1])] += 1
                temp_gain = self.gini_gain_2(counts, left_counts)

                if temp_gain > best_feat_gini_gain:
                    best_feat_split_point = temp_split_point
                    best_feat_gini_gain = temp_gain

            if best_feat_gini_gain > best_gini_gain:
                best_split_feat = i
                best_split_point = best_feat_split_point
                best_gini_gain = best_feat_gini_gain

        left_index = np.where(features[:, best_split_feat] < best_split_point)
        right_index = np.where(features[:, best_split_feat] >= best_split_point)

        left_features = features[left_index]
        left_classes = classes[left_index]

        right_features = features[right_index]
        right_classes = classes[right_index]

        if len(left_classes) == 0 or len(left_classes) == 0:
            # print("no need to go deeper")
            values, counts = np.unique(classes, return_counts=True)
            output = int(values[np.argmax(counts)])
            return DecisionNode(None, None, None, output)


        left_tree = self.__build_tree__(left_features, left_classes, depth + 1)
        right_tree = self.__build_tree__(right_features, right_classes, depth + 1)

        return DecisionNode(left_tree, right_tree, lambda feature:feature[best_split_feat] < best_split_point)


    def classify(self, features):
        """Use the fitted tree to classify a list of example features.
        Args:
            features (m x n): m examples with n features.
        Return:
            A list of class labels.
        """

        class_labels = []

        # TODO: finish this.
        class_labels = [self.root.decide(feat) for feat in features]
        return class_labels

def mean_abs_error(classes):
    y = np.mean(classes)

    res = [abs(x - y) for x in classes]

    return sum(res)

class RegressionTree:
    """Class for automatic tree-building and classification."""

    def __init__(self, tolN, tolS, depth_limit=float('inf')):
        """Create a decision tree with a set depth limit.
        Starts with an empty root.
        Args:
            depth_limit (float): The maximum depth to build the tree.
        """

        self.root = None
        self.depth_limit = depth_limit
        self.tolS = tolS
        self.tolN = tolN

    def fit(self, features, classes):
        """Build the tree from root using __build_tree__().
        Args:
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
        """

        self.root = self.__build_tree__(features, classes)


    def choose_best_split(self, features, classes, tolS, tolN):
        m, n = features.shape

        best_error = float("inf")
        best_feat = None
        best_split_value = None

        total_error = mean_abs_error(classes)

        for feat in range(n):
            for split_value in set(features[:, feat]):
                left_features, left_classes, right_features, right_classes = self.slice_data(features, classes, feat, split_value)

                if len(left_classes) < tolN or len(right_classes) < tolN: # no sufficient observations on either side
                    continue

                temp_error = mean_abs_error(left_classes) + mean_abs_error(right_classes)
                if temp_error < best_error:
                    best_error = temp_error
                    best_feat = feat
                    best_split_value = split_value

        if (total_error - best_error) / len(classes) < tolS:
            return None, float("inf")

        return best_feat, best_split_value


    def slice_data(self, features, classes, feat, split_value):
        left_index = np.where(features[:, feat] < split_value)
        right_index = np.where(features[:, feat] >= split_value)

        left_features = features[left_index]
        left_classes = classes[left_index]

        right_features = features[right_index]
        right_classes = classes[right_index]

        return (left_features, left_classes, right_features, right_classes)


    def __build_tree__(self, features, classes, depth=0):
        """Build tree that automatically finds the decision functions.
        Args:
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
            depth (int): depth to build tree to.
        Returns:
            Root node of decision tree.
        """

        # TODO: finish this.
        # exit if all values are equal
        if len(np.unique(classes)) == 1:
            return DecisionNode(None, None, None, classes[0])

        # exit if reach the maximum depth
        if depth == self.depth_limit:
            output = np.mean(classes)
            return DecisionNode(None, None, None, output)

        feat, split_value = self.choose_best_split(features, classes, self.tolS, self.tolN)

        if feat == None:
            output = np.mean(classes)
            return DecisionNode(None, None, None, output)

        left_features, left_classes, right_features, right_classes = self.slice_data(features, classes, feat,
                                                                                     split_value)
        if len(left_classes) < self.tolN or len(right_classes) < self.tolN:  # no sufficient observations on either side
            output = np.mean(classes)
            return DecisionNode(None, None, None, output)


        left_tree = self.__build_tree__(left_features, left_classes, depth + 1)
        right_tree = self.__build_tree__(right_features, right_classes, depth + 1)

        return DecisionNode(left_tree, right_tree, lambda feature:feature[feat] < split_value)


    def classify(self, features):
        """Use the fitted tree to classify a list of example features.
        Args:
            features (m x n): m examples with n features.
        Return:
            A list of class labels.
        """

        class_labels = []

        # TODO: finish this.
        class_labels = [self.root.decide(feat) for feat in features]
        return class_labels



def generate_k_folds(dataset, k):
    """Split dataset into folds.
    Randomly split data into k equal subsets.
    Fold is a tuple (training_set, test_set).
    Set is a tuple (features, classes).
    Args:
        dataset: dataset to be split.
        k (int): number of subsections to create.
    Returns:
        List of folds.
        => Each fold is a tuple of sets.
        => Each Set is a tuple of numpy arrays.
    """

    # TODO: finish this.
    # raise NotImplemented()
    features, classes = dataset[0], dataset[1]
    m, n = features.shape

    shuffled_indices = np.arange(m)
    np.random.shuffle(shuffled_indices) # shuffle indices

    k_index_list = np.array_split(shuffled_indices, k)

    list_of_folds = []

    test_set_size = m // k

    for kth_index in k_index_list:
        test_indices = kth_index[:test_set_size]
        test_set = (features[test_indices, :], classes[test_indices])

        training_set = (np.delete(features, test_indices, axis=0), np.delete(classes, test_indices))

        list_of_folds.append((training_set, test_set))

    return list_of_folds


def balance_data(features, classes):
    nonzero_features = features[classes != 0]
    nonzero_classes = classes[classes != 0]

    zero_features = features[classes == 0]
    zero_classes = classes[classes == 0]

    nonzero_counts = len(nonzero_classes)
    subsample_indices = np.random.choice(zero_features.shape[0], nonzero_counts, replace=False)
    zero_features = zero_features[subsample_indices]
    zero_classes = zero_classes[subsample_indices]

    features = np.vstack((nonzero_features, zero_features))
    classes = np.concatenate((nonzero_classes, zero_classes))

    return features, classes


class RandomForest:
    """Random forest classification."""

    def __init__(self, num_trees, depth_limit, example_subsample_rate,
                 attr_subsample_rate, tolN, tolS):
        """Create a random forest.
         Args:
             num_trees (int): fixed number of trees.
             depth_limit (int): max depth limit of tree.
             example_subsample_rate (float): percentage of example samples.
             attr_subsample_rate (float): percentage of attribute samples.
        """

        self.trees = []
        self.num_trees = num_trees
        self.depth_limit = depth_limit
        self.example_subsample_rate = example_subsample_rate
        self.attr_subsample_rate = attr_subsample_rate

        self.tolN = tolN
        self.tolS = tolS

        self.attrs = [] # store the selected attributes for trees

    def fit(self, features, classes):
        """Build a random forest of decision trees using Bootstrap Aggregation.
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
        """

        # TODO: finish this.
        # raise NotImplemented()
        features, classes = balance_data(features, classes)

        m, n = features.shape
        subsample_size = int(m * self.example_subsample_rate)
        subattr_size = int(n * self.attr_subsample_rate)

        for _ in range(self.num_trees):
            subsample_indices = np.random.choice(np.arange(m), subsample_size, replace=True) # get subsample indices

            subattr_indices = np.sort(np.random.choice(np.arange(n), subattr_size, replace=False)) # get subattri indices
            self.attrs.append(subattr_indices)

            temp_features = features[subsample_indices, :][:, subattr_indices]
            temp_classes = classes[subsample_indices]

            temp_tree = RegressionTree(self.tolN, self.tolS, self.depth_limit)
            temp_tree.fit(temp_features, temp_classes)
            self.trees.append(temp_tree)


    def classify(self, features):
        """Classify a list of features based on the trained random forest.
        Args:
            features (m x n): m examples with n features.
        """

        # TODO: finish this.
        # raise NotImplemented()
        m, n = features.shape

        cum_labels = np.zeros(m) # array to store the summation of labels from the trees

        for i in range(self.num_trees):
            temp_features = features[:, self.attrs[i]]
            temp_tree = self.trees[i]
            temp_labels = np.array(temp_tree.classify(temp_features))

            cum_labels = cum_labels + temp_labels

        cum_labels = cum_labels / self.num_trees

        return cum_labels.tolist()


class ChallengeClassifier:
    """Challenge Classifier used on Challenge Training Data."""

    def __init__(self):
        """Create challenge classifier.
        Initialize whatever parameters you may need here.
        This method will be called without parameters, therefore provide
        defaults.
        """

        # TODO: finish this.
        self.trees = []
        self.num_trees = 7
        self.depth_limit = 6
        self.example_subsample_rate = 0.5
        self.attr_subsample_rate = 0.5

        self.tolS = 0.1
        self.tolN = 5

        self.random_forest = RandomForest(self.num_trees, self.depth_limit,
                                          self.example_subsample_rate, self.attr_subsample_rate, self.tolN, self.tolS)


    def fit(self, features, classes):
        """Build the underlying tree(s).
            Fit your model to the provided features.
        Args:
            features (m x n): m examples with n features.
            classes (m x 1): Array of Classes.
        """

        # TODO: finish this.
        # raise NotImplemented()
        features, classes = balance_data(features, classes)
        self.random_forest.fit(features, classes)

    def classify(self, features):
        """Classify a list of features.
        Classify each feature in features as either 0 or 1.
        Args:
            features (m x n): m examples with n features.
        Returns:
            A list of class labels.
        """

        # TODO: finish this.
        # raise NotImplemented()
        return self.random_forest.classify(features)

class Vectorization:
    """Vectorization preparation for Assignment 5."""

    def __init__(self):
        pass

    def non_vectorized_loops(self, data):
        """Element wise array arithmetic with loops.
        This function takes one matrix, multiplies by itself and then adds to
        itself.
        Args:
            data: data to be added to array.
        Returns:
            Numpy array of data.
        """

        non_vectorized = np.zeros(data.shape)
        for row in range(data.shape[0]):
            for col in range(data.shape[1]):
                non_vectorized[row][col] = (data[row][col] * data[row][col] +
                                            data[row][col])
        return non_vectorized

    def vectorized_loops(self, data):
        """Element wise array arithmetic using vectorization.
        This function takes one matrix, multiplies by itself and then adds to
        itself.
        Args:
            data: data to be sliced and summed.
        Returns:
            Numpy array of data.
        """

        # TODO: finish this.
        # data = np.array(data)

        return np.multiply(data, data) + data

    def non_vectorized_slice(self, data):
        """Find row with max sum using loops.
        This function searches through the first 100 rows, looking for the row
        with the max sum. (ie, add all the values in that row together).
        Args:
            data: data to be added to array.
        Returns:
            Tuple (Max row sum, index of row with max sum)
        """

        max_sum = 0
        max_sum_index = 0
        for row in range(100):
            temp_sum = 0
            for col in range(data.shape[1]):
                temp_sum += data[row][col]

            if temp_sum > max_sum:
                max_sum = temp_sum
                max_sum_index = row

        return max_sum, max_sum_index

    def vectorized_slice(self, data):
        """Find row with max sum using vectorization.
        This function searches through the first 100 rows, looking for the row
        with the max sum. (ie, add all the values in that row together).
        Args:
            data: data to be sliced and summed.
        Returns:
            Tuple (Max row sum, index of row with max sum)
        """

        # TODO: finish this.
        # raise NotImplemented()
        data = np.array(data)
        temp_sum = data[:100].sum(axis=1)
        max_sum_index = temp_sum.argmax()
        max_sum = temp_sum[max_sum_index]

        return max_sum, max_sum_index


    def non_vectorized_flatten(self, data):
        """Display occurrences of positive numbers using loops.
         Flattens down data into a 1d array, then creates a dictionary of how
         often a positive number appears in the data and displays that value.
         ie, [(1203,3)] = integer 1203 appeared 3 times in data.
         Args:
            data: data to be added to array.
        Returns:
            List of occurrences [(integer, number of occurrences), ...]
        """

        unique_dict = {}
        flattened = np.hstack(data)
        for item in range(len(flattened)):
            if flattened[item] > 0:
                if flattened[item] in unique_dict:
                    unique_dict[flattened[item]] += 1
                else:
                    unique_dict[flattened[item]] = 1

        return unique_dict.items()

    def vectorized_flatten(self, data):
        """Display occurrences of positive numbers using vectorization.
         Flattens down data into a 1d array, then creates a dictionary of how
         often a positive number appears in the data and displays that value.
         ie, [(1203,3)] = integer 1203 appeared 3 times in data.
         Args:
            data: data to be added to array.
        Returns:
            List of occurrences [(integer, number of occurrences), ...]
        """

        # TODO: finish this.
        # raise NotImplemented()
        data = np.array(data)
        unique, counts = np.unique(data[data > 0], return_counts=True)
        return zip(unique, counts)

def return_your_name():
    # return your name
    # TODO: finish this
    return "Sijun Shen"
#
# import decision_trees_submission_tests as test

def load_data():
    my_data = np.genfromtxt('train.csv', delimiter=',')
    classes = my_data[:, -1].astype(int)
    features = my_data[:, :-1]
    return features, classes

if __name__ == '__main__':
    features, classes = load_data()

    # features = features
    # classes = classes
    #
    # nonzero_features = features[classes != 0]
    # nonzero_classes = classes[classes != 0]
    #
    # zero_features = features[classes == 0]
    # zero_classes = classes[classes == 0]
    #
    # nonzero_counts = len(nonzero_classes)
    # subsample_indices = np.random.randint(0, zero_features.shape[1], nonzero_counts)
    # zero_features = zero_features[subsample_indices]
    # zero_classes = zero_classes[subsample_indices]
    #
    #
    # features = np.vstack((nonzero_features, zero_features))
    # classes = np.concatenate((nonzero_classes, zero_classes))

    train_indices = np.random.choice(features.shape[0], int(features.shape[0] * 0.7), replace=False)

    train_features = features[train_indices]
    train_classes = classes[train_indices]

    testing_features = np.delete(features, train_indices, 0)
    testing_classes = np.delete(classes, train_indices)

    test_error = {}

    parameters = [parameter for parameter in itertools.product([10, 100, 500], [10, 20, 30], [0.5], [0.5], [10, 100, 500], [1, 8, 16])]

    print("Original test error:", mean_abs_error(testing_classes) / len(testing_classes))
    for parameter in parameters:
        print(parameter)
        num_tree, depth, subsample_rate, subattr_rate, tolN, tolS = parameter


        random_forest = RandomForest(num_tree, depth, subsample_rate, subattr_rate, tolN, tolS)
        # random_forest = ChallengeClassifier()
        random_forest.fit(train_features, train_classes)

        test_output = random_forest.classify(testing_features)


        test_error[parameter]  = sum([abs(output - testing_class) for output, testing_class
                                      in zip(test_output, testing_classes)]) / len(testing_classes)
        print(parameter, "test error", test_error[parameter])


    sort_orders = sorted(test_error.items(), key=lambda x:x[1])

    for l in sort_orders:
        print(l[0], l[1])

