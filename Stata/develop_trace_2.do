
/// code to duplicate the power estimation
powersim , /// 
b(-0.061154423) alpha(0.025) pos(1) /// log(0.94)
sample(1800) nreps(1000) ///
family(poisson) link(log) /// 
cov1(x1 _bp block 2) /// 
cons(1.774952351)  /// log(5.9), group 1 control 
dofile(example2_dofile, replace) : /// 
glm y x1, family(poisson) link(log)

/// loop through 0.6 0.7 0.8 0.9 intervention main effects
forvalues i = 0.6(0.1)0.9{ 
	display `i'
	
	powersim , /// * calls the powersim package
	b(-0.36 -0.29 -0.22 -0.16 -0.11 -0.05 0 0.05 0.10 0.14 0.18 0.22 0.26) /// * the effect size of interaction 0.7(0.05)1.3
	alpha(0.025) /// * significant level
	pos(3) /// interaction term position, the third beta in the model, and the first and second are the main effects for intervention and sex
	sample(3600) /// proposed sample size
	nreps(1000) /// simulated replications
	family(poisson) /// type of regression
	link(log) /// link function
	cov1(x1 log(`i') block 2) /// x1 is the intervention vs control with block 2
	cov2(x2 0.66 binomial 0.5) /// x2 is the sex variable with 50% the participant is male and 50% the participant is female
	inter1(_bp x1*x2) /// _bp = effect size set in the b(-2.643884) 
	cons(1.50) ///  beta0, the intercept value
	inside /// simulate data inside every loop of replication
	gendata /// // <-- creating a single realization 
	dofile(ex2_dofile, replace) : /// 
	glm y c.x1##c.x2, family(poisson) link(log) // model structure


	powersim , ///
	b(-0.36 -0.29 -0.22 -0.16 -0.11 -0.05 0 0.05 0.10 0.14 0.18 0.22 0.26) alpha(0.025) pos(3) /// 
	sample(3600) nreps(1000) /// 
	family(poisson) link(log) /// 
	cov1(x1 log(`i') block 2) /// 
	cov2(x2 0.66 binomial 0.5) /// 
	inter1(_bp x1*x2) /// 
	cons(1.50) inside /// 
	dofile(example2_dofile, replace) : /// 
	glm y c.x1##c.x2, family(poisson) link(log)

}











