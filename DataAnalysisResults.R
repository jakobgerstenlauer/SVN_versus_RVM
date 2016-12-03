
#Was the correct polynomial degree chosen?
plot(polynomial.degree.grid,polynomial_degree_setting)

#How do the chosen C and Epsilon relate to signal-to-noise ratio,
#N and D?

m1_c.lm<-lm(c_setting ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)
m1_epsilon.lm<-lm(epsilon_setting ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)

#How does the performance of the SVM depend on
#signal-to-noise ratio,
#N and D?

m1_cv_mean.lm<-lm(cv.mean ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)
m1_sparsity.lm<-lm(sparsity ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)