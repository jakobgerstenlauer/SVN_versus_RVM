###############################################################################################
# Kernel-Based Learning & Multivariate Modeling DMKM Master - MIRI Master
# Lecturer: Lluıs A. Belanche, belanche@cs.upc.edu
# Term project
# October-Dezember 2016  
# "Compare the Relevance Vector machine and the SVM for regression in terms of predictive accuracy 
# and sparsity."
#
# First, an instance generater is set-up.
# Second, we define a latin-hypercube (LH) scheme for efficient sampling of parameter space.
# Third, for each parameter combination of the LH sampling scheme, one instance (data set) is generated.
# Fourth, for each instance we tune the parameters of the SVM and the kernel.
#   Based on the optimum parameters (both kernel and SVM parameters, criteria: cv error) 
#   we record generalization error, sparsity, and total computation time (including tuning) for the selected model.
# Sixth, for each instance we tune the parameters of the RVM and the kernel.
#   Based on the optimum parameters (both kernel and RVM parameters, criteria: cv error) 
#   we record generalization error, sparsity, and total computation time (including tuning) for the selected model.
# Seventh, we analyse the results of step 5 and 6 comparing SVM and RVM across the following dimensions:
#   dim 1: signal-to-noise ratio (between 0: no signal only noise and 1: only signal no noise),
#   dim 2: number of observations N,
#   dim 3: number of parameters D,
#   dim 4: polynomial degree of the inputs.
# Eight, we compare the two models asking the following questions:
#   Question 1: How often do the models choose the appropriate ploynomial degree of the kernel?
#   Question 2: How does the predictive accuracy of both models depend on N, D, and signal-to-noise ratio?
#
# Date: 26.11.2016
# Jakob Gerstenlauer
# jakob.gerstenlauer@gjakob.de
# Francisco Pèrez
# pacogppl@gmail.com
###############################################################################################

#remove old objects for safety resons
rm(list=ls(all=TRUE))

#TODO set is local variable (on local laptop disc)
isLocal<-FALSE

#utility function
glue<-function(...){paste(...,sep="")}

#define path of standard directories
source("workingDir.R")

#read functions from external code file
setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

#Step 1: define the LH scheme 
require("lhs")
#set-up the Latin Hypercube sampling scheme
SampleSize<-100
NumVariables<-4                            
LHS<-improvedLHS(n=SampleSize, k=NumVariables, dup=1)

#Now define the marginals for all five parameters:

#V1: signal-to-noise ratio
low_V1= 0.1;
high_V1= 0.9;

#V2: number of observations N
low_V2  = 10;
high_V2 = 1000;

#V3: number of variables D
low_V3  = 2;
high_V3 = 100;

#V4: polynomial degree of the inputs.
low_V4  = 2;
high_V4 = 5;

setwd(dataDir)
file.names<-""

for (simulation in seq(1,dim(LHS)[1]))
{
  for (arguments in seq(1,NumVariables))
  {   
    #Here we use the quantile function for the uniform distribution to "translate" from the standard uniform distribution to the respective trait range
    eval(parse(text=paste(
      'A',arguments,'<-round(qunif(LHS[simulation,',arguments,'], min=low_A',arguments,', max=high_A',arguments,'),digits=3)'
      ,sep="")));
  } 
  
  #Create the new data set with the specific variables
  d<-instance.generator(signal_to_noise_ratio=A1, N=round(A2), D=round(A3), polynomialDegree=round(A4));
 
  dump.file.name<-glue("data_signal_to_noise_", A1,
                       "_N_", A2,
                       "_D_", A3,
                       "_poly_", round(A4),
                       ".RData");
  save(list="d", file=dump.file.name);
  file.names<-c(file.names, dump.file.name);
}     

file.names<-file.names[-1]



signal.to.noise.ratio.grid<-seq(from=0.1,to=0.9,by=0.1)
num_vars<-10
num_observations<-100
file.names<-""
polynomial_degree<-3

for(s in signal.to.noise.ratio.grid){
  for(repetition in 1:5){
  print(s)
  d<-instance.generator(signal_to_noise_ratio=s, N=num_observations, D=num_vars, polynomialDegree=polynomial_degree)
  dump.file.name<-glue("data_signal_to_noise_", s,
                       "_rep_",repetition,
                       "_N_", num_observations,
                       "_D_", num_vars,".RData")
  save(list="d", file=dump.file.name)
  file.names<-c(file.names, dump.file.name)
  }
}



#################################################################################
#
# SVM regression
#
#################################################################################

grid.c<-c(1,3,5)
grid.epsilon<-seq(from=0.1,to=0.9,by=0.1)
sample.size <- length(file.names) * length(grid.c) * length(grid.epsilon) 

#here I have to reverse engineer the signal to noise ratio as a vector for all data sets
stn<-seq(from=0.1,to=0.9,by=0.1)
signal_to_noise_setting<-vector()
for(stn in signal.to.noise.ratio.grid){
  signal_to_noise_setting<-c(signal_to_noise_setting,rep(stn, 10 * length(grid.c) * length(grid.epsilon)))
}

c_setting<-vector(mode="numeric",length=sample.size)  
epsilon_setting<-vector(mode="numeric",length=sample.size)
cv.mean<-vector(mode="numeric",length=sample.size)
cv.sd<-vector(mode="numeric",length=sample.size)
sparsity<-vector(mode="numeric",length=sample.size)
sd.sparsity<-vector(mode="numeric",length=sample.size)

i<-1
for(fileName in file.names){
  print(fileName)
  load(fileName)
  
  for(C in grid.c){
    for(Epsilon in grid.epsilon){
      print(paste("Step:",i,"from:",sample.size))
      result<-ksvm.10x10CV(data=d, response.name="output",c=C, eps=Epsilon, p=polynomial_degree,n=10)
      c_setting[i]<-C
      epsilon_setting[i]<-Epsilon
      cv.mean[i]<-result[1]
      cv.sd[i]<-result[2]
      sparsity[i]<-result[3]
      sd.sparsity[i]<-result[4]
      i<-i+1
    }
  }
}

require(lattice)
setwd(plotDir)

jpeg("distribution_cross_validation_rsquare_svm.jpeg")
densityplot(~cv.mean)
dev.off()

jpeg("cross_validation_rsquare_svm_versus_signal_to_noise_ratio.jpeg")
xyplot(cv.mean ~ signal_to_noise_setting)
dev.off()

cor(cv.mean, signal_to_noise_setting)
#0.006095
#0.32

jpeg("cross_validation_rsquare_svm_epsilon_c.jpeg")
xyplot(cv.mean ~ epsilon_setting | c_setting, auto.key = TRUE)
dev.off()

jpeg("cross_validation_rsquare_svm_epsilon_c_signal_to_noise_ratio.jpeg")
xyplot(cv.mean ~ epsilon_setting | c_setting, group = signal_to_noise_setting, auto.key = TRUE)
dev.off()

d_cv<-data.frame(
c_setting,
epsilon_setting,
cv.mean,
cv.sd,
sparsity,
sd.sparsity,
signal_to_noise_setting)

setwd(dataDir)
write.table(d_cv, col.names=FALSE, file="results_cross_validation_1.csv")

m1.lm<-lm(cv.mean ~ c_setting * epsilon_setting + signal_to_noise_setting)
summary(m1.lm)
# Call:
#   lm(formula = cv.mean ~ c_setting * epsilon_setting + signal_to_noise_setting)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.185565 -0.033353  0.000678  0.031924  0.172809 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                0.237628   0.006255  37.988  < 2e-16 ***
#   c_setting                  0.046863   0.001666  28.122  < 2e-16 ***
#   epsilon_setting           -0.232396   0.009908 -23.455  < 2e-16 ***
#   signal_to_noise_setting    0.028589   0.005189   5.509 4.31e-08 ***
#   c_setting:epsilon_setting -0.039970   0.002901 -13.779  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.04923 on 1345 degrees of freedom
# Multiple R-squared:  0.8354,	Adjusted R-squared:  0.8349 
# F-statistic:  1706 on 4 and 1345 DF,  p-value: < 2.2e-16




#################################################################################
#
# SVM regression, 2nd sensitivity analysis
# Changes to the first are:
# For each data set, the parameter combination with the lowest prediction error is recorded!
# The parameter ranges are adapted as a result of the conclusion from the first analysis.
#
#
#################################################################################

grid.c<-c(4, 5, 6)
grid.epsilon<-c(0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25)

#here I have to reverse engineer the signal to noise ratio as a vector for all data sets
stn<-seq(from=0.1,to=0.9,by=0.1)
signal_to_noise_setting<-vector()
for(stn in signal.to.noise.ratio.grid){
  #5 repetitions for each signal to noise setting
  signal_to_noise_setting<-c(signal_to_noise_setting,rep(stn, 5))
}

sample.size <- length(file.names) 

#Now I only store the results of the best model for each file!
c_setting<-vector(mode="numeric",length=sample.size)  
epsilon_setting<-vector(mode="numeric",length=sample.size)  
signal_to_noise_setting<-vector(mode="numeric",length=sample.size)
cv.mean<-vector(mode="numeric",length=sample.size)
cv.sd<-vector(mode="numeric",length=sample.size)
sparsity<-vector(mode="numeric",length=sample.size)
sd.sparsity<-vector(mode="numeric",length=sample.size)

i<-1
for(fileName in file.names){
  print(fileName)
  load(fileName)
  cv.mean.max<-0
  
  for(C in grid.c){
    for(Epsilon in grid.epsilon){
      result<-ksvm.10x10CV(data=d, response.name="output",c=C, eps=Epsilon, p=polynomial_degree,n=10)
      if(result[1]>cv.mean.max){
        cv.mean.max<-result[1]
        c_setting[i]<-C
        epsilon_setting[i]<-Epsilon
        cv.mean[i]<-result[1]
        cv.sd[i]<-result[2]
        sparsity[i]<-result[3]
        sd.sparsity[i]<-result[4]
      }
    }
  }
  #The index i has to run over all input files.
  #I store only one result for each file! 
  #I override results if the new model is better 
  #(has higher mean coefficient of determination).
  i<-i+1
}

require(lattice)
setwd(plotDir)

jpeg("distribution_cross_validation_rsquare_svm_2.jpeg")
densityplot(~cv.mean)
dev.off()

jpeg("cross_validation_rsquare_svm_versus_signal_to_noise_ratio_2.jpeg")
xyplot(cv.mean ~ signal_to_noise_setting)
dev.off()

cor(cv.mean, signal_to_noise_setting)
#0.006095
#0.32

jpeg("cross_validation_rsquare_svm_epsilon_c_2.jpeg")
xyplot(cv.mean ~ epsilon_setting | c_setting, auto.key = TRUE)
dev.off()

jpeg("cross_validation_rsquare_svm_epsilon_c_signal_to_noise_ratio_2.jpeg")
xyplot(cv.mean ~ epsilon_setting | c_setting, group = signal_to_noise_setting, auto.key = TRUE)
dev.off()

d_cv<-data.frame(
  c_setting,
  epsilon_setting,
  cv.mean,
  cv.sd,
  sparsity,
  sd.sparsity,
  signal_to_noise_setting)

setwd(dataDir)
write.table(d_cv, col.names=FALSE, file="results_cross_validation_2.csv")

# Weird results: Is there only a positive effect of signal to noise ratio for C=4?
# > with(subset(d_cv, c_setting==5),cor(cv.mean, signal_to_noise_setting))
# [1] -0.2081667
# > with(subset(d_cv, c_setting==4),cor(cv.mean, signal_to_noise_setting))
# [1] 0.3216072
# > with(subset(d_cv, c_setting==6),cor(cv.mean, signal_to_noise_setting))
# [1] -0.07561136















