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

#utility function
glue<-function(...){paste(...,sep="")}

#define path of standard directories
source("workingDir.R")

#read functions from external code file
setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

#Step 1: define the LH scheme 
require("lhs")
#TODO Define number of replications for LHC and Cross-validation!
numCVReplicates<-1
#set-up the Latin Hypercube sampling scheme
SampleSize<-10
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

signal.to.noise.ratio.grid<-rep(0,SampleSize)
num.vars.grid<-rep(0,SampleSize)
num.observations.grid<-rep(0,SampleSize)
polynomial.degree.grid<-rep(0,SampleSize)

i<-1
for (simulation in seq(1,dim(LHS)[1]))
{
  for (arguments in seq(1,NumVariables))
  {   
    #Here we use the quantile function for the uniform distribution to "translate" from the standard uniform distribution to the respective trait range
    eval(parse(text=paste(
      'A',arguments,'<-round(qunif(LHS[simulation,',arguments,'], min=low_V',arguments,', max=high_V',arguments,'),digits=3)'
      ,sep="")));
  } 
  
  #Create the new data set with the specific variables
  d<-instance.generator(signal_to_noise_ratio=A1, N=round(A2), D=round(A3), polynomialDegree=round(A4));
 
  signal.to.noise.ratio.grid[i]<-A1;
  num.vars.grid[i]<-round(A2);
  num.observations.grid[i]<-round(A3);
  polynomial.degree.grid[i]<-round(A4);
  
  i<-i+1
  
  dump.file.name<-glue("data_signal_to_noise_", A1,
                       "_N_", A2,
                       "_D_", A3,
                       "_poly_", round(A4),
                       ".RData");
  save(list="d", file=dump.file.name);
  file.names<-c(file.names, dump.file.name);
}     

file.names<-file.names[-1]


#################################################################################
#
# SVM regression, 
# For each data set, the parameter combination with the lowest prediction error is recorded!
# The parameter ranges are adapted as a result of the conclusion from the first analysis.
#
#
#################################################################################

require("kernlab")
sample.size <- length(file.names) 

#initial sample values
c.grid<-1:5
epsilon.grid<-seq(from=0.1,to=0.9,by=0.1)
poly.grid<-1:5

#start values
C.start=2
Epsilon=0.3
polynomial_degree=3

#Now I only store the results of the best model for each file!

#vectors for optimal parameters
c_setting<-vector(mode="numeric",length=sample.size)  
epsilon_setting<-vector(mode="numeric",length=sample.size)  
polynomial_degree_setting<-vector(mode="numeric",length=sample.size)

#vectors for cross-validation error mean and sd
cv.mean<-vector(mode="numeric",length=sample.size)
cv.sd<-vector(mode="numeric",length=sample.size)

#vectors for sparsity mean and sd
sparsity<-vector(mode="numeric",length=sample.size)
sd.sparsity<-vector(mode="numeric",length=sample.size)

#how many iterations should be run? 
#(in each iteration we do a line-search for each of the three hyper-parameters)
maxStep<-3

is.invalid<-function(x){
  is.nan(x)||is.infinite(x)
}

#index for files
i<-1
for(fileName in file.names){
  
  print(paste("read input file:",fileName))
  load(fileName)
  
  #start values for parameters
  cv.mean.max <- 0
  c.optim <- C.start
  epsilon.optim <- Epsilon
  polynomial.degree.optim <- polynomial_degree
  
  for (step in 1:maxStep) {
    print(paste("optim step:",step))
    for (epsilon in epsilon.grid) {
      result <- ksvm.10x10CV(
        data = d,
        response.name = "output",
        c = c.optim,
        eps = epsilon,
        p = polynomial.degree.optim,
        n = numCVReplicates
      )
      
      if(is.invalid(result[1])){
        print(paste("Invalid result for epsilon:", epsilon))
        next;
      } 
      
      if (result[1] > cv.mean.max) {
        epsilon.optim <- epsilon
        print(paste("epsilon optim:",epsilon.optim))
        cv.mean.max <- result[1]
      }
    }
    
    epsilon.grid <- updateGrid(epsilon.optim, step)
    
    for (C_ in c.grid) {
      result <- ksvm.10x10CV(
        data = d,
        response.name = "output",
        c = C_,
        eps = epsilon.optim,
        p = polynomial.degree.optim,
        n = numCVReplicates
      )
      
      if(is.invalid(result[1])){
        print(paste("Invalid result for c:",C_))
        next;
      } 
      
      if (result[1] > cv.mean.max) {
        c.optim <- C_
        print(paste("C optim:",c.optim))
        cv.mean.max <- result[1]
      }
    }
    
    c.grid <- updateGrid(c.optim, step)
    
    for (polynomial.degree in poly.grid) {
      result <- ksvm.10x10CV(
        data = d,
        response.name = "output",
        c = c.optim,
        eps = epsilon.optim,
        p = polynomial.degree,
        n = numCVReplicates
      )
      
      if(is.invalid(result[1])){
        print(paste("Invalid result for poly:",polynomial.degree))
        next;
      } 
      
      if (result[1] > cv.mean.max) {
        polynomial.degree.optim <- polynomial.degree
        print(paste("poly optim:", polynomial.degree.optim))
        cv.mean.max <- result[1]
        if (step == maxStep) {
          #guardar=save / almacenar=store
          
          #optimal parameters
          c_setting[i] <- c.optim
          epsilon_setting[i] <- epsilon.optim
          polynomial_degree_setting[i] <- polynomial.degree.optim
          
          #cross-validation error mean and sd
          cv.mean[i] <- result[1]
          cv.sd[i] <- result[2]
          
          #sparsity mean and sd
          sparsity[i] <- result[3]
          sd.sparsity[i] <- result[4]
          
        }
      }
    }
    
    poly.grid <- updateGrid(polynomial.degree.optim, step, poly=TRUE)
  }
  
  #The index i has to run over all input files.
  #I store only one result for each file!
  #I override results if the new model is better
  #(has higher mean coefficient of determination).
  i <- i + 1
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















