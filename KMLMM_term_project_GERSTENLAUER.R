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
#   Question 3: How does the sparsity of both models depend on N, D, and  signal-to-noise ratio?
#
# Git commands:
#   push changes:
#   Jakob@Win16 MINGW64 /j/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code (master)
#   $ git push -u origin master
#   pull changes:
#   Jakob@Win16 MINGW64 /j/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code (master)
#   $ git pull https://jakobgerstenlauer@bitbucket.org/jakobgerstenlauer/kernels_term_project.git
#
# Date: 26.11.2016
# Jakob Gerstenlauer
# jakob.gerstenlauer@gjakob.de
# Francisco Pèrez
# pacogppl@gmail.com
###############################################################################################

#remove old objects for safety resons
rm(list=ls(all=TRUE))

#*******************************************************************************
# Here all parameters are set for the R script:
#*******************************************************************************

#Define number of replications for LHC and Cross-validation!
numCVReplicates<-1

#number of samples from the LHC 
SampleSize<-1

#Now define the ranges for all four parameters of the LHC:
#V1: signal-to-noise ratio
low_V1= 0.1;
high_V1= 0.9;

#V2: number of observations N
low_V2  = 16;
high_V2 = 256;

#V3: number of variables D
low_V3  = 2;
high_V3 = 16;

#V4: polynomial degree of the inputs.
low_V4  = 1;
high_V4 = 3;

#initial sample values
c.grid<-1:5
epsilon.grid<-seq(from=0.1,to=0.9,by=0.1)
poly.grid<-low_V4:high_V4

#start values
C.start=2
Epsilon=0.3
polynomial_degree=3

#how many iterations should be run? 
#(in each iteration we do a line-search for each of the three hyper-parameters)
maxStep<-5
#*******************************************************************************

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
setwd("E:/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")

#read functions from external code file
setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

#Step 1: define the LH scheme 
require("lhs")

#set-up the Latin Hypercube sampling scheme
NumVariables<-4                            
LHS<-improvedLHS(n=SampleSize, k=NumVariables, dup=1)

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
  num.vars.grid[i]<-floor(A2);
  num.observations.grid[i]<-floor(A3);
  polynomial.degree.grid[i]<-round(A4);
  
  i<-i+1
  
  dump.file.name<-glue("data_signal_to_noise_", A1,
                       "_N_", floor(A2),
                       "_D_", floor(A3),
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

#Here I have to declare the variable without initialising it,
#because in the first call to optim.parameter() it is a necessary argument.
result.optim<-NULL

header<-paste(c("fileName", "opt.step", "parameter", "opt.value", "comput.time"), 
              sep="\t")
init.logging(header)

#index for files
i<-1
for(fileName in file.names){
  print(paste("read input file:", fileName))
  load(fileName)
  
  #start values for parameters
  cv.mean.max <- 0
  c.optim <- C.start
  epsilon.optim <- Epsilon
  poly.optim <- polynomial_degree
  
  for (step in 1:maxStep) {
    print(paste("optim step:", step))
  
    #optimize epsilon  
    o <-
      optim.parameter(
        result.optim,
        param.optim=epsilon.optim,
        epsilon.grid,
        "epsilon",
        data = d,
        c.optim,
        epsilon.optim,
        poly.optim,
        numCVReplicates
      )
    
    epsilon.grid  <- o$new.grid
    epsilon.optim <- o$parameter
    result.optim  <- o$result
    time.spent  <- o$time
    logging(paste(fileName, step, "epsilon:", epsilon.optim, time.spent))
    rm(o)
    
    #optimize C
    o <-
      optim.parameter(
        result.optim,
        param.optim=c.optim,
        c.grid,
        "C",
        data = d,
        c.optim,
        epsilon.optim,
        poly.optim,
        numCVReplicates
      )
    
    c.grid  <- o$new.grid
    c.optim <- o$parameter
    result.optim  <- o$result
    time.spent  <- o$time
    logging(paste(fileName, step, "C:", c.optim, time.spent))
    rm(o)
    
    #optimize polynomial degree
    o <-
      optim.parameter(
        result.optim,
        param.optim=poly.optim,
        poly.grid,
        "poly",
        data = d,
        c.optim,
        epsilon.optim,
        poly.optim,
        numCVReplicates
      )
    
    #I do not change / update the grid!
    #poly.grid  <- o$new.grid
    poly.optim <- o$parameter
    result.optim  <- o$result
    time.spent  <- o$time
    logging(paste(fileName, step, "poly:", poly.optim, time.spent, sep="\t"))
    rm(o)
    
    #optimal parameters
    c_setting[i] <- c.optim
    epsilon_setting[i] <- epsilon.optim
    polynomial_degree_setting[i] <- poly.optim
    
    #cross-validation error mean and sd
    cv.mean[i] <- result.optim[1]
    cv.sd[i] <- result.optim[2]
    
    #sparsity mean and sd
    sparsity[i] <- result.optim[3]
    sd.sparsity[i] <- result.optim[4]
  }
  
  #The index i has to run over all input files.
  #I store only one result for each file!
  #I override results if the new model is better
  #(has higher mean coefficient of determination).
  i <- i + 1
}

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
