###############################################################################################
# Kernel-Based Learning & Multivariate Modeling DMKM Master - MIRI Master
# Lecturer: Lluıs A. Belanche, belanche@cs.upc.edu
# Term project
# October-December 2016  
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
# Date: 22.12.2016
# Jakob Gerstenlauer
# jakob.gerstenlauer@gjakob.de
# Francisco Pèrez
# pacogppl@gmail.com
###############################################################################################

#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd("D:/Documents/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")
source("properties.R")

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

#V1: signal-to-noise ratio
signal.to.noise.ratio.grid<-rep(0,SampleSize*maxReplicatesLHC)

#V2: number of observations N
num.observations.grid<-rep(0,SampleSize*maxReplicatesLHC)

#V3: number of variables D
num.vars.grid<-rep(0,SampleSize*maxReplicatesLHC)

#V4: polynomial degree of the inputs.
polynomial.degree.grid<-rep(0,SampleSize*maxReplicatesLHC)

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
  
  for(replicate in 1:maxReplicatesLHC){
    
    #calculate the number of variables based on the ratio observations / variables which is stored in A3
    D<-round(A2/A3)
    
    #Create the new data set with the specific variables
    d<-instance.generator(signal_to_noise_ratio=A1, N=round(A2), D, polynomialDegree=round(A4));
    
    signal.to.noise.ratio.grid[i]<-A1;
    num.observations.grid[i]<-round(A2);
    num.vars.grid[i]<-D;
    polynomial.degree.grid[i]<-round(A4);
    
    i<-i+1
    
    dump.file.name<-glue("data_signal_to_noise_", A1,
                         "_N_", round(A2),
                         "_D_", D,
                         "_poly_", round(A4),
                         "_replicate_",replicate,
                         ".RData");
    save(list="d", file=dump.file.name);
    file.names<-c(file.names, dump.file.name);
  }
}     

file.names<-file.names[-1]

#################################################################################
#
# Relevance Vector Machine
#
#################################################################################

require("kernlab")
sample.size <- length(file.names) 

#Now I only store the results of the best model for each file!

#Id of the parameter combination (has maxReplicatesLHC replicates!)
id_parameter_combination<-vector(mode="numeric",length=sample.size) 

#vectors for optimal parameters
polynomial_degree_setting<-vector(mode="numeric",length=sample.size)

#vectors for cross-validation error mean and sd
cv.mean<-vector(mode="numeric",length=sample.size)
cv.sd<-vector(mode="numeric",length=sample.size)

#vectors for sparsity mean and sd
sparsity<-vector(mode="numeric",length=sample.size)
sd.sparsity<-vector(mode="numeric",length=sample.size)

#vector for computation time
compu.time<-vector(mode="numeric",length=sample.size)

header<-paste(c("fileName", "opt.step", "parameter", "opt.value", "comput.time"), 
              sep="\t")
init.logging(text=header, fileName="Log_KMLMM_term_project")
#index for files
i<-1
#index for parameter combination
j<-1

for(fileName in file.names){
  print(paste("read input file:", fileName))
  load(fileName)
  
  #Here I have to declare the variable without initialising it,
  #because in the first call to optim.parameter() it is a necessary argument.
  result.optim<-NULL
  
  #For each parameter combination there are maxReplicatesLHC replicates.
  #Index j identifies the replicate.
  id_parameter_combination[i]<-j
  if((i %% maxReplicatesLHC)==0){
    j<-j+1
  }
  
  #start values for parameters
  cv.mean.max <- 0
  total.sim.time<-0
  
  #optimize polynomial degree
  o <-
    optim.parameter.rvm(
      result.optim,
      initial.poly.grid,
      data = d,
      numCVReplicates
    )
  
  if(is.null(o)){
    logging(paste("No valid result for file:",fileName, "step:", step),
            fileNameBase="Log_KMLMM_term_project")
  }else{
    poly.grid  <- o$new.grid
    poly.optim <- o$parameter
    result.optim  <- o$result
    time.spent  <- o$time
    total.sim.time <- total.sim.time + time.spent
    logging(paste(fileName, "poly:", poly.optim, time.spent, sep="\t"),
            fileNameBase="Log_KMLMM_term_project")
    rm(o)
  }
  
  if(is.invalid(result.optim)){
    logging(paste("No valid result.optim object for file:",fileName),
            fileNameBase="Log_KMLMM_term_project")
  }else{
    
    #optimal parameters
    polynomial_degree_setting[i] <- poly.optim
    
    #cross-validation error mean and sd
    cv.mean[i] <- result.optim[1]
    cv.sd[i] <- result.optim[2]
    
    #sparsity mean and sd
    sparsity[i] <- result.optim[3]
    sd.sparsity[i] <- result.optim[4]
    compu.time[i]<-total.sim.time
  }
  
  #The index i has to run over all input files.
  #I store only one result for each file!
  #I override results if the new model is better
  #(has higher mean coefficient of determination).
  i <- i + 1
}

#Set up a data set with all the results of the simulation
d.results<-data.frame(
  compu.time,
  signal.to.noise.ratio=signal.to.noise.ratio.grid,
  num.vars=num.vars.grid,
  num.observations=num.observations.grid,
  polynomial.degree=polynomial.degree.grid,
  id_parameter_combination,
  #optimal parameters
  polynomial_degree_setting,
  #cross-validation error mean and sd
  cv.mean,
  cv.sd,
  #sparsity mean and sd
  sparsity,
  sd.sparsity)

setwd(dataDir)
#get current date and replace hyphens by underline
Date<-gsub(pattern="-", replacement="_",Sys.Date())
#paste new filename
fileName<-paste("Results_Simulation_RVM_KMLMM_term_project_",Date,".csv",sep="")
write.table(d.results, file=fileName, append=FALSE, row.names = FALSE, sep = ";")

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

#Id of the parameter combination (has maxReplicatesLHC replicates!)
id_parameter_combination<-vector(mode="numeric",length=sample.size) 

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

#vector for computation time
compu.time<-vector(mode="numeric",length=sample.size)

#index for files
i<-1

#index for parameter combination
j<-1

for(fileName in file.names){
  print(paste("read input file:", fileName))
  load(fileName)
  
  #For each parameter combination there are maxReplicatesLHC replicates.
  #Index j identifies the replicate.
  id_parameter_combination[i]<-j
  if((i %% maxReplicatesLHC)==0){
    j<-j+1
  }
  
  #start values for parameters
  cv.mean.max <- 0
  c.optim <- C.start
  epsilon.optim <- Epsilon
  poly.optim <- polynomial_degree
  total.sim.time<-0
  
  #reset all grids to initial/ default search grid
  poly.grid<-initial.poly.grid
  epsilon.grid<-initial.epsilon.grid
  c.grid<-initial.c.grid
  
  #Here I have to declare the variable without initialising it,
  #because in the first call to optim.parameter() it is a necessary argument.
  result.optim<-NULL
  
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
    
    if(is.null(o)){
      logging(paste("No valid result for file:",fileName, "step:", step),
              fileNameBase="Log_KMLMM_term_project")
    }else{
      
      epsilon.grid  <- o$new.grid
      epsilon.optim <- o$parameter
      result.optim  <- o$result
      time.spent  <- o$time
      total.sim.time <- total.sim.time + time.spent
      logging(paste(fileName, step, "epsilon:", epsilon.optim, time.spent),
              fileNameBase="Log_KMLMM_term_project")
      rm(o)
    }
    
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
    

    if(is.null(o)){
      logging(paste("No valid result for file:",fileName, "step:", step),
              fileNameBase="Log_KMLMM_term_project")
    }else{
      
      c.grid  <- o$new.grid
      c.optim <- o$parameter
      result.optim  <- o$result
      time.spent  <- o$time
      total.sim.time <- total.sim.time + time.spent
      logging(paste(fileName, step, "C:", c.optim, time.spent),
              fileNameBase="Log_KMLMM_term_project")
      rm(o)
    }
    
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
    
    if(is.null(o)){
      logging(paste("No valid result for file:",fileName, "step:", step),
              fileNameBase="Log_KMLMM_term_project")
    }else{
      poly.grid  <- o$new.grid
      poly.optim <- o$parameter
      result.optim  <- o$result
      time.spent  <- o$time
      total.sim.time <- total.sim.time + time.spent
      logging(paste(fileName, step, "poly:", poly.optim, time.spent, sep="\t"),
              fileNameBase="Log_KMLMM_term_project")
      rm(o)
    }
    
    if(is.invalid(result.optim)){
      logging(paste("No valid result.optim object for file:",fileName),
              fileNameBase="Log_KMLMM_term_project")
    }else{
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
  }
  
  compu.time[i]<-total.sim.time
  
  #The index i has to run over all input files.
  #I store only one result for each file!
  #I override results if the new model is better
  #(has higher mean coefficient of determination).
  i <- i + 1
}

#Set up a data set with all the results of the simulation
d.results<-data.frame(
  compu.time,
  signal.to.noise.ratio=signal.to.noise.ratio.grid,
  num.vars=num.vars.grid,
  num.observations=num.observations.grid,
  polynomial.degree=polynomial.degree.grid,
  id_parameter_combination,
  #optimal parameters
  c_setting,
  epsilon_setting, 
  polynomial_degree_setting,
  #cross-validation error mean and sd
  cv.mean,
  cv.sd,
  #sparsity mean and sd
  sparsity,
  sd.sparsity)

setwd(dataDir)
#get current date and replace hyphens by underline
Date<-gsub(pattern="-", replacement="_",Sys.Date())
#paste new filename
fileName<-paste("Results_Simulation_SVM_KMLMM_term_project_",Date,".csv",sep="")
write.table(d.results, file=fileName, append=FALSE, row.names = FALSE, sep = ";")