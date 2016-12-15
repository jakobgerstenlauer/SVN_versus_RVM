#*******************************************************************************
# Here all parameters are set for the R script:
#*******************************************************************************

#Define number of replications for 10-fold Cross-validation!
#E.g 10 means that we use 10-times 10-fold cross-validation
numCVReplicates<-1

#Define number of replications for each  parameter combination
#sampled in the Latin Hyper Cube.
maxReplicatesLHC<-3

#TODO Check if the sample size is correct!
#number of samples from the LHC 
SampleSize<-200

#Now define the ranges for all four parameters of the LHC:
#V1: signal-to-noise ratio
low_V1= 0.01;
high_V1= 0.99;

#V2: number of observations N
low_V2  = 16;
high_V2 = 256;

#V3: ratio observations / number of variables D
low_V3  = 0.5;
high_V3 = 20;

#V4: polynomial degree of the inputs.
low_V4  = 3;
high_V4 = 7;

#initial sample values
#c.grid<-2**c(0,0.5,1,1.5)
initial.c.grid<-1:5
#epsilon.grid<-10**c(-0.5,0,0.5)
initial.epsilon.grid<-seq(from=0.1,to=0.9,by=0.1)
#Here I restrict the initial search space from 1 to the maximum polynomial degree + 1.
#However, note that the grid for all parameter changes after each optimization step!
initial.poly.grid<-1:(high_V4+3)

#start values
C.start=initial.c.grid[2]
Epsilon=initial.epsilon.grid[2]
polynomial_degree=3

#How many iterations should be run? 
#(in each iteration we do a line-search for each of the three hyper-parameters)
maxStep<-5
#*******************************************************************************
