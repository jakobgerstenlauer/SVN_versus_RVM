glue<-function(...){paste(...,sep="")}

init.logging<-function(text){
  #get current date and replace hyphens by underline
  Date<-gsub(pattern="-", replacement="_",Sys.Date())
  #paste new filename
  fileName<-paste("Log_KMLMM_term_project_",Date,".log",sep="")
  cat(text, file = fileName, sep = " ", fill = TRUE,
      append = FALSE)
}

logging<-function(text){
  #get current date and replace hyphens by underline
  Date<-gsub(pattern="-", replacement="_",Sys.Date())
  #paste new filename
  fileName<-paste("Log_KMLMM_term_project_",Date,".log",sep="")
  cat(text, file = fileName, sep = " ", fill = TRUE,
      append = TRUE)
}

check.numeric.values<-function(x,min_length){
  stopifnot(exists("x"))
  stopifnot(is.numeric(x))
  stopifnot(length(x)>=min_length)
}


check.character.values<-function(x,min_length){
  stopifnot(exists("x"))
  stopifnot(is.character(x))
  stopifnot(length(x)>=min_length)
}

check.data.frames<-function(x, min_rows, min_columns){
  stopifnot(exists("x"))
  stopifnot(is.data.frame(x))
  stopifnot(dim(x)[1]>=min_rows)
  stopifnot(dim(x)[2]>=min_columns)
}



#############################################################################################
# Instance generator 
#
# Instance generator with three dimensions:
# dim 1: signal-to-noise ratio (between 0: no signal only noise and 1: only signal no noise)
# dim 2: size of the data set N
# dim 3: number of parameters D
# polynomialDegree: power of the inputs that affects output
# additional optional arguments:
# withInteractions: should interactions between the variables be included (default is FALSE)
# isDebug: Should debug statements be printed? Default is FALSE.
#
#############################################################################################

instance.generator<-function(signal_to_noise_ratio, N, D, polynomialDegree, isDebug=FALSE){

  #test preconditions
  check.numeric.values(signal_to_noise_ratio,1);
  check.numeric.values(N,1);
  check.numeric.values(D,1);
  check.numeric.values(polynomialDegree,1);
  stopifnot(signal_to_noise_ratio>=0)
  stopifnot(signal_to_noise_ratio<=1)
  stopifnot(D>=1)
  stopifnot(N>=1)  

  #for test purposes:
  # signal_to_noise_ratio= 0.99
  # N=5
  # D=2
  # polynomialDegree = 2
  # isDebug=TRUE

  #Here the variability in the data is based on a hierarchical Gaussian model:
  #The mean and the variance of the inputs and the coefficients follow a normal distribution.
  
  #means of the inputs
  means<-rnorm(n=D, mean=0, sd=1)
  if(isDebug) print(means)
  
  #standard deviations of the inputs
  sds<-abs(rnorm(n=D, mean=1, sd=1))
  if(isDebug) print(sds)
  
  d<-data.frame("output"=rep(0,N))

  #the coefficients of the polynomials of the inputs on the output are normally distributed
  #with mean 0 and standard deviation 1:
  for(nrVar in 1:D){
    for(power in 1:polynomialDegree){
      eval(parse(text=
                   glue("coefs_",nrVar,"_",power,"<-rnorm(n=1, 
                        mean=0,
                        sd=1)")
      ));
    }
  }

  for(nrVar in 1:D){
    eval(parse(text=
                 glue("d$input_",nrVar,"<-rnorm(n=N, 
                      mean=means[",nrVar,"],
                      sd=abs(sds[",nrVar,"]))")
               ));
  }

  #additive linear component for each variable
  #and for each power function of the input (from 1 to polynomial degree)
  for(nrVar in 1:D){
    for(power in 1:polynomialDegree){
      eval(parse(text=glue("d$output <- d$output + coefs_",nrVar,"_",power," * d$input_",nrVar,"**",power
      )));
    }
  }
  
  #add Gaussian noise 
  d$output <- rnorm(n=N, mean=d$output, sd= 1-signal_to_noise_ratio)
  
  return(d)
}

#How to call the instance generator:
#d1<-instance.generator(signal_to_noise_ratio=0.5, N=100, D=10, polynomialDegree=4, isDebug=FALSE)
  
######################################################################################################
#Calculates k-fold-cross-validation for a kernelized SVM regression
#
#Arguments:
#response.name: the name of the response (output) as a "string",
#data: the data set,
#c: the C parameter,
#eps: the epsilon parameter,
#k: number of cross-validation folds, defaults to 10.
#p: polynomial degree of the kernel to be fitted
#
#return value: a numeric vector with three components 
#1: mean coefficient of determination for validation data 
#2: standard deviation of the coefficient of determination for validation data
#3: sparsity index (1 - ratio of data used as support vectors)
#4: standard deviation of the sparsity index (ratio of data used as support vectors)
#
#Code adapted from: 
#http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
#######################################################################################################
ksvm.CV<-function(response.name, data, c, eps, p, k=10){
  
  #check preconditions
  stopifnot(exists("response.name"))
  stopifnot(is.character(response.name))
  stopifnot(exists("c"))
  stopifnot(c>0)
  stopifnot(exists("eps"))
  stopifnot(eps>0)
  stopifnot(exists("data"))
  #at least 10 rows
  stopifnot(nrow(data)>9)
  #at least 2 columns 
  stopifnot(ncol(data)>1)
  stopifnot(k<=nrow(data))
  
  require(kernlab)
  
  #anonymous function calculating squared errors
  calculateSquaredErrors<-function(x,y){
    sum((x-y)**2)
  }
  
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  
  #sparsity 
  sparsity<-numeric(length=k)
  
  #The mean squared prediction error of the regression model for a given 
  #pair of a training and a test data set: 
  meanSquaredError<-numeric(length=k)
  
  #The null hypothesis is given by a global mean:
  meanSquaredErrorNullHypothesis<-numeric(length=k)
  
  #The R-squared value: % of total variance explained
  #Total variance is defined as variance of the null model.
  rSquared<-numeric(length=k)
  
  #Perform k fold cross validation
  for(i in 1:k){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData    <- data[testIndexes, ]
    trainData   <- data[-testIndexes, ]
    
    eval(parse(text=glue(
      "m1.svm <- ksvm(",response.name,"~.,
      data=trainData,scales=rep(FALSE, dim(data)[2]),
      type=\"eps-svr\",epsilon=eps,C=c,
      kernel='polydot', kpar=list(degree=p, scale=1, offset=1))"
    )))
    
    stopifnot(exists("m1.svm"))
    
    sparsity[i] <-1 - (m1.svm@nSV / dim(data)[1])
    predictions<-predict(m1.svm, newdata=testData)
    
    eval(parse(text=glue(
      "meanSquaredError[i] <- calculateSquaredErrors(predictions, testData$",response.name,")"
    )))
    
    eval(parse(text=glue(
      "meanSquaredErrorNullHypothesis[i] <- calculateSquaredErrors(mean(testData$",response.name,"), testData$",response.name,")"
    )))
    
    rSquared[i] <- (meanSquaredErrorNullHypothesis[i] - meanSquaredError[i]) / meanSquaredErrorNullHypothesis[i]
  }
  return(c(mean(rSquared),sd(rSquared), mean(sparsity), sd(sparsity) ))
}

is.invalid<-function(x){
  is.nan(x)||is.infinite(x)
}

isInvalidResult<-function(result, param_name, param_value){
  if(is.invalid(result[1])){
    print(paste("Invalid result for ", param_name,":",param_value))
    return (TRUE);
  } 
  return (FALSE);
}

gridPoly<-function(value.optim){
  value.optim<-round(value.optim)
  #for the degree 1 is the minimum
  minV <- max(value.optim - 1, 1);
  maxV <- value.optim + 1;
  return(c(minV, value.optim, maxV))
}

#Adapt the optimization grid for parameter C
#Here we work on the base 2 scale!
gridC<-function(value.optim){
  value.optim<-log2(value.optim)
  minV <- max(value.optim - (1 / step**2) * value.optim, 0.001);
  maxV <- value.optim + (1 / step**2) * value.optim;
  return(2**c(minV, value.optim, maxV))
}

#Adapt the optimization grid for parameter epsilon.
#Here we work on the base 10 scale!
gridEpsilon<-function(value.optim){
  value.optim<-log10(value.optim)
  minV <- max(value.optim - (1 / step**2) * value.optim, 0.001);
  maxV <- value.optim + (1 / step**2) * value.optim;
  return(10**c(minV, value.optim, maxV))
}

#Calculates a new grid for parameter optimization
#
#value.optim: current estimate for the optimal value
#step: current iteration
#type: select the appropriate parameter: "C","epsilon","poly".
updateGrid <- function(value.optim, step, type) {
  stopifnot(value.optim>0.00001)
  switch(type,
    poly =  gridPoly(value.optim),
    C = gridC(value.optim),
    epsilon = gridEpsilon(value.optim)
  )
}

#Calculates n times k-fold-cross-validation for a kernelized SVM regression
#Arguments:
#n: number of replicates, default 10
#data: the data set
#c: the C parameter
#eps: the epsilon parameter
#return value: mean and sd of the prediction error and the sparsity ratio 
#as average over all n replicates!
ksvm.10x10CV<-function(response.name, data, c, eps, p, n=10,k=10){
  
  #check preconditions
  stopifnot(exists("response.name"))
  stopifnot(is.character(response.name))
  stopifnot(exists("c"))
  stopifnot(c>0)
  stopifnot(exists("eps"))
  stopifnot(eps>0)
  stopifnot(exists("data"))
  #at least 10 rows
  stopifnot(nrow(data)>9)
  #at least 2 columns 
  stopifnot(ncol(data)>1)
  stopifnot(k<=nrow(data))
  
  r<-replicate(n, ksvm.CV(response.name, data,c,eps, p, k))
  r<-apply(r,1,mean)
  names(r)<-c("r2.mean","r2.sd","sparsity.mean","sparsity.sd")
  return(r)
}

optim.parameter<-function(result.optim, param.optim, grid, param_name, data, c.optim, epsilon.optim, polynomial.degree.optim, numCVReplicates){
 
  startTime <- Sys.time()
  
  print(paste("Optimize parameter:",param_name))
  print(paste("with grid:",grid))
  print(paste("and fixed params epsilon:",epsilon.optim))
  print(paste("and fixed params C:",c.optim))
  print(paste("and fixed params poly:",polynomial.degree.optim))
  
  #check preconditions
  #Here I do not check result.optim because it is NULL in the first call!
  check.numeric.values(grid,3)
  check.character.values(param_name,1)
  check.data.frames(data, min_rows=10, min_columns=2)
  check.numeric.values(c.optim,1)
  check.numeric.values(epsilon.optim,1)
  check.numeric.values(polynomial.degree.optim,1)
  check.numeric.values(numCVReplicates,1)
  
 for (param in grid) {
    
    result <- switch(
      param_name,
      "epsilon" = ksvm.10x10CV(data,
                               response.name = "output",
                               c = c.optim,
                               eps = param,
                               p = round(polynomial.degree.optim),
                               n = numCVReplicates),
      
      "C" = ksvm.10x10CV(data,
                         response.name = "output",
                         c = param,
                         eps = epsilon.optim,
                         p = round(polynomial.degree.optim),
                         n = numCVReplicates),
      
      "poly" = ksvm.10x10CV(data,
                            response.name = "output",
                            c = c.optim,
                            eps = epsilon.optim,
                            p = round(param),
                            n = numCVReplicates)
    )
    
    if(exists("result")){
      if (isInvalidResult(result, param_name, param)) {
          print("Skip this run because there is no valid result!");
      }else if(length(result.optim)==0){#this is the case for the first model run
          result.optim <- result;
          if(param_name=="poly")param<-round(param);
          param.optim <- param;
          print(paste("First run:",param_name, "optim:", param.optim));
      }else if (result[1] > result.optim[1]){
          result.optim <- result;
          if(param_name=="poly")param<-round(param);
          param.optim <- param;
          print(paste(param_name, "optim:", param.optim));
      }
    }else{
      stop("Missing result!")
    }
 }
  
  #type: select the appropriate parameter: "C","epsilon","poly".
  grid <- updateGrid(param.optim, step, type=param_name)
  endTime <- Sys.time();
  time.used <- endTime - startTime
  return(list(new.grid=grid, result=result.optim, parameter=param.optim, time=time.used))
}

