#############################################################################################
#                               Utility Functions
#############################################################################################

#' @title Glue together a list of characters.
#'
#' @description This is a wrapper function for paste which does not use any separator.
glue<-function(...){paste(...,sep="")}

#' @title Initialize a common log file.
#'
#' @description
#' Creates a new log file whose name contains the current date.
#' Only use this function if you want to overwrite existing log files!
#'
#' @details
#' If the log file already consists it is deleted and overwritten.
#' 
#' @param text The log statement which should be printed to the new logfile.
#' @param fileName The root of the file name for the new logfile.
init.logging<-function(text, fileName){
  #get current date and replace hyphens by underline
  Date<-gsub(pattern="-", replacement="_",Sys.Date())
  #paste new filename
  fileName<-paste(fileName, Date,".log",sep="")
  cat(text, file = fileName, sep = " ", fill = TRUE,
      append = FALSE)
}


#' @title Write to a common log file.
#'
#' @description
#' Writes to a log file whose name contains the current date.
#'
#' @details
#' Calls init.logging() if the log file does not yet exists, 
#' else writes directly to the existing file.
#' 
#' @param text The log statement which should be printed to the new logfile.
#' @param fileNameBase The root of the file name for the new logfile.
logging<-function(text, fileNameBase){
  #get current date and replace hyphens by underline
  Date<-gsub(pattern="-", replacement="_",Sys.Date())
  #paste new filename
  fileName<-paste(fileNameBase,Date,".log",sep="")
  if(!file.exists(fileName)){
    init.logging(text, fileNameBase);
  }else{
      cat(text, file = fileName, sep = " ", fill = TRUE,
          append = TRUE)
  }
}


#' @title Test validity of numeric values.
#'
#' @description
#' Checks if a numeric vector with given name exists, is of type numeric, and has the given minimum length.
#' This function should be used to test pre- and postconditions within functions.
#' 
#' @param x The numeric vector to be tested.
#' @param min_length The minimum number of elements of the vector.
check.numeric.values<-function(x,min_length){
  stopifnot(exists("x"))
  stopifnot(is.numeric(x))
  stopifnot(length(x)>=min_length)
}


#' @title Test validity of character values.
#'
#' @description
#' Checks if a character vector with given name exists, is of type character,
#' and has the given minimum length.
#' This function should be used to test pre- and postconditions within functions.
#' 
#' @param x The character vector to be tested.
#' @param min_length The minimum number of elements of the vector.
check.character.values<-function(x,min_length){
  stopifnot(exists("x"))
  stopifnot(is.character(x))
  stopifnot(length(x)>=min_length)
}


#' @title Test validity of data frames.
#'
#' @description
#' Checks if a data frame with given name exists, is of class "data.frame",
#' and has the given minimum number of rows and columns.
#' This function should be used to test pre- and postconditions within functions.
#' 
#' @param x The data frame to be tested.
#' @param min_rows The minimum number of rows of the data frame.
#' @param min_columns The minimum number of columns of the data frame.
check.data.frames<-function(x, min_rows, min_columns){
  stopifnot(exists("x"))
  stopifnot(is.data.frame(x))
  stopifnot(dim(x)[1]>=min_rows)
  stopifnot(dim(x)[2]>=min_columns)
}


#' @title Creates a new problem instance as a data frame.
#'
#' @description
#' This functions creates a new data set (instance) given four different parameters: 
#' signal-to-noise ratio, number of observations, number of features,  and polynomial degree.
#'
#' @param signal-to-noise_ratio Ratio between signal and noise in the data (between 0: no signal only noise and 1: only signal no noise)
#' @param N Number of observations (cases) in the new data set
#' @param D Number of features (inputs) in the new data set
#' @param polynomialDegree Highest power of the inputs affecting the output
#' @param isDebug Should debug statements be printed? Default is FALSE.
#' 
#' @examples d1<-instance.generator(signal_to_noise_ratio=0.5, N=100, D=10, polynomialDegree=4, isDebug=FALSE)
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

#' @title Calculates k-fold-cross-validation for a kernelized relevance vector machine.
#'
#' @description
#' This functions calculates k-fold-cross-validation for a kernelized relevance vector machine.
#' It uses the polynomial kernel with the supplied polynomial degree.
#' The returned list represents averaged results for k cross-validation runs.
#' Cross-validation code adapted from: http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
#' @param response.name: The name of the response (output) as a character
#' @param data: The data set for this analysis
#' @param p: The polynomial degree of the polynomial kernel which will be used by the relevance vector machine
#' @param k: The number of cross-validation folds, defaults to 10
#' 
#' @return A numeric vector with three components:
#' \itemize{
#'  \item{"index 1"}{The mean coefficient of determination for validation data}
#'  \item{"index 2"}{The standard deviation of the coefficient of determination for validation data}
#'  \item{"index 3"}{The mean of the sparsity index, which is 1 - ratio of data used as support vectors}
#'  \item{"index 4"}{The standard deviation of the sparsity index}
#' }
#' 
#' @examples 
krvm.CV<-function(response.name, data, p, k=10){
  
  #check preconditions
  stopifnot(exists("response.name"))
  stopifnot(is.character(response.name))
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
      "m1.rvm <- ksvm(",response.name,"~.,
      data=trainData, scales=rep(FALSE, dim(data)[2]),
      kernel='polydot', kpar=list(degree=p, scale=1, offset=1))"
    )))
    
    stopifnot(exists("m1.rvm"))
    
    sparsity[i] <-1 - (m1.rvm@nSV / dim(data)[1])
    predictions<-predict(m1.rvm, newdata=testData)
    
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

#' @title Calculates k-fold-cross-validation for a kernelized support vector machine.
#'
#' @description
#' This functions calculates k-fold-cross-validation for a kernelized support vector machine.
#' It uses the polynomial kernel with the supplied polynomial degree.
#' The returned list represents averaged results for k cross-validation runs.
#' Cross-validation code adapted from: http://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
#' @param response.name The name of the response (output) as a character
#' @param data The data set for this analysis
#' @param c The C parameter
#' @param eps The epsilon parameter
#' @param p The polynomial degree of the polynomial kernel which will be used by the relevance vector machine
#' @param k The number of cross-validation folds, defaults to 10
#' 
#' @return A numeric vector with four elements:
#' \itemize{
#'  \item{"index 1"}{The mean coefficient of determination for validation data}
#'  \item{"index 2"}{The standard deviation of the coefficient of determination for validation data}
#'  \item{"index 3"}{The mean of the sparsity index, which is 1 - ratio of data used as support vectors}
#'  \item{"index 4"}{The standard deviation of the sparsity index}
#' }
#' 
#' @examples 
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

#' @title Checks if argument if is NAN or infinite.
#'
#' @param x The argument to test.
#'
#' @return Boolean indicating that argument is a valid value.
is.invalid<-function(x){
  is.nan(x)||is.infinite(x)
}

#' @title Tests if a result object contains valid values for a given model parameter.
#'
#' @param result The result object to test.
#' @param param_name Name of the parameter to test.
#' @param param_value Value of the parameter to test.
#'
#' @return Boolean indicating if result object has valid value for requested parameter.
isInvalidResult<-function(result, param_name, param_value){
  if(is.invalid(result[1])){
    print(paste("Invalid result for ", param_name,":",param_value))
    return (TRUE);
  } 
  return (FALSE);
}

#' @title Update optimization grid based on current optimum for the polynomial degree.
#'
#' @param value.optim The current optimum value for the parameter.
#'
#' @return A numeric vector with three elements which can be used as updated optimization grid.
gridPoly<-function(value.optim){
  value.optim<-round(value.optim)
  #for the degree 1 is the minimum
  minV <- max(value.optim - 1, 1);
  maxV <- value.optim + 1;
  return(c(minV, value.optim, maxV))
}

#' @title Update optimization grid based on current optimum for the parameter C of the support vector machine.
#'
#' @description This function adapts the optimization grid for C. 
#' Based on a recommendation of Lluis Belanche, we work on the base 2 scale!
#' Note that we use a simulted annealing idea: the step size of the grid decreases with increasing number of iterations (step).
#' 
#' @param value.optim The current optimum value for C.
#' @param step The optimization step.
#'
#' @return A numeric vector with three elements which can be used as updated optimization grid.
gridC<-function(value.optim, step){
  value.optim<-log2(value.optim)
  minV <- max(value.optim - (1 / step**2) * value.optim, 0.001);
  maxV <- value.optim + (1 / step**2) * value.optim;
  return(2**c(minV, value.optim, maxV))
}

#' @title Update optimization grid based on current optimum for the parameter \epsilon of the support vector machine.
#'
#' @description This function adapts the optimization grid for \epsilon. 
#' Based on a recommendation of Lluis Belanche, we work on the base 10 scale!
#' Note that we use a simulted annealing idea: the step size of the grid decreases with increasing number of iterations (step).
#' 
#' @param value.optim The current optimum value for \epsilon.
#' @param step The optimization step.
#'
#' @return A numeric vector with three elements which can be used as updated optimization grid.
gridEpsilon<-function(value.optim, step){
  value.optim<-log10(value.optim)
  minV <- max(value.optim - (1 / step**2) * value.optim, 0.001);
  maxV <- value.optim + (1 / step**2) * value.optim;
  return(10**c(minV, value.optim, maxV))
}

#' @title Calculates a new grid for parameter optimization
#'
#' @description Switches between different parameter types and calls respective specialised function for grid update.
#' 
#' @value.optim Current estimate for the optimal value
#' @step Current iteration step.
#' @type Select the appropriate parameter from: "C","epsilon","poly".
updateGrid <- function(value.optim, step, type) {
  stopifnot(value.optim>0.00001)
  switch(type,
    poly =  gridPoly(value.optim),
    C = gridC(value.optim, step),
    epsilon = gridEpsilon(value.optim, step)
  )
}

#' @title Calculates n-times k-fold-cross-validation for a kernelized relevance vector machine.
#' 
#' @param response.name Name of the response (output)
#' @data The data set to analyse
#' @p Degree of the polynomial kernel
#' @n Number of replicates, default is 10
#' @k Number of folds in k-fold cross validation, default is 10
#' @return 
#' #' @return A numeric vector with averages over all n replicates:
#' \itemize{
#'  \item{"index 1"}{The mean coefficient of determination for validation data}
#'  \item{"index 2"}{The standard deviation of the coefficient of determination for validation data}
#'  \item{"index 3"}{The mean of the sparsity index, which is 1 - ratio of data used as support vectors}
#'  \item{"index 4"}{The standard deviation of the sparsity index}
#' }
krvm.10x10CV<-function(response.name, data, p, n=10,k=10){
  
  #check preconditions
  stopifnot(exists("response.name"))
  stopifnot(is.character(response.name))
  stopifnot(exists("data"))
  #at least 10 rows
  stopifnot(nrow(data)>9)
  #at least 2 columns 
  stopifnot(ncol(data)>1)
  stopifnot(k<=nrow(data))
  
  r<-replicate(n, krvm.CV(response.name, data, p, k))
  r<-apply(r,1,mean)
  names(r)<-c("r2.mean","r2.sd","sparsity.mean","sparsity.sd")
  return(r)
}

#' @title Calculates n-times k-fold-cross-validation for a kernelized support vector machine.
#' 
#' @param response.name Name of the response (output)
#' @data The data set to analyse
#' @c The C parameter
#' @eps The epsilon parameter
#' @p Degree of the polynomial kernel
#' @n Number of replicates, default is 10
#' @k Number of folds in k-fold cross validation, default is 10
#' @return 
#' #' @return A numeric vector with averages over all n replicates:
#' \itemize{
#'  \item{"index 1"}{The mean coefficient of determination for validation data}
#'  \item{"index 2"}{The standard deviation of the coefficient of determination for validation data}
#'  \item{"index 3"}{The mean of the sparsity index, which is 1 - ratio of data used as support vectors}
#'  \item{"index 4"}{The standard deviation of the sparsity index}
#' }
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

#' Optimize the polynomial degree for the relevance vector machine.
#'
#' @param result.optim A result objects containing previous results that should be improved if possible
#' @param grid The optimization grid with values for polynomial degree 
#' @param data The data set to analyse
#' @param numCVReplicates The number of replicates to use in k-fold cross-validation
#'
#' @return A list containing three elements:
#' \itemize{
#'  \item{"result"}{The best result obtained. For details of this object compare \link{krvm.10x10CV}.}
#'  \item{"parameter"}{The optimal parameter setting, i.e. the optimal polynomial degree.}
#'  \item{"time}{Computation time for this function call.}
#' }

optim.parameter.rvm<-function(result.optim, grid, data, numCVReplicates){
  ptm <- proc.time();
  #check preconditions
  #Here I do not check result.optim because it is NULL in the first call!
  check.numeric.values(grid,3)
  check.data.frames(data, min_rows=10, min_columns=2)
  check.numeric.values(numCVReplicates,1)
  param.optim<-"NA"
  print(grid)
  for (param in grid) {
    result <- krvm.10x10CV(response.name = "output", data, p = round(param),n = numCVReplicates);
    if(exists("result")){
      if (isInvalidResult(result, "poly", param)) {
        print("Skip this run because there is no valid result!");
      }else if(length(result.optim)==0){#this is the case for the first model run
        result.optim <- result;
        param<-round(param);
        param.optim <- param;
        print(paste("First run: RVM optim poly degree:", param.optim));
      }else if (result[1] > result.optim[1]){
        result.optim <- result;
        param<-round(param);
        param.optim <- param;
        print(paste("optim poly degree:", param.optim));
      }
    }else{
      stop("Missing result!")
    }
  }
  time.used <- proc.time() - ptm
  print(time.used[3])
  return(list(result=result.optim, parameter=param.optim, time=time.used[3]))
}
  
#' Optimize the C,\epsilon, and the polynomial degree for the support vector machine.
#'
#' @description Switches between three versions of calls to \link{ksvm.10x10CV}
#' depending on param_name. Proposes a new optimization grid for the respective parameter.    
#' 
#' @param result.optim A result objects containing previous results that should be improved if possible
#' @param param_optim The optimal parameter value in previous analyses or a start value.
#' @param grid The optimization grid 
#' @param param_name The name of the parameter, must be either "epsilon","C", or "poly"
#' @param data The data set to analyse
#' @param c.optim The optimal parameter value for C in previous analyses or a start value.
#' @param epsilon.optim The optimal parameter value for epsilon in previous analyses or a start value.
#' @param polynomial.degree.optim The optimal parameter value for the polynomial degree in previous analyses or a start value.
#' @param numCVReplicates The number of replicates to use in k-fold cross-validation
#'
#' @return A list containing three elements:
#' \itemize{
#'  \item{"result"}{The best result obtained. For details of this object compare \link{ksvm.10x10CV}.}
#'  \item{"parameter"}{The optimal parameter setting, i.e. the optimal polynomial degree.}
#'  \item{"time}{Computation time for this function call.}
#' }
optim.parameter<-function(result.optim, param.optim, grid, param_name, data, c.optim, epsilon.optim, polynomial.degree.optim, numCVReplicates){
  ptm <- proc.time();
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
  time.used <- proc.time() - ptm
  print(time.used[3])
  return(list(new.grid=grid, result=result.optim, parameter=param.optim, time=time.used[3]))
}

