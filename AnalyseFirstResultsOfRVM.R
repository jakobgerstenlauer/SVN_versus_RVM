#Test results for rvm
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
#setwd(dataDir)
#d<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv",sep=";") #<-set to static
#opens a dialog box to input the file
data.file <- file.choose()
d  <- read.csv(data.file,sep=";")
setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")
str(d)
#there was a mistake in the first run: num.observations and num.vars were mixed up! why?
names(d)[3]<-"num.observations"
names(d)[4]<-"num.vars"

#check the same way as with svm
with(d,plot(signal.to.noise.ratio, cv.mean)) #P.P. the plot is already here?
#not very informative, (P.P. everyone) nearly always 1!<- Again the same results
with(d,hist(cv.mean))
#not very informative, nearly always 1!<- Again the same results

#P.P. copy from tests done in isolated environment#
#install.packages("ggplot2")
library(ggplot2)

#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d$polynomial.degree-d$polynomial_degree_setting","d$sparsity","d$cv.mean","d$compu.time")
column.attributes<-c("d$signal.to.noise.ratio","d$num.observations/d$num.vars","d$polynomial.degree")
result.table.rvm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.rvm) <- c("Signal to noise ratio", "Number of observations/Number of variables", "Polynomial degree")
rownames(result.table.rvm) <- c("Error in estimating poly","Sparcity","CV Mean","Computational Time")
#save to a new file
Date<-gsub(pattern="-", replacement="_",Sys.Date())
fileName<-paste("RVM_results_table_KMLMM_term_project_",Date,".csv",sep="")
result.table.rvm
setwd(dataDir)
write.table(result.table.rvm, file=fileName, append=FALSE, row.names = TRUE, col.names = NA, sep = ";")
