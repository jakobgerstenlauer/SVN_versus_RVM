#Test results for rvm
#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd("D:/Documents/MIRI/Semestre 2/ProyectoFinal/KernelFinalProject/code")
setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")
setwd(dataDir)
d<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv",sep=";") #<-set to static
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
