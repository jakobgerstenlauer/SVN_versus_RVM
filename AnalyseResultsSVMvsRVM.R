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

#this region dinamically loads the file
#opens a dialog box to input the file for svm
# data.file <- file.choose()
# d.svm  <- read.csv(data.file,sep=";")
# str(d.svm)

#opens a dialog box to input the file for rvm
# data.file <- file.choose()
# library(tcltk2)
# tk_choose.files()
# d.rvm  <- read.csv(data.file,sep=";")
# str(d.rvm)

#this region is for static loading
setwd(dataDir)
d.svm<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_16.csv",sep=";")
str(d.svm)
d.rvm<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_15.csv",sep=";")
str(d.rvm)

dim(d.svm)
dim(d.rvm)

#auxiliary variables to set the names for each set
d2.svm<-d.svm
names(d2.svm)<-paste("svm",names(d.svm),sep="_")
d2.rvm<-d.rvm
names(d2.rvm)<-paste("rvm",names(d.rvm),sep="_")

#we work with one horizontal grouped dataset, and other vertical grouped dataset for easyness
d.horizontal<-cbind(d2.svm,d2.rvm)
d.vertical<-rbind(subset(d.svm,select = names(d.rvm)),d.rvm)
#add clasificator for the grouped dataset
d.vertical$method <- c(rep("svm",nrow(d.svm)),rep("rvm",nrow(d.rvm)))
d.vertical$cv.mean.corrected <- ifelse(d.vertical$cv.mean < 0.0,0.0,d.vertical$cv.mean)

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

#Obtain the plots for each combination of SVM and RVM 

row.attributes<-c("polynomial.degree - polynomial_degree_setting","sparcity","cv.mean.corrected","compu.time")
column.attributes<-c("signal.to.noise.ratio","num.observations/num.vars","polynomial.degree")
populate.table.svm.rvm("d.vertical",row.attributes,column.attributes,"method",TRUE,"gam")

library(gam)
library(ggplot2)
#Error in estimated poly ~ signal to noise ratio
error.poly<-d.vertical$polynomial.degree - d.vertical$polynomial_degree_setting
ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=polynomial.degree - polynomial_degree_setting,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Error in estimated poly ~ number of observations/number of variables
rate.no.nv<-d.vertical$num.observations/d.vertical$num.vars
ggplot(d.vertical, aes(x=rate.no.nv, y=error.poly,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Error in estimated poly ~ polynomial degree
ggplot(d.vertical, aes(x=polynomial.degree, y=error.poly,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Sparsity ~ signal to noise ratio
ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=sparcity,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Sparsity ~ number of observations/number of variables
ggplot(d.vertical, aes(x=rate.no.nv, y=sparcity,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Sparsity ~ polynomial degree
ggplot(d.vertical, aes(x=polynomial.degree, y=sparcity,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Computational Burden ~ signal to noise ratio
ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=compu.time,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Computational Burden ~ number of observations/number of variables
ggplot(d.vertical, aes(x=rate.no.nv, y=compu.time,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()

#Computational Burden ~ polynomial degree
ggplot(d.vertical, aes(x=polynomial.degree, y=compu.time,colour=method)) + stat_smooth(method=gam,formula = y ~ x) + geom_point()


error.poly.svm <-d.svm$polynomial.degree - d.svm$polynomial_degree_setting
error.poly.rvm <-d.rvm$polynomial.degree - d.rvm$polynomial_degree_setting
plot(d.svm$signal.to.noise.ratio,error.poly.svm,ylab = "Error in estimated poly",xlab="Signal to noise ratio") 
abline(gam(error.poly.svm ~ d.svm$signal.to.noise.ratio))
plot(d.rvm$signal.to.noise.ratio,error.poly.rvm,ylab = "Error in estimated poly",xlab="Signal to noise ratio")
abline(gam(error.poly.rvm ~ d.rvm$signal.to.noise.ratio))

ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=error.poly,colour=method)) + geom_point() + stat_smooth(method=lm)
with(d.vertical, cor(cv.mean, cv.mean))
