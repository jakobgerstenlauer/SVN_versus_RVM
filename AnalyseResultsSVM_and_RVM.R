#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd("D:/Documents/MIRI/Semestre 2/ProyectoFinal/KernelFinalProject/code")
#setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")
setwd(dataDir)
d<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_09.csv",sep=";") #<-set to static
str(d)

with(d,plot(signal.to.noise.ratio, cv.mean)) #P.P. the plot is already here?
#not very informative, (P.P. everyone) nearly always 1!
with(d,hist(cv.mean))
#not very informative, nearly always 1!

d_low_model_quality<-subset(d,cv.mean<0.9)

with(d, hist(num.vars))
with(d_low_model_quality, hist(num.vars))

with(d_low_model_quality, table(print(num.observations/num.vars)) )
# 10.1666666666667               11               13 14.7857142857143 
# 3                3                3                3 
# 19.5714285714286               22             24.6 27.7142857142857 
# 3                3                3                3

with(d, hist(num.observations/num.vars) )
#the mean of the distribution of the ratio of observations/ variables is too high!

names(d)
with(d,table(polynomial_degree_setting-polynomial.degree))
# -3  -2  -1   0 
# 51  98 100  51 

#Conclusion 1:
#The svm model fits often fits a too low polynomial degree for the kernel!

setwd(dataDir)
d<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv",sep=";")
dim(d)
#[1] 300  11

names(d)
with(d,table(polynomial_degree_setting-polynomial.degree))
#-3  -2  -1   0 
#51  99 101  49

#Conclusion 1:
#The rvm and the svm model do not differ in the choice in the polynomial degree.

with(d,plot(signal.to.noise.ratio, cv.mean))
