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

#this region is for static loading
setwd(dataDir)
d.svm<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_16_aggregated.csv",sep=";")
str(d.svm)
d.rvm<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_15_aggregated.csv",sep=";")
str(d.rvm)

dim(d.svm)
#[1] 300  23
dim(d.rvm)
#[1] 300  19

#auxiliary variables to set the names for each set
d2.svm<-d.svm
names(d2.svm)<-paste("svm",names(d.svm),sep="_")
d2.rvm<-d.rvm
names(d2.rvm)<-paste("rvm",names(d.rvm),sep="_")

#we work with one horizontal grouped dataset, and other vertical grouped dataset for easyness
d.horizontal<-cbind(d2.svm,d2.rvm)
d.vertical<-rbind(subset(d.svm,select = names(d.rvm)),d.rvm)
#add a variable which indicates the method 
d.vertical$method <- c(rep("svm",nrow(d.svm)),rep("rvm",nrow(d.rvm)))

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

require(lattice)
setwd(plotDir)
jpeg("Error_Estimating_Polynomial_Degree_boxplot.jpeg")
histogram(~polynomial.degree-polynomial_degree_setting|method,
          xlab="Empirical minus estimated polynomial degree",
          ylab="Frequency",
          layout=c(1,2),
          data=d.vertical)
dev.off()

setwd(plotDir)
jpeg("Error_Estimating_Polynomial_Degree.jpeg")
densityplot(~polynomial.degree-polynomial_degree_setting,
            groups=method,
          xlab="Empirical minus estimated polynomial degree",
          ylab="Frequency",
          auto.key=TRUE,
          data=d.vertical)
dev.off()
densityplot(~polynomial.degree-polynomial_degree_setting, groups=method, data=d.vertical)

#comparison between performance of svm and rvm
jpeg("Compare_RVM_and_RVM_STNR.jpeg")
xyplot(rvm_cv.mean - svm_cv.mean, rvm_signal.to.noise.ratio,
          xlab="Empirical minus estimated polynomial degree",
          ylab="Frequency",
          data=d.horizontal)
dev.off()



#Obtain the grid of plots for each combination of SVM and RVM and saves it to the plot directory
row.attributes<-c("polynomial.degree - polynomial_degree_setting","sparsity","cv.mean.corrected","compu.time")
column.attributes<-c("signal.to.noise.ratio","num.observations/num.vars","polynomial.degree")
populate.table.svm.rvm("d.vertical",row.attributes,column.attributes,"method",TRUE,"gam")

labels.columns <- c("signal/noise","cases/inputs","degree")
labels.rows <- c("delta degree","sparsity","lambda","time[s]")

#verifying data doing a regression for svm and rvm
error.poly.svm <-d.svm$polynomial.degree - d.svm$polynomial_degree_setting
error.poly.rvm <-d.rvm$polynomial.degree - d.rvm$polynomial_degree_setting
plot(d.svm$signal.to.noise.ratio,error.poly.svm,ylab = "Error in estimated poly",xlab="Signal to noise ratio") 
abline(gam(error.poly.svm ~ d.svm$signal.to.noise.ratio))
plot(d.rvm$signal.to.noise.ratio,error.poly.rvm,ylab = "Error in estimated poly",xlab="Signal to noise ratio")
abline(gam(error.poly.rvm ~ d.rvm$signal.to.noise.ratio))

ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=error.poly,colour=method)) + geom_point() + stat_smooth(method=lm)
with(d.vertical, cor(cv.mean, cv.mean))
