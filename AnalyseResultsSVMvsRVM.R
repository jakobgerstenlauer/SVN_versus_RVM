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

#dynamically load the file:
#opens a dialog box to input the file for svm
# data.file <- file.choose()
# d.svm  <- read.csv(data.file,sep=";")
# str(d.svm)

#hard coded file names 
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
d.flat<-cbind(d2.svm,d2.rvm)
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
with(d.flat, quantile(rvm_cv.mean.corrected - svm_cv.mean.corrected))
# 0%         25%         50%         75%        100% 
# -0.51762249 -0.07967348 -0.03326958  0.00000000  0.22644942

#Conclusion: In general the coeficient of determination is slightly higher for the SVM!

with(d.flat, quantile(rvm_sparsity - svm_sparsity))
# 0%         25%         50%         75%        100% 
# -0.29428571 -0.12585621 -0.04645833  0.01592162  0.32195122

densityplot(~rvm_sparsity - svm_sparsity,
            xlab="Sparsity[%]",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)

densityplot(~rvm_cv.mean.corrected - svm_cv.mean.corrected,
            xlab="Sparsity[%]",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)

#Conclusion: In general the sparsity is slightly higher for the SVM!
setwd(plotDir)
jpeg("Sparsity_SVM_vs_RVM.jpeg")
densityplot(~sparsity,
            groups=method,
            xlab="Sparsity[%]",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.vertical)
dev.off()

jpeg("R2_SVM_vs_RVM.jpeg")
densityplot(~cv.mean.corrected,
            groups=method,
            xlab=expression(lambda),
            ylab="Frequency",
            auto.key=TRUE,
            data=d.vertical)
dev.off()


jpeg("Compare_RVM_and_RVM_STNR.jpeg")
xyplot(rvm_cv.mean.corrected - svm_cv.mean.corrected, rvm_signal.to.noise.ratio,
          xlab="Empirical minus estimated polynomial degree",
          ylab="Frequency",
          data=d.flat)
dev.off()

#plot difference between R2 of svm and rvm 


#Obtain the grid of plots for each combination of SVM and RVM and saves it to the plot directory
row.attributes<-c("polynomial.degree - polynomial_degree_setting","sparsity","cv.mean.corrected","compu.time")
column.attributes<-c("signal.to.noise.ratio","num.observations/num.vars","polynomial.degree")
labels.columns <- c("signal/noise","cases/inputs","degree")
labels.rows <- c("expression(paste(delta,\" degree\"))","\"sparsity\"","expression(lambda)","\"time[s]\"")

populate.table.svm.rvm("d.vertical",row.attributes,column.attributes,"method",TRUE,"gam",labels.rows,labels.columns)


#verifying data doing a regression for svm and rvm
error.poly.svm <-d.svm$polynomial.degree - d.svm$polynomial_degree_setting
error.poly.rvm <-d.rvm$polynomial.degree - d.rvm$polynomial_degree_setting
plot(d.svm$signal.to.noise.ratio,error.poly.svm,ylab = "Error in estimated poly",xlab="Signal to noise ratio") 
abline(gam(error.poly.svm ~ d.svm$signal.to.noise.ratio))
plot(d.rvm$signal.to.noise.ratio,error.poly.rvm,ylab = "Error in estimated poly",xlab="Signal to noise ratio")
abline(gam(error.poly.rvm ~ d.rvm$signal.to.noise.ratio))

ggplot(d.vertical, aes(x=signal.to.noise.ratio, y=error.poly,colour=method)) + geom_point() + stat_smooth(method=lm)
with(d.vertical, cor(cv.mean, cv.mean))
