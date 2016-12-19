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

#d.flat: each row contains data from svm and rvm
#d.vertical: data from svm and rvm in separate rows
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

#Now let´s analyse the difference in svm and rvm for each single data set and then average over replicates
setwd(dataDir)
d.svm.atomic<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_16.csv",sep=";")
d.rvm.atomic<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_15.csv",sep=";")


#########################################################################
#Figure 1
#########################################################################
N<-length(d.rvm.atomic$cv.mean)
cv.mean.pairwise <-  pmax(d.rvm.atomic$cv.mean,rep(0,N)) - pmax(d.svm.atomic$cv.mean,rep(0,N))
hist(cv.mean.pairwise)
#aggregate over samples
cv.mean.pairwise.mean<-tapply(cv.mean.pairwise, d.rvm.atomic$id_parameter_combination, mean)

quantile(cv.mean.pairwise.mean)
# 0%         25%         50%         75%        100% 
# -0.51762249 -0.07967348 -0.03326958  0.00000000  0.22644942 

setwd(plotDir)
jpeg("R2_SVM_vs_RVM_pairwise.jpeg")
densityplot(~cv.mean.pairwise.mean,
            xlab="Difference RVM - SVM in coefficient of determination",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)
dev.off()


#########################################################################
#Figure 2
#########################################################################
N<-length(d.rvm.atomic$cv.mean)
sparsity.pairwise <-  pmax(d.rvm.atomic$sparsity,rep(0,N)) - pmax(d.svm.atomic$sparsity,rep(0,N))
hist(sparsity.pairwise)
#aggregate over samples
sparsity.pairwise.mean<-tapply(sparsity.pairwise, d.rvm.atomic$id_parameter_combination, mean)

quantile(sparsity.pairwise.mean)
# 0%         25%         50%         75%        100% 
# -0.29428571 -0.12585621 -0.04645833  0.01592162  0.32195122 

setwd(plotDir)
jpeg("Sparsity_RVM_minus_SVM_pairwise.jpeg")
densityplot(~sparsity.pairwise.mean,
            xlab="Difference RVM - SVM in Sparsity[%]",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)
dev.off()


#########################################################################
#Poly Degree
#########################################################################
N<-length(d.rvm.atomic$cv.mean)
deviation.degree.pairwise <-  abs(d.rvm.atomic$polynomial.degree - d.rvm.atomic$polynomial_degree_setting)-abs(d.svm.atomic$polynomial.degree - d.svm.atomic$polynomial_degree_setting);
hist(deviation.degree.pairwise)
#aggregate over samples
deviation.degree.pairwise.mean<-tapply(deviation.degree.pairwise, d.rvm.atomic$id_parameter_combination, mean)

quantile(deviation.degree.pairwise.mean)
#0%        25%        50%        75%       100% 
#-3.6666667 -0.6666667  0.0000000  0.6666667  4.3333333  

setwd(plotDir)
jpeg("DeviationPolyDegree_RVM_minus_SVM_pairwise.jpeg")
densityplot(~deviation.degree.pairwise.mean,
            xlab="Difference RVM - SVM in absolute deviation from correct degree",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)
dev.off()

#########################################################################
#Figure 3 Computation time
#########################################################################

N<-length(d.rvm.atomic$cv.mean)
compu.time.pairwise <-  d.svm.atomic$compu.time / d.rvm.atomic$compu.time 
hist(compu.time.pairwise)
#aggregate over samples
compu.time.pairwise.mean<-tapply(compu.time.pairwise, d.rvm.atomic$id_parameter_combination, mean)

quantile(compu.time.pairwise.mean, c(0.05,0.1,0.25,0.5,0.75,0.9,0.95))
#5%       10%       25%       50%       75%       90%       95% 
#3.063410  4.036868  5.082727  6.455291 10.721340 19.871889 33.901927 

setwd(plotDir)
jpeg("DeviationPolyDegree_RVM_minus_SVM_pairwise.jpeg")
densityplot(~deviation.degree.pairwise.mean,
            xlab="Difference RVM - SVM in absolute deviation from correct degree",
            ylab="Frequency",
            auto.key=TRUE,
            data=d.flat)
dev.off()



#########################################################################
#Figure 4
#########################################################################

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
