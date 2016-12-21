###############################################################################################
# Kernel-Based Learning & Multivariate Modeling DMKM Master - MIRI Master
# Lecturer: Lluıs A. Belanche, belanche@cs.upc.edu
# Term project
# October-December 2016  
#
# In this script we analyze aggregated results from our experiments 
# and create the figures, tables, and summary statistics included in the final report.
# The input data files 
# "Results_Simulation_SVM_KMLMM_term_project_2016_12_16_aggregated.csv"
# and "Results_Simulation_RVM_KMLMM_term_project_2016_12_16_aggregated.csv"
# were created in the script files "AnalyseFirstResultsOfSVM.R"
# and "AnalyseFirstResultsOfSVM.R".
#############################################################################################

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

#################################################################
#Model the effect of data attributes on model performance:
#################################################################

#lambda
m1.lm<-lm(log(cv.mean.corrected+0.0000001)~
            signal.to.noise.ratio+polynomial.degree+num.observations+num.vars+(num.observations/num.vars)+method,
          data=d.vertical)
plot(d.vertical$cv.mean.corrected, m1.lm$residuals)
summary(m1.lm)
# Call:
#   lm(formula = log(cv.mean.corrected + 1e-07) ~ signal.to.noise.ratio + 
#        polynomial.degree + num.observations + num.observations/num.vars, 
#      data = d.vertical)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -11.563  -1.436   1.017   2.876  16.280 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               -9.536e-01  8.821e-01  -1.081    0.280    
# signal.to.noise.ratio      1.261e-01  6.308e-01   0.200    0.842    
# polynomial.degree         -1.530e+00  1.454e-01 -10.521   <2e-16 ***
#   num.observations           4.202e-02  2.832e-03  14.838   <2e-16 ***
#   num.observations:num.vars -3.947e-04  2.719e-05 -14.514   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.355 on 595 degrees of freedom
# Multiple R-squared:  0.4085,	Adjusted R-squared:  0.4045 
# F-statistic: 102.7 on 4 and 595 DF,  p-value: < 2.2e-16

#sparsity
m1.lm<-lm(log(sparsity+0.0000001)~
            signal.to.noise.ratio+polynomial.degree+num.observations+num.vars+(num.observations/num.vars)+method,
          data=d.vertical)
plot(d.vertical$sparsity, m1.lm$residuals)
plot(d.vertical$sparsity[d.vertical$sparsity>0], m1.lm$residuals[d.vertical$sparsity>0])
summary(m1.lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               -2.056e+00  3.948e-01  -5.209 2.63e-07 ***
#   signal.to.noise.ratio     -7.551e-01  2.823e-01  -2.675  0.00769 ** 
#   polynomial.degree          1.335e-01  6.507e-02   2.051  0.04067 *  
#   num.observations           5.620e-03  1.267e-03   4.434 1.10e-05 ***
#   num.observations:num.vars -2.250e-05  1.217e-05  -1.849  0.06500 .  

#sparsity
m1.lm<-lm(polynomial.degree-polynomial_degree_setting~
            signal.to.noise.ratio+polynomial.degree+num.observations+num.vars+(num.observations/num.vars)+method,
          data=d.vertical)
plot(d.vertical$polynomial.degree-d.vertical$polynomial_degree_setting, m1.lm$residuals)
summary(m1.lm)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               -1.754e+00  3.323e-01  -5.277 1.84e-07 ***
#   signal.to.noise.ratio      3.461e-01  2.250e-01   1.539 0.124418    
# polynomial.degree          3.672e-01  5.179e-02   7.090 3.82e-12 ***
#   num.observations           7.119e-03  1.097e-03   6.492 1.79e-10 ***
#   num.vars                  -2.253e-02  5.956e-03  -3.783 0.000171 ***
#   methodsvm                  3.689e-01  1.266e-01   2.913 0.003718 ** 
#   num.observations:num.vars  5.973e-05  3.205e-05   1.864 0.062844 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.551 on 593 degrees of freedom
# Multiple R-squared:  0.2133,	Adjusted R-squared:  0.2053 
# F-statistic:  26.8 on 6 and 593 DF,  p-value: < 2.2e-16

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

#inter-batteries analysis
library(MASS)
Xs<-scale(cbind(d.vertical[,16:19],d.vertical$num.observations/d.vertical$num.vars))
Ys<-scale(d.vertical[,c(4,14,10)])

#number of predictors
p<-dim(Xs)[2]
#number of responses
q<-dim(Ys)[2]
#calculate the covariance matrix between predictors xi and responses yi
Vxy<-var(x=Xs, y=Ys)

#calculate the covariance matrix between responses yi and predictors xi
Vyx<-var(x=Ys, y=Xs)

#singular value decomposition of Vxy:
Vxy.svd <- svd(Vxy)

#the singular values
Vxy.svd$d
#[1] 1.0010226 0.7983576 0.1333812
#[1] 0.94301132 0.49331940 0.22690072 0.02610421

#Here u represents A and v represents B (notation in the slights).
A<-Vxy.svd$u
B<-Vxy.svd$v
#The columns of A and B represent the a_h and b_h respectively.

#Compute the t_h and the u_h:
T<-Xs %*% A
dim(T)
#[1] 600 4

U<-Ys %*% B
dim(U)
#[1] 600  4

#Decide how many components you retain for prediction.

#calculate the redundancies
a<-dim(T)[2]
dim1<-q
dim2<-a
red.yt<-array(data=rep(-1,dim1*dim2),dim=c(dim1,dim2))
for (i in 1:q) {for ( j in 1:a) {red.yt[i,j] <- summary(lm(Ys[,i]~T[,1:j]))$r.squared}}
rownames(red.yt) <- names(d.vertical)[c(4,14,10)]
print(red.yt)
# [,1]      [,2]      [,3]
# polynomial_degree_setting 0.1906802396 0.2587839 0.2771105
# cv.mean.corrected         0.6494184689 0.6586568 0.6682885
# sparsity                  0.0007090972 0.4486494 0.4495045

#Conclusions:
#With the first two components / axes we can already explain all four response variables!
#The first axis lambda, the second explains sparsity!

# names(d.vertical)[16:19]
# [1] "signal.to.noise.ratio" "num.vars"             
# [3] "num.observations"      "polynomial.degree"

cor(T[,1],Xs[,1])
cor(T[,1],Xs[,2])
cor(T[,1],Xs[,3])
cor(T[,1],Xs[,4])
cor(T[,1],Xs[,5])
#The first axis is mainly explained by number of variables(+)
#and the polynomial degree (+) and cases/ inputs(-).

cor(T[,2],Xs[,1])
cor(T[,2],Xs[,2])
cor(T[,2],Xs[,3])
cor(T[,2],Xs[,4])
cor(T[,2],Xs[,5])
#The second axis is mainly explained by
#number of variables (-) 
#polynomial degree (+)
#and cases/ inputs (+).

#Conclusion:

plot(T[,1],d.vertical$sparsity, pch="+", col=ifelse(d.vertical$method=="svm","red","blue"))
plot(T[,2],d.vertical$sparsity, pch="+", col=ifelse(d.vertical$method=="svm","red","blue"))

plot(T[,1],d.vertical$cv.mean.corrected, pch="+", col=ifelse(d.vertical$method=="svm","red","blue"))
plot(T[,2],d.vertical$cv.mean.corrected, pch="+")
setwd(plotDir)
jpeg("Inter_batteries_lambda.jpeg")
xyplot(d.vertical$cv.mean.corrected ~ T[,1], 
       groups = d.vertical$method,
       xlab="Inter Batteries Component 1",
       ylab=expression(lambda),
       auto.key=TRUE,
       cex.axis=10.0,
       cex.lab=10.0)
dev.off()

xyplot(T[,2] ~ T[,1], 
       groups = d.vertical$method,
       xlab="Inter Batteries component 1",
       ylab=expression(lambda),
       auto.key=TRUE)

setwd(plotDir)
jpeg("Inter_batteries_sparsity.jpeg")
xyplot(d.vertical$sparsity ~ T[,2], 
       groups = d.vertical$method,
       xlab="Inter Batteries Component 2",
       ylab="Sparsity",
       auto.key=TRUE,
       cex.axis=10.0,
       cex.lab=10.0)
dev.off()

require(mgcv)
m1.gam <- gam(cv.mean.corrected ~ s(num.observations), data=d.vertical)
m2.gam <- gam(cv.mean.corrected ~ s(cv), data=d.vertical)
m3.gam <- gam(cv.mean.corrected ~ s(num.vars), data=d.vertical)
m4.gam <- gam(cv.mean.corrected ~ polynomial.degree, data=d.vertical)
m5.gam <- gam(cv.mean.corrected ~ s(num.observations, cv), data=d.vertical)
anova(m1.gam,m2.gam,m3.gam,m4.gam, m5.gam)
# Resid. Df Resid. Dev      Df Deviance
# 1    596.89     30.626                 
# 2    596.48     21.636  0.4108   8.9898
# 3    595.22     26.133  1.2585  -4.4966
# 4    598.00     19.320 -2.7784   6.8133
# 5    571.28     18.643 26.7195   0.6772
plot(m6.gam)

m6.gam <- gam(cv.mean.corrected ~ s(num.vars)+s(num.observations)+s(num.observations, cv), data=d.vertical)
anova(m5.gam, m6.gam)
# Resid. Df Resid. Dev   Df Deviance
# 1    571.28     18.643              
# 2    562.49     17.815 8.79  0.82759


setwd(plotDir)
jpeg("Interaction_num_observations_cases_to_variables_ratio_interactions.jpeg")
plot(m4.gam, cex=6)
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
