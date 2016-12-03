###############################################################################
#                         Analisys on data generated                          #
###############################################################################

#remove old objects for safety resons
rm(list=ls(all=TRUE))
library(ggplot2)

#utility function
glue<-function(...){paste(...,sep="")}

#prepare path variables
setwd('..')
base.directory<-getwd()
setwd(glue(base.directory,'/code'))
source("workingDir.R")
setwd(dataDir)

#import log from file
data.log.file <- read.table("Log_KMLMM_term_project_2016_11_30.log",header = T)
ggplot(data.log.file,aes(x=data.log.file$opt.step,y=data.log.file$comput.time,fill=data.log.file$parameter))+geom_bar(position = "dodge",stat="identity")+labs(title="Comparison for each parameter in each step",x="Step",y="Computation Time") 

#get the computation time and length for each parameter
tapply(data.log.file$comput.time,data.log.file$parameter,sum)
tapply(data.log.file$comput.time,data.log.file$parameter,length)

#get the computation time and length for each step
tapply(data.log.file$comput.time,data.log.file$opt.step,sum)
tapply(data.log.file$comput.time,data.log.file$opt.step,length)

tapply(data.log.file$comput.time,data.log.file$parameter,ave)
ave(data.log.file$comput.time)
