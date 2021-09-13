library(readxl)
library(dplyr)
library(tidyr)
options("install.lock"=FALSE)

set.seed(2)
#..............Recruitment data....................
#CFS
# cfs=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/CFS/Correlation_Feature_Selection_B.xlsx")
# data=cfs
#ACE
# ACE=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/ACE/ACE_B.xlsx")
# data=ACE
# #ACE_ININGE
# ACE_ININGE=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/ACE/ACE_ININGE_B.xlsx")
# data=ACE_ININGE
# #GFS
# gfs=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/GFS/GAFS_Final_B.xlsx")
# data=gfs
#RFE_Radom_Forest
RFE_RF=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/RFE_RF_B.xlsx")
data=RFE_RF
# #RFE_Naive_Bayes
# RFE_NB=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/RFE_NB_B.xlsx")
# data=RFE_NB
RFE_CF=read_excel("C:/Users/fvazq/Google Drive/3er a?o/semestre 2/ININ4996 Special/Feature selection/RFE_CF_Final.xlsx")
data=RFE_CF_Final

library(readxl)
RFE_CF_Final <- read_excel("RFE_CF_Final.xlsx")
data=RFE_CF_Final


#Create SMOTE samples
library(smotefamily)
data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.numeric)
table(data$Y)
resample=SMOTE(X=data[,-ncol(data)],target=as.vector(data$Y),K=3,dup_size=1)
table(resample$data$class)
data=resample$data
Y = c(data$class)
X = data.frame(data[1:ncol(data)-1])
data=cbind(X,Y)
data[,ncol(data)]=as.character(data[,ncol(data)])

# #CFS
# data[,1:(ncol(data)-1)]=round(data[,1:(ncol(data)-1)])
# data[,1:ncol(data)]<- lapply(data[, 1:ncol(data)], as.factor)
# #ACE
# data[,6:ncol(data)-1]=round(data[,6:ncol(data)-1])
# data[,6:ncol(data)]<- lapply(data[, 6:ncol(data)], as.factor)
# data[,1:5]<- lapply(data[,1:5], as.numeric)
# #ACE_ININGE
# data[,6:ncol(data)-1]=round(data[,6:ncol(data)-1])
# data[,6:ncol(data)]<- lapply(data[, 6:ncol(data)], as.factor)
# data[,1:5]<- lapply(data[,1:5], as.numeric)
# #GFS
# data[,4:(ncol(data)-1)]=round(data[,4:(ncol(data)-1)])
# data[,1:3]<- lapply(data[,1:3], as.numeric)
# data[,4:ncol(data)]<- lapply(data[,4:ncol(data)], as.factor)
# #RFE_RF
# data[,3:(ncol(data)-1)]=round(data[,3:(ncol(data)-1)])
# data[,1:2]<- lapply(data[, 1:2], as.numeric)
# data[,3:ncol(data)]<- lapply(data[, 3:ncol(data)], as.factor)
# #RFE_NB
# data[,1:(ncol(data)-1)]=round(data[,1:(ncol(data)-1)])
# data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.factor)

#RFE_CF
data[,6:(ncol(data)-1)]=round(data[,6:(ncol(data)-1)])
data[,1:5]<- lapply(data[, 1:5], as.numeric)
data[,6:ncol(data)]<- lapply(data[, 6:ncol(data)], as.factor)

str(data)
#............................RF..........................
rfResults=NULL
rfResultsComplete=NULL
min=max(round(0.1*(ncol(data)-1),0),1)
max=round(0.5*(ncol(data)-1),0)
min=max(round(0.1*n-1,0),1)
max=round(0.5*(n-1),0)

for (nTrees in seq(25,300,25))
{
  for (mNodes in 2:floor(0.9*ncol(data)))
  {
    for (mTry in min:max)#min:max)
    {
      rF_mod=cvError.RF(X=data.frame(data[1:ncol(data)-1]),Y=as.factor(data$Y),ntree=nTrees,maxnodes=mNodes,mtry=mTry)
      rfResults=rbind(rfResults, c(nTrees,mNodes,mTry,rF_mod$mean,rF_mod$sd))
      rfResultsComplete=rbind(rfResultsComplete, data.frame(nTrees,mNodes,mTry,rF_mod$Performance.Measures[,1:4]))
    }
  }
}
rf=na.omit(rfResults)
rfResultsComplete=na.omit(rfResultsComplete)
rfResultsComplete=data.frame(rfResultsComplete,transmute(rfResultsComplete,nDesirability=Desirability/max(Desirability)))
rfResultsComplete=data.frame(rfResultsComplete %>% group_by(nTrees,mNodes,mTry) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(rfResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(rfResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
rfResultsComplete=filter(rfResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)
print(rfResultsComplete)

#CFS
write.table(rfResultsComplete,"CFS Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")
#GFS
write.table(rfResultsComplete,"GFS Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE
write.table(rfResultsComplete,"ACE Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE_ININGE
write.table(rfResultsComplete,"ACE_ININGE Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_RF
write.table(rfResultsComplete,"RFE_RF Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_NB
write.table(rfResultsComplete,"RFE_NB Binary Random Forest Results [Recruitment].csv",row.names=FALSE,sep=",")

#..............................RPART DT............................
rpartResults=NULL
rpartResultsComplete=NULL
for (minSplit in 5:10)
{
  for (minBucket in 3:7)
  {
    for (Cp in c(0,0.001,0.01))
    {
      for (maxDepth in c(6,8,10))
      {
        rPART_mod=cvError.RPART(X=data.frame(data[1:ncol(data)-1]),Y=as.factor(data$Y),minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth)
        rpartResults=rbind(rpartResults, c(minSplit,minBucket,Cp,maxDepth,rPART_mod$mean,rPART_mod$sd))
        rpartResultsComplete=rbind(rpartResultsComplete, data.frame(minSplit,minBucket,Cp,maxDepth,rPART_mod$Performance.Measures[1:4,1:4]))
      }
    }
  }
}
rpartResultsComplete=na.omit(rpartResultsComplete)
rpartResultsComplete=data.frame(rpartResultsComplete %>% group_by(minSplit, minBucket, Cp, maxDepth) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(rpartResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(rpartResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
rpartResultsComplete=filter(rpartResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)
print(rpartResultsComplete)

#CFS
write.table(rpartResultsComplete,"CFS Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")
#GFS
write.table(rpartResultsComplete,"GFS Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE
write.table(rpartResultsComplete,"ACE Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE_ININGE
write.table(rpartResultsComplete,"ACE_ININGE Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_RF
write.table(rpartResultsComplete,"RFE_RF Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_NB
write.table(rpartResultsComplete,"RFE_NB Binary Rpart DT Results [Recruitment].csv",row.names=FALSE,sep=",")

#..................................LDA................................................
LDAResults=NULL
LDAResultsComplete=NULL
for (mLDA in c("moment","mle","t"))
{
  LDA_mod=cvError.LDA(X=data.frame(data[1:ncol(data)-1]),Y=as.factor(data$Y),mLDA=mLDA)
  LDAResults=rbind(LDAResults, c(mLDA,LDA_mod$mean,LDA_mod$sd))
  LDAResultsComplete=rbind(LDAResultsComplete, data.frame(mLDA,LDA_mod$Performance.Measures[,1:4]))
}
LDAResultsComplete=data.frame(LDAResultsComplete,transmute(LDAResultsComplete,nDesirability=Desirability/max(Desirability)))
LDAResultsComplete=data.frame(LDAResultsComplete %>% group_by(mLDA) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(LDAResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(LDAResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
LDAResultsComplete=filter(LDAResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)
LDAResultsComplete

#CFS
write.table(LDAResultsComplete,"CFS Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")
#GFS
write.table(LDAResultsComplete,"GFS Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE
write.table(LDAResultsComplete,"ACE Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE_ININGE
write.table(LDAResultsComplete,"ACE_ININGE Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_RF
write.table(LDAResultsComplete,"RFE_RF Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_NB
write.table(LDAResultsComplete,"RFE_NB Binary LDA Results [Recruitment].csv",row.names=FALSE,sep=",")

#..................................SVM................................................

SVMResults=NULL
SVMResultsComplete=NULL
for (Type in c("C-classification","nu-classification"))
{
  for (Kernel in c("linear","polynomial","radial","sigmoid")){
    SVM_mod=cvError.SVM(X=data.frame(data[1:ncol(data)-1]),Y=as.factor(data$Y),type=Type,kernel=Kernel)
    SVMResults=rbind(SVMResults, c(Type,Kernel,SVM_mod$mean,SVM_mod$sd))
    SVMResultsComplete=rbind(SVMResultsComplete, data.frame(Type,Kernel,SVM_mod$Performance.Measures[,1:4]))
  }
}
proSVMResultsComplete=data.frame(SVMResultsComplete,transmute(SVMResultsComplete,nDesirability=Desirability/max(Desirability)))
SVMResultsComplete=data.frame(SVMResultsComplete %>% group_by(Type,Kernel) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(SVMResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(SVMResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
SVMResultsComplete=filter(SVMResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)
SVMResultsComplete

#CFS Binary
#write.table(SVMResultsComplete,"CFS Binary SVM Results [Recruitment].csv",row.names=FALSE,sep=",")
#GFS Binary
#write.table(SVMResultsComplete,"GFS Binary SVM Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE Binary
#write.table(SVMResultsComplete,"ACE Binary SVM Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE_ININGE Binary
#write.table(SVMResultsComplete,"ACE_ININGE Binary SVM Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_RF Binary
#write.table(SVMResultsComplete,"RFE_RF Binary SVM  Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_NB Binary
#write.table(SVMResultsComplete,"RFE_NB Binary SVM Results [Recruitment].csv",row.names=FALSE,sep=",")
#.............................Logistic................................................
LogResults=NULL
LogResultsComplete=NULL
for (mLink in c("logit","probit","cauchit"))
{
  for (threshold in seq(0.4,0.9,0.05))
  {
    Log_mod=cvError.Log(X=data.frame(data[1:ncol(data)-1]),Y=as.factor(data$Y),mLink=mLink,threshold=threshold)
    LogResults=rbind(LogResults, c(mLink,threshold,Log_mod$mean,Log_mod$sd))
    LogResultsComplete=rbind(LogResultsComplete, data.frame(mLink,threshold,Log_mod$Performance.Measures[,1:4]))
  }
}
LogResultsComplete=data.frame(LogResultsComplete,transmute(LogResultsComplete,nDesirability=Desirability/max(Desirability)))
LogResultsComplete=data.frame(LogResultsComplete %>% group_by(mLink, threshold) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(LogResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(LogResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
LogResultsComplete=filter(LogResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)

#CFS Binomial
write.table(LogResultsComplete,"CFS Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
#GFS Binomial
write.table(LogResultsComplete,"GFS Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE Binomial
write.table(LogResultsComplete,"ACE Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
#ACE_ININGE Binomial
write.table(LogResultsComplete,"ACE_ININGE Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_RF Binomial
write.table(LogResultsComplete,"RFE_RF Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
#RFE_NB Binomial
write.table(LogResultsComplete,"RFE_NB Binary LR Results [Recruitment].csv",row.names=FALSE,sep=",")
