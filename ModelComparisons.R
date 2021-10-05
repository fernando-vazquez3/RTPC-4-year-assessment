
library(tidyverse)
library(smotefamily)
library(devtools)

("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/ModelComparisons.R")


#Version de prueba para el grupo
ACE =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/ACE.csv"))
CFS =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/CFS.csv"))
GAFS =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/GAFS.csv"))
RFE_RF =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_RF.csv"))
RFE_NB =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_NB.csv"))
RFE_CF =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_CF.csv"))

#Folds

F1= c(64, 85, 91, 72, 83, 98, 116, 67, 87, 73, 76, 57, 6, 39, 50, 22, 3, 42, 55, 32, 49, 26, 44)
F2= c(100, 82, 113, 90, 61, 79, 105, 86, 77, 96, 102, 56, 35, 5, 30, 38, 19, 59, 13, 41, 8, 58, 45)
F3= c(75, 97, 70, 69, 89, 84, 94, 115, 93, 106, 99, 16, 51, 18, 37, 40, 9, 47, 23, 17, 27, 48, 52)
F4= c(92, 80, 109, 112, 62, 111, 103, 88, 81, 108, 104, 7, 46, 11, 28, 10, 31, 43, 21, 14, 54, 24, 4)
F5= c(1, 2, 12, 15, 20, 25, 29, 33, 34, 36, 53, 60, 63, 65, 66, 68, 71, 74, 78, 95, 101, 107, 110, 114)

Folds=list(F1,F2,F3,F4,F5)
names(Folds)=c("F1","F2","F3","F4","F5")
numeric=c(5,0,3,0,0,5)
names(numeric)=c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")

Datasets=list(ACE,CFS,GAFS,RFE_RF,RFE_NB,RFE_CF)
names(Datasets)=c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")


glimpse(data)

nFolds=5
#set.seed=()

# #Assing rows per class
# permClass=list()
# obsC=list()
# p=table(data$Y)/nrow(data)
# for (i in 1:length(levels(data$Y)))
# {
#   selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
#   permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
#   assign(paste("permClass",i,sep=""),permRows)
#   permClass[[i]]=permRows
#   assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
#   obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
# }
# 
# #Create folds for training and testing
# rowsFold=list()
# for(i in 1:(nFolds-1)){
#   rows=list()
#   for (j in 1:length(levels(data$Y))){
#     if ((i-1)==0){rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]}
#     else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
#   }
#   name=paste("F",i,sep="")
#   rowsFold[[name]]=unlist(rows)
# }
# remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
# rowsFold[[paste("F",nFolds,sep="")]]=remaining

metrics=c("Accuracy", "Kappa", "F1","Sensitivity","Specificity", "Pos Pred Value", "Neg Pred Value","Detection Rate", "Desirability")

#............................RF..........................
rfResults=NULL
rfResultsComplete=NULL
min=max(round(0.1*(ncol(data)-1),0),1)
max=round(0.5*(ncol(data)-1),0)


for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data=Datasets[[Data]]
  data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.numeric)
  table(data$Y)
  
  #SMOTe resampling data
  resample=SMOTE(X=data[,-ncol(data)],target=as.vector(data$Y),K=3,dup_size=1)
  table(resample$data$class)
  
  #Now we rebuild the data set which is ready
  data=resample$data
  Y = c(data$class)
  X = data.frame(data[1:ncol(data)-1])
  data=cbind(X,Y)
  data[,ncol(data)]=as.character(data[,ncol(data)])
  
  if(numeric[Data]>0){
    data[,1:numeric[Data]]<- lapply(data[, 1:numeric[Data]], as.numeric)
    data[,(numeric[Data]+1):(ncol(data)-1)]=round(data[,(numeric[Data]+1):(ncol(data)-1)],4)
    
    data[,(numeric[Data]+1):ncol(data)]<- lapply(data[, (numeric[Data]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
  
    data[,(numeric[Data]+1):ncol(data)]<- lapply(data[, (numeric[Data]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  
  
for (nTrees in seq(25,300,25))
{
  for (mNodes in 2:floor(0.9*ncol(data)))
  {
    for (mTry in min:max)#min:max)
    {
      rF_mod=cvError.RF(ntree=nTrees,maxnodes=mNodes,mtry=mTry)
      rfResults=rbind(rfResults, data.frame(nTrees,mNodes,mTry,rF_mod$Mean,rF_mod$SE))
      rfResultsComplete=rbind(rfResultsComplete,data.frame(nTrees,mNodes,mTry,t(rF_mod$meanPM),rF_mod$SE))
    }
  }
}
  }

rf=na.omit(rfResultsComplete)
rf=data.frame(rf,transmute(rf,nDesirability=Desirability/max(Desirability)))
rftest=data.frame(rf %>% group_by(Data,nTrees,mNodes,mTry)) %>% drop_na()
maxDesirability=filter(rftest,Desirability==max(Desirability))$Desirability
SE.maxDesirability=filter(rf,rF_mod.SE==max(rF_mod.SE))$rF_mod.SE
rf=filter(rf,Desirability>=maxDesirability-SE.maxDesirability)
print(rf)


#............................R Part..........................
rpartResults=NULL
rpartResultsComplete=NULL

for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data=Datasets[[Data]]
  data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.numeric)
  table(data$Y)
  
  #SMOTe resampling data
  resample=SMOTE(X=data[,-ncol(data)],target=as.vector(data$Y),K=3,dup_size=1)
  table(resample$data$class)
  
  #Now we rebuild the data set which is ready
  data=resample$data
  Y = c(data$class)
  X = data.frame(data[1:ncol(data)-1])
  data=cbind(X,Y)
  data[,ncol(data)]=as.character(data[,ncol(data)])
  
  if(numeric[i]>0){
    data[,1:numeric[i]]<- lapply(data[, 1:numeric[i]], as.numeric)
    data[,(numeric[i]+1):(ncol(data)-1)]=round(data[,(numeric[i]+1):(ncol(data)-1)],4)
    
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
  else{
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
for (minSplit in 5:10)
{
  for (minBucket in 3:7)
  {
    for (Cp in c(0,0.001,0.01))
    {
      for (maxDepth in c(6,8,10))
      {
        rPART_mod=cvError.RPART(minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth)
        rpartResults=rbind(rpartResults, data.frame(Data,minSplit,minBucket,Cp,maxDepth,rPART_mod$Mean,rPART_mod$SE))
        rpartResultsComplete=rbind(rpartResultsComplete,data.frame(Data,minSplit,minBucket,Cp,maxDepth,t(rPART_mod$meanPM),rPART_mod$SE))
        
      }
    }
  }
}
}
rpart=na.omit(rpartResultsComplete)
rpart=data.frame(rpart,transmute(rpart,nDesirability=Desirability/max(Desirability)))
rparttest=data.frame(rpart %>% group_by(Data,minSplit,minBucket,Cp,maxDepth)) %>% drop_na()
maxDesirability=filter(rparttest,Desirability==max(Desirability))$Desirability
SE.maxDesirability=filter(rpart,rPART_mod.SE ==max(rPART_mod.SE))$rPART_mod.SE
rpart=filter(rpart,Desirability>=maxDesirability-SE.maxDesirability)
print(rpart)


#............................  LDA  ..........................
LDAResults=NULL
LDAResultsComplete=NULL
for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data=Datasets[[Data]]
  data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.numeric)
  table(data$Y)
  
  #SMOTe resampling data
  resample=SMOTE(X=data[,-ncol(data)],target=as.vector(data$Y),K=3,dup_size=1)
  table(resample$data$class)
  
  #Now we rebuild the data set which is ready
  data=resample$data
  Y = c(data$class)
  X = data.frame(data[1:ncol(data)-1])
  data=cbind(X,Y)
  data[,ncol(data)]=as.character(data[,ncol(data)])
  
  if(numeric[i]>0){
    data[,1:numeric[i]]<- lapply(data[, 1:numeric[i]], as.numeric)
    data[,(numeric[i]+1):(ncol(data)-1)]=round(data[,(numeric[i]+1):(ncol(data)-1)],4)
    
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
  else{
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
for (mLDA in c("moment","mle","t"))
{
  LDA_mod=cvError.LDA(mLDA=mLDA)
  LDAResults=rbind(LDAResults, data.frame(Data,mLDA,LDA_mod$Mean,LDA_mod$SE))
  LDAResultsComplete=rbind(LDAResultsComplete, data.frame(Data,mLDA,t(LDA_mod$meanPM),LDA_mod$SE))
}
}
LDA=na.omit(LDAResultsComplete)
LDA=data.frame(LDA,transmute(LDA,nDesirability=Desirability/max(Desirability)))
LDAtest=data.frame(LDA %>% group_by(Data,mLDA)) %>% drop_na()
maxDesirability=filter(LDAtest,Desirability==max(Desirability))$Desirability
SE.maxDesirability=filter(LDA,LDA_mod.SE ==max(LDA_mod.SE))$LDA_mod.SE
LDA=filter(LDA,Desirability>=maxDesirability-SE.maxDesirability)
print(LDA)


# #..................................SVM................................................
SVMResults=NULL
SVMResultsComplete=NULL
for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data=Datasets[[Data]]
  data[,1:ncol(data)]<- lapply(data[,1:ncol(data)], as.numeric)
  table(data$Y)
  
  #SMOTe resampling data
  resample=SMOTE(X=data[,-ncol(data)],target=as.vector(data$Y),K=3,dup_size=1)
  table(resample$data$class)
  
  #Now we rebuild the data set which is ready
  data=resample$data
  Y = c(data$class)
  X = data.frame(data[1:ncol(data)-1])
  data=cbind(X,Y)
  data[,ncol(data)]=as.character(data[,ncol(data)])
  
  if(numeric[i]>0){
    data[,1:numeric[i]]<- lapply(data[, 1:numeric[i]], as.numeric)
    data[,(numeric[i]+1):(ncol(data)-1)]=round(data[,(numeric[i]+1):(ncol(data)-1)],4)
    
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
  else{
    data[,(numeric[i]+1):ncol(data)]<- lapply(data[, (numeric[i]+1):ncol(data)], as.factor)
    ycol=ncol(data)
    names(data[ycol])="Y"
  }
for (Type in c("C-classification","nu-classification")){
  for (Kernel in c("vanilladot","polydot","rbfdot",)){
    if (Kernel =="polydot"){
      for(Degree in 1:2) {
        SVM_mod=cvError.SVM(type=Type,kernel=Kernel,degree=Degree)
        SVMResults=rbind(SVMResults, c(Data,Type,Kernel,SVM_mod$Mean,SVM_mod$SE))
        SVMResultsComplete=rbind(SVMResultsComplete, data.frame(Data,Type,Kernel,SVM_mod$Performance.Measures[,1:4]))
        }
      }
    }
}
}
proSVMResultsComplete=data.frame(SVMResultsComplete,transmute(SVMResultsComplete,nDesirability=Desirability/max(Desirability)))
SVMResultsComplete=data.frame(SVMResultsComplete %>% group_by(Type,Kernel) %>% summarise(Mean.Accuracy=mean(Accuracy),SE.Accuracy=sd(Accuracy)/sqrt(n()), Mean.Kappa = mean(Kappa),SE.Kappa=sd(Kappa)/sqrt(n()),Mean.F1=mean(F1),SE.F1=sd(F1)/sqrt(n()),Mean.Desirability=mean(Desirability),SE.Desirability=sd(Desirability)/sqrt(n()))) %>% drop_na()
maxDesirability=filter(SVMResultsComplete,Mean.Desirability==max(Mean.Desirability))$Mean.Desirability
SE.maxDesirability=filter(SVMResultsComplete,Mean.Desirability==max(Mean.Desirability))$SE.Desirability
SVMResultsComplete=filter(SVMResultsComplete,Mean.Desirability>=maxDesirability-SE.maxDesirability)
SVMResultsComplete

LogResults=NULL
LogResultsComplete=NULL
for (mLink in c("logit","probit","cauchit"))
{
  for (threshold in seq(0.4,0.9,0.05))
  {
    Log_mod=cvError.Log(mLink=mLink,threshold=threshold)
    LogResults=rbind(LogResults, c(mLink,threshold,Log_mod$Mean,Log_mod$SE))
    LogResultsComplete=rbind(LogResultsComplete, data.frame(mLink,threshold,t(Log_mod$meanPM),Log_mod$SE))
  }
}

LogReg=na.omit(LogResultsComplete)
LogReg=data.frame(LogReg,transmute(LogReg,nDesirability=Desirability/max(Desirability)))
LogTest=data.frame(LogReg %>% group_by(mLink,threshold)) %>% drop_na()
maxDesirability=filter(LogTest,Desirability==max(Desirability))$Desirability
SE.maxDesirability=filter(LogReg,Log_mod.SE ==max(Log_mod.SE))$Log_mod.SE
LogReg=filter(LogReg,Desirability>=maxDesirability-SE.maxDesirability)
print(LDA)
