#Load packages
library(tidyverse)
library(tibble)
library(smotefamily)
library(devtools)

#Load Data
ACE =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/ACE.csv"))
CFS =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/CFS.csv"))
GAFS =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/GAFS.csv"))
RFE_RF =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_RF.csv"))
RFE_NB =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_NB.csv"))
RFE_CF =tibble(read.csv("https://raw.githubusercontent.com/fernando-vazquez3/RTPC-4-year-assessment/master/RFE_CF.csv"))

#Set folds and assign rows
nFolds=5
F1= c(64, 85, 91, 72, 83, 98, 116, 67, 87, 73, 76, 57, 6, 39, 50, 22, 3, 42, 55, 32, 49, 26, 44)
F2= c(100, 82, 113, 90, 61, 79, 105, 86, 77, 96, 102, 56, 35, 5, 30, 38, 19, 59, 13, 41, 8, 58, 45)
F3= c(75, 97, 70, 69, 89, 84, 94, 115, 93, 106, 99, 16, 51, 18, 37, 40, 9, 47, 23, 17, 27, 48, 52)
F4= c(92, 80, 109, 112, 62, 111, 103, 88, 81, 108, 104, 7, 46, 11, 28, 10, 31, 43, 21, 14, 54, 24, 4)
F5= c(1, 2, 12, 15, 20, 25, 29, 33, 34, 36, 53, 60, 63, 65, 66, 68, 71, 74, 78, 95, 101, 107, 110, 114)
Folds=list(F1,F2,F3,F4,F5)
names(Folds)=c("F1","F2","F3","F4","F5")

#Assign numeric variables per dataset
numeric=c(5,0,3,0,0,5)
names(numeric)=c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")
Datasets=list(ACE,CFS,GAFS,RFE_RF,RFE_NB,RFE_CF)
names(Datasets)=c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")

#Resample data to balance class distribution
dataResample=list()
for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  dataResample[[Data]]=Datasets[[Data]]
  dataResample[[Data]][,1:ncol(dataResample[[Data]])]<- lapply(dataResample[[Data]][,1:ncol(dataResample[[Data]])], as.numeric)
  table(dataResample[[Data]]$Y)
  #SMOTe resampling data
  resample=SMOTE(X=dataResample[[Data]][,-ncol(dataResample[[Data]])],target=as.vector(dataResample[[Data]]$Y),K=3,dup_size=1)
  table(resample$data$class)
  #Now we rebuild the data set which is ready
  dataResample[[Data]]=resample$data
  Y = data.frame(c(dataResample[[Data]]$class))
  X = data.frame(dataResample[[Data]][1:ncol(dataResample[[Data]])-1])
  dataResample[[Data]]=cbind(X,Y)
  dataResample[[Data]][,ncol(dataResample[[Data]])]=as.character(dataResample[[Data]][,ncol(dataResample[[Data]])])
  dataResample[[Data]][,(numeric[Data]+1):(ncol(dataResample[[Data]])-1)]=round(dataResample[[Data]][,(numeric[Data]+1):(ncol(dataResample[[Data]])-1)])
  dataResample[[Data]][,(numeric[Data]+1):ncol(dataResample[[Data]])]<- lapply(dataResample[[Data]][, (numeric[Data]+1):ncol(dataResample[[Data]])], as.factor)
  if(numeric[Data]>0){
    dataResample[[Data]][,1:numeric[Data]]<- lapply(dataResample[[Data]][, 1:numeric[Data]], as.numeric)
  }
  name=tail(c(names(dataResample[[Data]])),n=1)
  dataResample[[Data]]=rename(dataResample[[Data]], Y=name)
}

#Metrics used in functions
metrics=c("Accuracy", "Kappa", "F1","Sensitivity","Specificity", "Pos Pred Value", "Neg Pred Value","Detection Rate", "Desirability")


#............................RF..........................
rF_mod=list()
rf_folds=list()
rfResultsComplete=NULL

for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data = dataResample[[Data]] 
  min=max(round(0.1*(ncol(data)-1),0),1)
  max=round(0.5*(ncol(data)-1),0)
  for (nTrees in seq(25,300,25)){
    for (mNodes in 2:floor(0.9*ncol(data))){
      for (mTry in min:max){#min:max)
        params=paste(Data,"-",nTrees,"-",mNodes,"-",mTry)
        rF_mod[[params]]=cvError.RF(ntree=nTrees,maxnodes=mNodes,mtry=mTry)
        rfResultsComplete=rbind(rfResultsComplete,data.frame(Data,Parameters=paste("nTrees =",nTrees,"mNodes =",mNodes,"mTry =", mTry),t(rF_mod[[params]]$meanPM),t(rF_mod[[params]]$sePM),rF_mod[[params]]$SE))
        rf_folds[[params]]=list(rF_mod[[params]]$Performance.Measures)
      }}}}

colnames(rfResultsComplete)[ncol(rfResultsComplete)]<- "Desirability.SE"
rf=na.omit(rfResultsComplete)
rf=data.frame(rf,transmute(rf,nDesirability=Desirability/max(Desirability)))
maxDesirability=filter(rf,Desirability==max(Desirability))$Desirability
SE.maxDesirability=filter(rf,Desirability==max(Desirability))$Desirability.SE
rf=filter(rf,Desirability>=maxDesirability-SE.maxDesirability)
resultsRF=mutate(rf,model="rf")

#............................R Part..........................
rPART_mod=list()
rPART_folds=list()
rpartResultsComplete=NULL

for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data = dataResample[[Data]]
  for (minSplit in 5:10){
    for (minBucket in 3:7){
      for (Cp in c(0,0.001,0.01)){
        for (maxDepth in c(6,8,10)){
          params=paste(Data,"-",minSplit,"-",minBucket,"-",Cp,"-",maxDepth)
          rPART_mod[[params]]=cvError.RPART(minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth)
          rpartResultsComplete=rbind(rpartResultsComplete,data.frame(Data,Parameters=paste("minSplit =",minSplit,"minBucket =",minBucket,"Cp =", Cp,"maxDepth =", maxDepth),t(rPART_mod[[params]]$meanPM),t(rPART_mod[[params]]$sePM),rPART_mod[[params]]$SE))
                                     rPART_folds[[params]]=rPART_mod[[params]]$Performance.Measures
        }}}}}

colnames(rpartResultsComplete)[ncol(rpartResultsComplete)]<- "Desirability.SE"
rpart=na.omit(rpartResultsComplete)
rpart=data.frame(rpart,transmute(rpart,nDesirability=Desirability/max(Desirability)))
maxDesirability=unique(filter(rpart,Desirability==max(Desirability))$Desirability)
SE.maxDesirability=unique(filter(rpart,Desirability==max(Desirability))$Desirability.SE)
rpart=filter(rpart,Desirability>=maxDesirability-SE.maxDesirability)
resultsRPART=mutate(rpart,model="rpart")

#............................  LDA  ..........................
LDA_mod=list()
LDA_folds=list()
LDAResultsComplete=NULL

for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data = dataResample[[Data]] 
  for (mLDA in c("moment","mle","t")){
    params=paste(Data,"-",mLink,"-",threshold)
    LDA_mod[[params]]=cvError.LDA(mLDA=mLDA)
    LDAResultsComplete=rbind(LDAResultsComplete, data.frame(Data,Parameters=paste("mLDA =",mLDA),t(LDA_mod[[params]]$meanPM),t(LDA_mod[[params]]$sePM),LDA_mod[[params]]$SE))
                             LDA_folds[[params]]=list(LDA_mod[[params]]$Performance.Measures)
  }}

colnames(LDAResultsComplete)[ncol(LDAResultsComplete)]<- "Desirability.SE"
LDA=na.omit(LDAResultsComplete)
LDA=data.frame(LDA,transmute(LDA,nDesirability=Desirability/max(Desirability)))
LDAtest=data.frame(LDA %>% group_by(Data,mLDA)) %>% drop_na()
maxDesirability=unique(filter(LDAtest,Desirability==max(Desirability))$Desirability)
SE.maxDesirability=unique(filter(LDA,Desirability ==max(Desirability))$Desirability.SE)
LDA=filter(LDA,Desirability>=maxDesirability-SE.maxDesirability)
resultsLDA=mutate(LDA,model="LDA")


# #..................................SVM................................................
SVM_mod=list()
SVM_folds=list()
SVMResultsComplete=NULL

for (Data in c("ACE","CFS","GAFS","RFE_RF","RFE_NB","RFE_CF")){
  data = dataResample[[Data]] 
  for (Type in c("C-svc")){
    for (Kernel in c("vanilladot","polydot","rbfdot")){
      if (Kernel =="polydot"){
        for(Degree in 1:2) {
          params=paste(Data,"-",Type,"-",Kernel,"-",Degree)
          SVM_mod[[params]]=cvError.SVMpoly(type=Type,kernel=Kernel,degree=Degree)
          SVMResultsComplete=rbind(SVMResultsComplete, data.frame(Data,Parameters=paste("Type =",Type,"Kernel =",Kernel,"Degree =", Degree), t(SVM_mod[[params]]$meanPM),t(SVM_mod[[params]]$sePM), SVM_mod[[params]]$SE))
        }}
      else {
        params=paste(Data,"-",Type,"-",Kernel,"-","NA")
        SVM_mod[[params]]=cvError.SVM(type=Type,kernel=Kernel)
        SVMResultsComplete=rbind(SVMResultsComplete, data.frame(Data,Parameters=paste("Type =",Type,"Kernel =",Kernel,"Degree =", Degree="-"), t(SVM_mod[[params]]$meanPM),t(SVM_mod[[params]]$sePM), SVM_mod[[params]]$SE))
      }
      SVM_folds[[params]]=SVM_mod[[params]]$Performance.Measures
    }}}

colnames(SVMResultsComplete)[ncol(SVMResultsComplete)]<- "Desirability.SE"
SVM=na.omit(SVMResultsComplete)
SVM=data.frame(SVM,transmute(SVM,nDesirability=Desirability/max(Desirability)))
maxDesirability=unique(filter(SVM,Desirability==max(Desirability))$Desirability)
SE.maxDesirability=unique(filter(SVM,Desirability ==max(Desirability))$Desirability.SE)
SVM=filter(SVM,Desirability>=maxDesirability-SE.maxDesirability)
resultsSVM=mutate(SVM,model="SVM")

#.............................Logistic................................................
Log_mod=list()
Log_folds=list()
LogResultsComplete=NULL

for (Data in c("ACE","GAFS","RFE_RF","RFE_NB","RFE_CF")){#no CFS dataset because data caused trouble
  data = dataResample[[Data]] 
  for (mLink in c("logit","probit","cauchit")){
    for (threshold in seq(0.4,0.9,0.05)){
      params=paste(Data,"-",mLink,"-",threshold)
      Log_mod[[params]]=cvError.Log(mLink=mLink,threshold=threshold)
      LogResultsComplete=rbind(LogResultsComplete, data.frame(Data,Parameters=paste("mLink =",mLink,"threshold =",threshold),t(Log_mod[[params]]$meanPM),t(Log_mod[[params]]$sePM),Log_mod[[params]]$SE))
      Log_folds[[params]]=list(Log_mod[[params]]$Parameters,Log_mod[[params]]$Performance.Measures)
    }}}

colnames(LogResultsComplete)[ncol(LogResultsComplete)]<- "Desirability.SE"
LogReg=na.omit(LogResultsComplete)
LogReg=data.frame(LogReg,transmute(LogReg,nDesirability=Desirability/max(Desirability)))
maxDesirability=unique(filter(LogReg,Desirability==max(Desirability))$Desirability)
SE.maxDesirability=unique(filter(LogReg,Desirability==max(Desirability))$Desirability.SE)
LogReg=filter(LogReg,Desirability>=maxDesirability-SE.maxDesirability)
resultsLogReg=mutate(LogReg,model="LogReg")

-----------
  write.table(rf,"ComparisonsRF.csv",row.names=FALSE,sep=",")
write.table(rpart,"ComparisonsRPART.csv",row.names=FALSE,sep=",")
write.table(LDA,"ComparisonsLDA.csv",row.names=FALSE,sep=",")
write.table(LogReg,"ComparisonsLR.csv",row.names=FALSE,sep=",")



results=rbind(resultsRF,resultsRPART,resultsLDA,resultsSVM,resultsLogReg) %>% arrange(desc(Desirability))
print(results)


write.table(results,"ComparisonsResults.csv",row.names=FALSE,sep=",")







