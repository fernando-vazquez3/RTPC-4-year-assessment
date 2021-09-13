#...................Random Forest CV...................
cvError.RF=function(X,Y,k=5,ntree=nTrees,maxnodes=mNodes,mtry=mTry,PM=NULL)
{
  library(randomForest)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasure=matrix(nrow=nFolds,ncol=4)
  
  #Assing rows per class
  permClass=list()
  obsC=list()
  p=table(data$Y)/nrow(data)
  for (i in 1:length(levels(data$Y)))
  {
    selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
    permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
    assign(paste("permClass",i,sep=""),permRows)
    permClass[[i]]=permRows
    assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
    obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
  }
  
  #Create folds for training and testing
  rowsFold=list()
  for(i in 1:(nFolds-1)){
    rows=list()
    for (j in 1:length(levels(data$Y))){
      if ((i-1)==0){rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]}
      else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
    }
    name=paste("F",i,sep="")
    rowsFold[[name]]=unlist(rows)
  }
  remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
  rowsFold[[paste("F",nFolds,sep="")]]=remaining
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    myRF=randomForest(Y~.,data=training,ntree=nTrees,maxnodes=mNodes,mtry=mTry)  
    predicted=predict(myRF,newdata=testing)
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    cm=confusionMatrix(data=predicted,reference=actual)
    
    #error
    performanceMeasure[i,1]=unname((cm$overall['Accuracy']))
    #kappa
    performanceMeasure[i,2]=unname((cm$overall['Kappa']))  
    #F1
    cm[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasure[i,3]=unname(mean(cm[["byClass"]]["F1"]))
    #Desirability
    performanceMeasure[i,4]=unname((2/9)*(performanceMeasure[i,1])+(1/9)*((1+performanceMeasure[i,2])/2)+(2/3)*(performanceMeasure[i,3]))
  }
  performanceMeasure=data.frame(performanceMeasure)
  names(performanceMeasure)=c("Accuracy","Kappa","F1","Desirability")
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=round(colMeans(performanceMeasure),4)
  sdDesirability=round(apply(performanceMeasure,2,sd)/sqrt(k),4)
  output=list(meanDesirability,sdDesirability,performanceMeasure)
  names(output)=c("mean","sd","Performance.Measures")
  return(output)
}

#...................RPART Decision Tree CV........................
cvError.RPART=function(X,Y,k=5,minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth,weights=NULL)
{
  library(rpart)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasure=matrix(nrow=nFolds,ncol=4)
  
  #Assing rows per class
  permClass=list()
  obsC=list()
  p=table(data$Y)/nrow(data)
  for (i in 1:length(levels(data$Y)))
  {
    selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
    permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
    assign(paste("permClass",i,sep=""),permRows)
    permClass[[i]]=permRows
    assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
    obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
  }
  
  #Loop for folds
  rowsFold=list()
  for(i in 1:(nFolds-1)){
    rows=list()
    for (j in 1:length(levels(data$Y))){
      if ((i-1)==0){
        rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]
      }
      
      else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
    }
    name=paste("F",i,sep="")
    rowsFold[[name]]=unlist(rows)
  }
  remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
  rowsFold[[paste("F",nFolds,sep="")]]=remaining
  
  #Split data into training and testing 
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    myRPART=rpart(Y~.,data=training,weights=NULL,control=rpart.control(minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth))
    predicted=predict(myRPART,newdata=testing,type="class")
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    cm=confusionMatrix(data=predicted,reference=actual)
    
    #error
    performanceMeasure[i,1]=unname((cm$overall['Accuracy']))
    #kappa
    performanceMeasure[i,2]=unname((cm$overall['Kappa']))  
    #F1
    cm[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasure[i,3]=unname(mean(cm[["byClass"]]["F1"]))
    #Desirability
    performanceMeasure[i,4]=unname((2/9)*(performanceMeasure[i,1])+(1/9)*((1+performanceMeasure[i,2])/2)+(2/3)*(performanceMeasure[i,3]))
  }
  performanceMeasure=data.frame(performanceMeasure)
  names(performanceMeasure)=c("Accuracy","Kappa","F1","Desirability")
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=round(colMeans(performanceMeasure),4)
  sdDesirability=round(apply(performanceMeasure,2,sd)/sqrt(k),4)
  output=list(meanDesirability,sdDesirability,performanceMeasure)
  names(output)=c("mean","sd","Performance.Measures")
  return(output)
}

#................................SVM................................
cvError.SVM=function(X,Y,k=5,type=Type,kernel=Kernel)
{
  library(e1071)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasure=matrix(nrow=nFolds,ncol=4)
  
  #Assing rows per class
  permClass=list()
  obsC=list()
  p=table(data$Y)/nrow(data)
  for (i in 1:length(levels(data$Y)))
  {
    selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
    permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
    assign(paste("permClass",i,sep=""),permRows)
    permClass[[i]]=permRows
    assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
    obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
  }
  
  #Create folds for training and testing
  rowsFold=list()
  for(i in 1:(nFolds-1)){
    rows=list()
    for (j in 1:length(levels(data$Y))){
      if ((i-1)==0){rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]}
      else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
    }
    name=paste("F",i,sep="")
    rowsFold[[name]]=unlist(rows)
  }
  remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
  rowsFold[[paste("F",nFolds,sep="")]]=remaining
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    mySVM = svm(formula = Y ~ ., data = training, type = Type, kernel = Kernel) 
    predicted=as.factor(as.numeric(as.character(predict(mySVM,newdata=testing))))
    levels(predicted)=c("0","1")
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    cm=confusionMatrix(data=predicted,reference=actual)
    
    #error
    performanceMeasure[i,1]=unname((cm$overall['Accuracy']))
    #kappa
    performanceMeasure[i,2]=unname((cm$overall['Kappa']))  
    #F1
    cm[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasure[i,3]=unname(mean(cm[["byClass"]]["F1"]))
    #Desirability
    performanceMeasure[i,4]=unname((2/9)*(performanceMeasure[i,1])+(1/9)*((1+performanceMeasure[i,2])/2)+(2/3)*(performanceMeasure[i,3]))
  }
  performanceMeasure=data.frame(performanceMeasure)
  names(performanceMeasure)=c("Accuracy","Kappa","F1","Desirability")
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=round(colMeans(performanceMeasure),4)
  sdDesirability=round(apply(performanceMeasure,2,sd)/sqrt(k),4)
  output=list(meanDesirability,sdDesirability,performanceMeasure)
  names(output)=c("mean","sd","Performance.Measures")
  return(output)
}

#................................LDA...............................
cvError.LDA=function(X,Y,k=5,mLDA=mLDA)
{
  library(MASS)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasure=matrix(nrow=nFolds,ncol=4)
  
  #Assing rows per class
  permClass=list()
  obsC=list()
  p=table(data$Y)/nrow(data)
  for (i in 1:length(levels(data$Y)))
  {
    selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
    permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
    assign(paste("permClass",i,sep=""),permRows)
    permClass[[i]]=permRows
    assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
    obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
  }
  
  #Create folds for training and testing
  rowsFold=list()
  for(i in 1:(nFolds-1)){
    rows=list()
    for (j in 1:length(levels(data$Y))){
      if ((i-1)==0){rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]}
      else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
    }
    name=paste("F",i,sep="")
    rowsFold[[name]]=unlist(rows)
  }
  remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
  rowsFold[[paste("F",nFolds,sep="")]]=remaining
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    training[,1:(ncol(training)-1)]<- lapply(training[,1:(ncol(training)-1)], as.numeric)
    testing[,1:(ncol(testing)-1)]<- lapply(testing[,1:(ncol(testing)-1)], as.numeric)
    myLDA=lda(Y~.,data=training,method=mLDA)
    predicted=predict(myLDA,newdata=testing)$class
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    cm=confusionMatrix(data=predicted,reference=actual)
    
    #error
    performanceMeasure[i,1]=unname((cm$overall['Accuracy']))
    #kappa
    performanceMeasure[i,2]=unname((cm$overall['Kappa']))  
    #F1
    cm[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasure[i,3]=unname(mean(cm[["byClass"]]["F1"]))
    #Desirability
    performanceMeasure[i,4]=unname((2/9)*(performanceMeasure[i,1])+(1/9)*((1+performanceMeasure[i,2])/2)+(2/3)*(performanceMeasure[i,3]))
  }
  performanceMeasure=data.frame(performanceMeasure)
  names(performanceMeasure)=c("Accuracy","Kappa","F1","Desirability")
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=round(colMeans(performanceMeasure),4)
  sdDesirability=round(apply(performanceMeasure,2,sd)/sqrt(k),4)
  output=list(meanDesirability,sdDesirability,performanceMeasure)
  names(output)=c("mean","sd","Performance.Measures")
  return(output)
}

#......................Logistic Regression...............
cvError.Log=function(X,Y,k=5,mLink=mLink,threshold=threshold)
{
  library(MASS)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasure=matrix(nrow=nFolds,ncol=4)
  
  #Assing rows per class
  permClass=list()
  obsC=list()
  p=table(data$Y)/nrow(data)
  for (i in 1:length(levels(data$Y)))
  {
    selectedRows=as.numeric(row.names(data[data$Y==levels(data$Y)[i],]))
    permRows=sample(x=selectedRows,size=length(selectedRows),replace=FALSE)
    assign(paste("permClass",i,sep=""),permRows)
    permClass[[i]]=permRows
    assign(paste("obsC",i,sep=""),p[[i]][1]*floor(nrow(data)/nFolds))
    obsC[[i]]=round(p[[i]][1]*(nrow(data)/nFolds))
  }
  
  #Create folds for training and testing
  rowsFold=list()
  for(i in 1:(nFolds-1)){
    rows=list()
    for (j in 1:length(levels(data$Y))){
      if ((i-1)==0){rows[[j]] = permClass[[j]][c(i:(i*obsC[[j]]))]}
      else{rows[[j]] = permClass[[j]][((i-1)*obsC[[j]]+1):(i*obsC[[j]])]}
    }
    name=paste("F",i,sep="")
    rowsFold[[name]]=unlist(rows)
  }
  remaining = setdiff(1:nrow(data),as.numeric(unlist(rowsFold)))
  rowsFold[[paste("F",nFolds,sep="")]]=remaining
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    myLR=glm(Y~.,data=training,family=binomial(link=mLink),control=(list(maxit=50)))
    predicted=as.factor(as.numeric(predict(myLR,newdata=testing,type="response")>=threshold))
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    cm=confusionMatrix(data=predicted,reference=actual)
    
    #error
    performanceMeasure[i,1]=unname((cm$overall['Accuracy']))
    #kappa
    performanceMeasure[i,2]=unname((cm$overall['Kappa']))  
    #F1
    cm[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasure[i,3]=unname(mean(cm[["byClass"]]["F1"]))
    #Desirability
    performanceMeasure[i,4]=unname((2/9)*(performanceMeasure[i,1])+(1/9)*((1+performanceMeasure[i,2])/2)+(2/3)*(performanceMeasure[i,3]))
  }
  performanceMeasure=data.frame(performanceMeasure)
  names(performanceMeasure)=c("Accuracy","Kappa","F1","Desirability")
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=round(colMeans(performanceMeasure),4)
  sdDesirability=round(apply(performanceMeasure,2,sd)/sqrt(k),4)
  output=list(meanDesirability,sdDesirability,performanceMeasure)
  names(output)=c("mean","sd","Performance.Measures")
  return(output)
}

