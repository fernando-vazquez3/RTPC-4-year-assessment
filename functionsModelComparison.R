#...................Random Forest CV...................
cvError.RF=function(k=5,ntree=nTrees,maxnodes=mNodes,mtry=mTry,PM=NULL)
{
  library(randomForest)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasureRF=matrix(nrow=length(metrics),ncol=nFolds)
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(Folds[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
  
    myRF=randomForest(Y~.,data=training,ntree=nTrees,maxnodes=mNodes,mtry=mTry)
    predictedRF=predict(myRF,newdata=testing)
    cmRF=confusionMatrix(data=predictedRF,reference=actual)
    
    #error
    performanceMeasureRF[1,i]=unname((cmRF$overall['Accuracy']))
    #kappa
    performanceMeasureRF[2,i]=unname((cmRF$overall['Kappa']))  
    #F1
    cmRF[["byClass"]]["F1"][is.na(cmRF[["byClass"]]["F1"])] = 0
    performanceMeasureRF[3,i]=unname(mean(cmRF[["byClass"]]["F1"]))
    
    for(j in 4:length(metrics)-1){
      cmRF[["byClass"]][metrics[j]][is.na(cmRF[["byClass"]][metrics[j]])] = 0
      performanceMeasureRF[j,i]=unname(mean(cmRF[["byClass"]][metrics[j]]))
    }
    
    
    #Desirability 
    performanceMeasureRF[length(metrics),i]=sum((3/14)*(performanceMeasureRF[1:3,i]))+sum((1/14)*(performanceMeasureRF[4:(length(metrics)-1),i]))

  }
  
  colnames(performanceMeasureRF) = c(names(Folds))
  rownames(performanceMeasureRF) = metrics
  meanRF = round(rowMeans(performanceMeasureRF),4)
  seRF =round(c(((apply(performanceMeasureRF[1:(length(metrics)-1),],1,sd)/sqrt(5))),sqrt(sum(((3/14)^2)*(apply(performanceMeasureRF[1:3,],1,sd)^2/5))+sum(((1/14)^2)*(apply(performanceMeasureRF[4:(length(metrics)-1),],1,sd)^2/5)))),4)
  performanceMeasure=data.frame(performanceMeasureRF)
  
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=unname(meanRF[length(meanRF)])
  seDesirability=unname(seRF[length(meanRF)])
  output=list(meanDesirability,seDesirability,performanceMeasure,meanRF,seRF)
  names(output)=c("Mean","SE","Performance.Measures","meanPM","sePM")
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
  performanceMeasureRPART=matrix(nrow=length(metrics),ncol=nFolds)
  
  
 
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(Folds[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    
    myRPART=rpart(Y~.,data=training,weights=NULL,control=rpart.control(minsplit=minSplit, minbucket=minBucket,cp=Cp,maxdepth=maxDepth))
    predictedRPART=predict(myRPART,newdata=testing,type="class")
    cmRPART=confusionMatrix(data=predictedRPART,reference=actual)
    
    #error
    performanceMeasureRPART[1,i]=unname((cmRPART$overall['Accuracy']))
    #kappa
    performanceMeasureRPART[2,i]=unname((cmRPART$overall['Kappa']))  
    #F1
    cmRPART[["byClass"]]["F1"][is.na(cmRPART[["byClass"]]["F1"])] = 0
    performanceMeasureRPART[3,i]=unname(mean(cmRPART[["byClass"]]["F1"]))
    
    for(j in 4:length(metrics)-1){
      cmRPART[["byClass"]][metrics[j]][is.na(cmRPART[["byClass"]][metrics[j]])] = 0
      performanceMeasureRPART[j,i]=unname(mean(cmRPART[["byClass"]][metrics[j]]))
    }
    
    #Desirability 
    performanceMeasureRPART[length(metrics),i]=sum((3/14)*(performanceMeasureRPART[1:3,i]))+sum((1/14)*(performanceMeasureRPART[4:(length(metrics)-1),i]))
    
  }
  
  colnames(performanceMeasureRPART) = c(names(Folds))
  rownames(performanceMeasureRPART) = metrics
  meanRPART = round(rowMeans(performanceMeasureRPART),4)
  seRPART =round(c(((apply(performanceMeasureRPART[1:(length(metrics)-1),],1,sd)/sqrt(5))),sqrt(sum(((3/14)^2)*(apply(performanceMeasureRPART[1:3,],1,sd)^2/5))+sum(((1/14)^2)*(apply(performanceMeasureRPART[4:(length(metrics)-1),],1,sd)^2/5)))),4)
  performanceMeasure=data.frame(performanceMeasureRF)
  
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=unname(meanRPART[length(meanRPART)])
  seDesirability=unname(seRPART[length(meanRPART)])
  output=list(meanDesirability,seDesirability,performanceMeasure,meanRPART,seRPART)
  names(output)=c("Mean","SE","Performance.Measures","meanPM","sePM")
  return(output)
}

#................................SVM................................
cvError.SVM=function(X,Y,k=5,type=Type,kernel=Kernel)
{
  library(kernlab)
  library(psych)
  library(PRROC)
  library(caret)
  data=data.frame(data)
  ycol=ncol(data)
  names(data[ycol])="Y"
  nFolds=k
  performanceMeasureSVM=matrix(nrow=length(metrics),ncol=nFolds)
  
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(Folds[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    
    mySVM = ksvm(formula = Y ~ ., data = training, type = Type, kernel = Kernel, kpar=list(degree=Degree)) 
    predictedSVM=as.factor(as.numeric(as.character(predict(mySVM,newdata=testing))))
    levels(predictedSVM)=c("0","1")
    cmSVM=confusionMatrix(data=predictedSVM,reference=actual)
    
    #error
    performanceMeasureSVM[1,i]=unname((cmSVM$overall['Accuracy']))
    #kappa
    performanceMeasureSVM[2,i]=unname((cmSVM$overall['Kappa']))  
    #F1
    cmSVM[["byClass"]]["F1"][is.na(cm[["byClass"]]["F1"])] = 0
    performanceMeasureSVM[3,i]=unname(mean(cmSVM[["byClass"]]["F1"]))
   
    for(j in 4:length(metrics)-1){
      cmRF[["byClass"]][metrics[j]][is.na(cmRF[["byClass"]][metrics[j]])] = 0
      performanceMeasureRF[j,i]=unname(mean(cmRF[["byClass"]][metrics[j]]))
    }
    
    #Desirability 
    performanceMeasureSVM[length(metrics),i]=sum((3/14)*(performanceMeasureSVM[1:3,i]))+sum((1/14)*(performanceMeasureSVM[4:(length(metrics)-1),i]))
    
  }
  colnames(performanceMeasureSVM) = c(names(Folds))
  rownames(performanceMeasureSVM) = metrics
  meanSVM = round(rowMeans(performanceMeasureSVM),4)
  seSVM =round(c(((apply(performanceMeasureSVM[1:(length(metrics)-1),],1,sd)/sqrt(5))),sqrt(sum(((3/14)^2)*(apply(performanceMeasureSVM[1:3,],1,sd)^2/5))+sum(((1/14)^2)*(apply(performanceMeasureSVM[4:(length(metrics)-1),],1,sd)^2/5)))),4)
  performanceMeasureSVM=data.frame(performanceMeasureSVM)
  
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=unname(meanSVM[length(meanSVM)])
  seDesirability=unname(seSVM[length(meanSVM)])
  output=list(meanDesirability,seDesirability,performanceMeasure,meanSVM,seSVM)
  names(output)=c("Mean","SE","Performance.Measures","meanPM","sePM")
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
  performanceMeasureLDA=matrix(nrow=length(metrics),ncol=nFolds)
  
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(rowsFold[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    training[,1:(ncol(training)-1)]<- lapply(training[,1:(ncol(training)-1)], as.numeric)
    testing[,1:(ncol(testing)-1)]<- lapply(testing[,1:(ncol(testing)-1)], as.numeric)
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    
    myLDA=lda(Y~.,data=training,method=mLDA)
    predictedLDA=predict(myLDA,newdata=testing)$class
    cmLDA=confusionMatrix(data=predictedLDA,reference=actual)
    
    #error
    performanceMeasureLDA[1,i]=unname((cmLDA$overall['Accuracy']))
    #kappa
    performanceMeasureLDA[2,i]=unname((cmLDA$overall['Kappa']))  
    #F1
    cmLDA[["byClass"]]["F1"][is.na(cmLDA[["byClass"]]["F1"])] = 0
    performanceMeasureLDA[3,i]=unname(mean(cmLDA[["byClass"]]["F1"]))
    
    for(j in 4:length(metrics)-1){
      cmLDA[["byClass"]][metrics[j]][is.na(cmLDA[["byClass"]][metrics[j]])] = 0
      performanceMeasureLDA[j,i]=unname(mean(cmLDA[["byClass"]][metrics[j]]))
    }
    
    #Desirability 
    performanceMeasureLDA[length(metrics),i]=sum((3/14)*(performanceMeasureLDA[1:3,i]))+sum((1/14)*(performanceMeasureLDA[4:(length(metrics)-1),i]))
    
  }
  
  colnames(performanceMeasureLDA) = c(names(Folds))
  rownames(performanceMeasureLDA) = metrics
  meanLDA = round(rowMeans(performanceMeasureLDA),4)
  seLDA =round(c(((apply(performanceMeasureLDA[1:(length(metrics)-1),],1,sd)/sqrt(5))),sqrt(sum(((3/14)^2)*(apply(performanceMeasureLDA[1:3,],1,sd)^2/5))+sum(((1/14)^2)*(apply(performanceMeasureLDA[4:(length(metrics)-1),],1,sd)^2/5)))),4)
  performanceMeasure=data.frame(performanceMeasureLDA)
  
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=unname(meanLDA[length(meanLDA)])
  seDesirability=unname(seLDA[length(meanLDA)])
  output=list(meanDesirability,seDesirability,performanceMeasure,meanLDA,seLDA)
  names(output)=c("Mean","SE","Performance.Measures","meanPM","sePM")
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
  performanceMeasureLR= matrix(nrow=length(metrics),ncol=nFolds)
  
  #Fit model and estimate error
  for(i in 1:nFolds){
    assign(paste("F",i,sep=""),data[unlist(Folds[[i]]),])
    testing=get(paste("F",i,sep=""))
    trainingRows=setdiff(1:nrow(data),as.numeric(row.names(testing)))
    training=data[trainingRows,]
    names(training)[ncol(training)]="Y"
    names(testing)[ncol(testing)]="Y"
    actual=testing$Y
    
    myLR=glm(Y~.,data=training,family=binomial(link=mLink),control=(list(maxit=50)))
    predictedLR=as.factor(as.numeric(predict(myLR,newdata=testing,type="response")>=threshold))
    cmLR=confusionMatrix(data=predictedLR,reference=actual)
    
    #error
    performanceMeasureLR[1,i]=unname((cmLR$overall['Accuracy']))
    #kappa
    performanceMeasureLR[2,i]=unname((cmLR$overall['Kappa']))  
    #F1
    cmLR[["byClass"]]["F1"][is.na(cmLR[["byClass"]]["F1"])] = 0
    performanceMeasureLR[3,i]=unname(mean(cmLR[["byClass"]]["F1"]))
    
    for(j in 4:length(metrics)-1){
      cmLR[["byClass"]][metrics[j]][is.na(cmLR[["byClass"]][metrics[j]])] = 0
      performanceMeasureLR[j,i]=unname(mean(cmLR[["byClass"]][metrics[j]]))
    }
    
    #Desirability 
    performanceMeasureLR[length(metrics),i]=sum((3/14)*(performanceMeasureLR[1:3,i]))+sum((1/14)*(performanceMeasureLR[4:(length(metrics)-1),i]))
    
  }
  colnames(performanceMeasureLR) = c(names(Folds))
  rownames(performanceMeasureLR) = metrics
  meanLR = round(rowMeans(performanceMeasureLR),4)
  seLR =round(c(((apply(performanceMeasureLR[1:(length(metrics)-1),],1,sd)/sqrt(5))),sqrt(sum(((3/14)^2)*(apply(performanceMeasureLR[1:3,],1,sd)^2/5))+sum(((1/14)^2)*(apply(performanceMeasureLR[4:(length(metrics)-1),],1,sd)^2/5)))),4)
  performanceMeasure=data.frame(performanceMeasureLR)
  
  performanceMeasure=round(performanceMeasure,4)
  meanDesirability=unname(meanLR[length(meanLR)])
  seDesirability=unname(seLR[length(meanLR)])
  output=list(meanDesirability,seDesirability,performanceMeasure,meanLR,seLR)
  names(output)=c("Mean","SE","Performance.Measures","meanPM","sePM")
  return(output)
}

