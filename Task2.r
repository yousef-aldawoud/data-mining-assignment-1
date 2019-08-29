source(paste(projectDIR ,"Task1.R",sep=""))
# oneR with 310 attributes
OneRModel <- OneR(class ~ ., data = TrainLSVT , na.action=NULL)
 
predOneR <- predict(OneRModel,TestLSVT, na.action=NULL,seed=1)
 
F_weightedOneRAlldata=getFweighted(actual,predOneR)
 
 
#J48 with 310 attributes
J48Model <- J48(class ~ ., data = TrainLSVT , na.action=NULL)
 
predJ48 <- predict(J48Model,TestLSVT, na.action=NULL,seed=1)
 
F_weightedJ48Alldata=getFweighted(actual,predJ48)
 
 
#Naive Bayes with 310 attributes
 
NBModel <- NB(class ~ ., data = TrainLSVT , na.action=NULL)
 
predNB <- predict(NBModel,TestLSVT, na.action=NULL,seed=1)
 
F_weightedNBAlldata=getFweighted(actual,predNB)
 
 
#1NN with 310 attributes
IBkModel <- IBk(class ~ ., data = TrainLSVT , na.action=NULL)
 
predIBk <- predict(IBkModel,TestLSVT, na.action=NULL,seed=1)
 
F_weightedIBkAlldata=getFweighted(actual,predIBk)
 
Cases=c("Before selection","After selection")
Classifiers=c("OneR","J48","Naive Bayes","1NN")
classifiersTable<-matrix(1,2,4)
rownames(classifiersTable)<-Cases
colnames(classifiersTable)<-Classifiers
 
classifiersTable[1,1]<-F_weightedOneRAlldata
classifiersTable[1,2]<-F_weightedJ48Alldata
classifiersTable[1,3]<-F_weightedNBAlldata
classifiersTable[1,4]<-F_weightedIBkAlldata
 
classifiersTable[2,1]<- toString(c(maxFOneR,BestKOneR))
classifiersTable[2,2]<- toString(c(maxFJ48,BestKJ48))
classifiersTable[2,3]<- toString(c(maxFNB,BestKNB))
classifiersTable[2,4]<- toString(c(maxFIBk,BestKIBk))
