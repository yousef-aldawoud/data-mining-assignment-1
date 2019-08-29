library(rJava)
library(RWeka)
library(RWekajars)
 
projectDIR <- "/home/Yousef/Desktop/r-studio-projects/Assignment1/"
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
oneR <- make_Weka_classifier("weka/classifiers/rules/OneR")
ibk <- make_Weka_classifier("weka/classifiers/lazy/IBk")
j48 <- make_Weka_classifier("weka/classifiers/trees/J48")
GainRatio <- make_Weka_attribute_evaluator("weka/attributeSelection/GainRatioAttributeEval")
 
 
source(paste(projectDIR ,"getFWeighted.R",sep=""))  #gets the functions used
 
source(paste(projectDIR ,"dropColumns.R",sep=""))
 
source(paste(projectDIR ,"rebalanceData.R",sep=""))
 
 
TrainLSVT<-read.arff(paste(projectDIR ,"datasets/LSVT_train.arff",sep=""))
colnames(TrainLSVT)[ncol(TrainLSVT)] <- "class" 
 
TestLSVT<-read.arff(paste(projectDIR ,"datasets/LSVT_test.arff",sep=""))
colnames(TestLSVT)[ncol(TestLSVT)] <- "class" 
actual<-TestLSVT[, ncol(TestLSVT)] 
 
 
D <- vector()
accuracy<-vector()
F1_1<-vector()
F1_2<-vector()
Prec_1<-vector()
Prec_2<-vector()
Recall_1<-vector()
Recall_2<-vector()
nc1 <- 0
nc2 <- 0
 
 
F_weightedOneR<- vector()
F_weightedJ48 <-vector()
F_weightedNB<-vector()
F_weightedIBk<-vector()
 
for (i in actual){
 if (i==1){
   nc1 <- nc1 + 1
 }
 else{
   nc2 <- nc2 + 1
 }
}
 
features_number <- seq(305,5,-5)
n <- 1
 
A <- GainRatio(class ~ . , data = TrainLSVT,na.action=NULL )
ranked_list<- A[order(A)]
 
 
 
for (K in features_number){
 
 D[n]=310-K    #Numb of attributes to drop
 s<- ranked_list[1:D[n]]
 cols.dont.want <- c(names(s))
  TrainLSVTDropped <- TrainLSVT[, !names(TrainLSVT) %in% cols.dont.want, drop = T]
 TestLSVTDropped <- TestLSVT[, !names(TestLSVT) %in% cols.dont.want, drop = T]
  #oneR with K number of attributes
 OneRModel <- OneR(class ~ ., data = TrainLSVTDropped , na.action=NULL)
  predOneR <- predict(OneRModel,TestLSVTDropped, na.action=NULL,seed=1)
  F_weightedOneR[n]=getFweighted(actual,predOneR)
 
 #J48 with K number of attributes
 J48Model <- J48(class ~ ., data = TrainLSVTDropped , na.action=NULL)
  predJ48 <- predict(J48Model,TestLSVTDropped, na.action=NULL,seed=1)
  F_weightedJ48[n]=getFweighted(actual,predJ48)
 
 #Naive Bayes with K number of attributes
  NBModel <- NB(class ~ ., data = TrainLSVTDropped , na.action=NULL)
  predNB <- predict(NBModel,TestLSVTDropped, na.action=NULL,seed=1)
  F_weightedNB[n]=getFweighted(actual,predNB)
 
 #1NN with K number of attributes
 IBkModel <- IBk(class ~ ., data = TrainLSVTDropped , na.action=NULL)
  predIBk <- predict(IBkModel,TestLSVTDropped, na.action=NULL,seed=1)
  F_weightedIBk[n]=getFweighted(actual,predIBk)
 
 n <- n+1
}
 
maxFOneR=max(F_weightedOneR)
BestKOneR= features_number[which.max(F_weightedOneR)]
 
maxFJ48=max(F_weightedJ48)
BestKJ48=features_number[which.max(F_weightedJ48)]
 
maxFNB=max(F_weightedNB)
BestKNB=features_number[which.max(F_weightedNB)]
 
maxFIBk=max(F_weightedIBk)
BestKIBk=features_number[which.max(F_weightedIBk)]
 
