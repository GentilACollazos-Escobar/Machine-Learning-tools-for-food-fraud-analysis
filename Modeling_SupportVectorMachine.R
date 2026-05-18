#Load dataset
data <- read_excel("Dataset_Raw.xlsx")

#Principal Component Analysis
PCA<-prcomp(data[,2:902], center = TRUE, scale. = TRUE)  
X<-scale(data[,2:902], center = TRUE, scale = TRUE)
t<-PCA$x #Scores t
p<-PCA$rotation #Loadings p
pT=t(p)
Xpred<-t%*%pT
# Residual sum squares (RSS)
E<- X-Xpred
E2<-E^2
RSS<-rowSums (E2[,])
RSSlim95<-quantile(x=RSS, probs=c(0.95))
RSSlim99<-quantile(x=RSS, probs=c(0.99))
RSSdummy<-(RSS>2*RSSlim95)
RSSdummy
RSS*RSSdummy
#Hotelling T2
S2inv<-1/apply(t, 2, var)
T2M<-((t^2)%*%diag(S2inv))
T2<-apply(T2M, 1, sum)
T2lim95<-quantile(x=T2, probs=c(0.95))
T2lim99<-quantile(x=T2, probs=c(0.99))
T2dummy<-(T2>2*T2lim95)
T2dummy
T2*T2dummy
par(mfrow=c(1,2))
plot(RSS, xlab="Observations",ylab="Orthogonal distance",pch = 8, cex = 0.8)
text(135, 0.55e-30, "RSS control limit (95%)")
segments(0, RSSlim95, x1 = 511, y1 = RSSlim95, col = "orange")
text(135, 1.3e-29, "RSS control limit (99%)")
segments(0, RSSlim99, x1 = 511, y1 = RSSlim99, col = "red")
plot(T2,xlab="Observations",ylab="Score distance",pch = 8, cex = 0.8)
text(172, 15, "Hotelling T2 Control limit (95%)")
segments(0, T2lim95, x1 = 511, y1 = T2lim95, col = "orange")
text(172, 37, "Hotelling T2 control limit (99%)")
segments(0, T2lim99, x1 = 511, y1 = T2lim99, col = "red")

#Dataset for latent-variable based modeling
datos<-cbind(data[,1],t)
datos<-as.data.frame(datos)

#Support Vector Machines (SVM)
auc.SVMTrain<-NA
tpSVMTrain<-NA
fpSVMTrain<-NA
fnSVMTrain<-NA
tnSVMTrain<-NA
FScoreSVMTrain<-NA
RecallSVMTrain<-NA
PrecisionSVMTrain<-NA
SensitivitySVMTrain<-NA
SpecificitySVMTrain<-NA
AccSVMTrain<-NA
MCCSVMTrain<-NA
auc.SVMTest<-NA
tpSVMTest<-NA
fpSVMTest<-NA
fnSVMTest<-NA
tnSVMTest<-NA
FScoreSVMTest<-NA
RecallSVMTest<-NA
PrecisionSVMTest<-NA
SensitivitySVMTest<-NA
SpecificitySVMTest<-NA
AccSVMTest<-NA
MCCSVMTest<-NA

datosBCGreen<-datos[1:25,] #Green control 
datosBCRoasted<-datos[51:75,] #Roasted control tostado
datosFCGreen<-datos[26:50,]#Green defective
datosFCRoasted<-datos[76:100,]#Fuera de Control tostado

#Stratified repeated holdout
l<-list()
set.seed(1234)
for(i in 1:1000){
  cjoint<-sample(c(1:25),25*0.75)
  l[[i]]<-cjoint
}


for(i in 1:1000){
  cjo<-l[[i]]
  #Training
  datosBCGreenTrain<-datosBCGreen[cjo,]
  datosBCRoastedTrain<-datosBCRoasted[cjo,]
  datosFCGreenTrain<-datosFCGreen[cjo,]
  datosFCRoastedTrain<-datosFCRoasted[cjo,]
  dc.Train<-rbind(datosBCGreenTrain,datosBCRoastedTrain,datosFCGreenTrain,datosFCRoastedTrain)
  
  #Validation
  datosBCGreenTest<-datosBCGreen[-cjo,]
  datosBCRoastedTest<-datosBCRoasted[-cjo,]
  datosFCGreenTest<-datosFCGreen[-cjo,]
  datosFCRoastedTest<-datosFCRoasted[-cjo,]
  dc.Test<-rbind(datosBCGreenTest,datosBCRoastedTest,datosFCGreenTest,datosFCRoastedTest)
  
  #SVM
  svm<-ksvm(V1~., data=dc.Train, scaled = FALSE, kernel = "polydot", type = "C-svc", C=100, kpar = "automatic", epsilon = 0.1, nu=0.2, tol = 0.001, cross = 4)
  pred.SVMTrain<-prediction(as.numeric(predict(svm, dc.Train[,-c(1)])), dc.Train[,c(1)], label.ordering = NULL)
  auc.SVMTrain[i]<-as.numeric(performance(pred.SVMTrain,"auc")@y.values)
  ConfusionSVMTrain<-confusionMatrix(as.factor(predict(svm, dc.Train[,-c(1)])), as.factor(dc.Train[,c(1)]), mode = "everything", positive="1")
  tpSVMTrain[i]<-ConfusionSVMTrain$table[4]
  fpSVMTrain[i]<-ConfusionSVMTrain$table[2]
  fnSVMTrain[i]<-ConfusionSVMTrain$table[3]
  tnSVMTrain[i]<-ConfusionSVMTrain$table[1]
  FScoreSVMTrain[i]<-ConfusionSVMTrain[["byClass"]][["F1"]]
  RecallSVMTrain[i]<-ConfusionSVMTrain[["byClass"]][["Recall"]]
  PrecisionSVMTrain[i]<-ConfusionSVMTrain[["byClass"]][["Precision"]]
  SensitivitySVMTrain[i]<-ConfusionSVMTrain[["byClass"]][["Sensitivity"]]
  SpecificitySVMTrain[i]<-ConfusionSVMTrain[["byClass"]][["Specificity"]]
  AccSVMTrain[i]<-(sum(diag(ConfusionSVMTrain[["table"]]))/sum(ConfusionSVMTrain[["table"]]))*100
  MCCSVMTrain[i]<-(tpSVMTrain[i]*tnSVMTrain[i]-fpSVMTrain[i]*fnSVMTrain[i])/sqrt((tpSVMTrain[i]+fpSVMTrain[i])*(tpSVMTrain[i]+fnSVMTrain[i])*(tnSVMTrain[i]+fpSVMTrain[i])*(tnSVMTrain[i]+fnSVMTrain[i]))
  
  pred.SVMTest<-prediction(as.numeric(predict(svm, dc.Test[,-c(1)])), dc.Test[,c(1)], label.ordering = NULL)
  auc.SVMTest[i]<-as.numeric(performance(pred.SVMTest,"auc")@y.values)
  ConfusionSVMTest<-confusionMatrix(as.factor(predict(svm, dc.Test[,-c(1)])),as.factor(dc.Test[,c(1)]), mode = "everything", positive="1")
  tpSVMTest[i]<-ConfusionSVMTest$table[4]
  fpSVMTest[i]<-ConfusionSVMTest$table[2]
  fnSVMTest[i]<-ConfusionSVMTest$table[3]
  tnSVMTest[i]<-ConfusionSVMTest$table[1]
  FScoreSVMTest[i]<-ConfusionSVMTest[["byClass"]][["F1"]]
  RecallSVMTest[i]<-ConfusionSVMTest[["byClass"]][["Recall"]]
  PrecisionSVMTest[i]<-ConfusionSVMTest[["byClass"]][["Precision"]]
  SensitivitySVMTest[i]<-ConfusionSVMTest[["byClass"]][["Sensitivity"]]
  SpecificitySVMTest[i]<-ConfusionSVMTest[["byClass"]][["Specificity"]]
  AccSVMTest[i]<-(sum(diag(ConfusionSVMTest[["table"]]))/sum(ConfusionSVMTest[["table"]]))*100
  MCCSVMTest[i]<-(tpSVMTest[i]*tnSVMTest[i]-fpSVMTest[i]*fnSVMTest[i])/sqrt((tpSVMTest[i]+fpSVMTest[i])*(tpSVMTest[i]+fnSVMTest[i])*(tnSVMTest[i]+fpSVMTest[i])*(tnSVMTest[i]+fnSVMTest[i]))
}

#Goodness of fit
aucTodos<-cbind(auc.SVMTrain,auc.SVMTest)
FScoreTodos<-cbind(FScoreSVMTrain,FScoreSVMTest)
RecallTodos<-cbind(RecallSVMTrain,RecallSVMTest)
PrecisionTodos<-cbind(PrecisionSVMTrain,PrecisionSVMTest)
MCCTodos<-cbind(MCCSVMTrain,MCCSVMTest)
AccTodos<-cbind(AccSVMTrain,AccSVMTest)
SensitivityTodos<-cbind(SensitivitySVMTrain,SensitivitySVMTest)
SpecificityTodos<-cbind(SpecificitySVMTrain,SpecificitySVMTest)

#Accuracy
Acc.todos.mean<-apply(AccTodos,2, mean)
Acc.todos.mean
Acc.todos.sd<-apply(AccTodos,2, sd)
Acc.todos.sd
#Sensitivity
Sensitivity.todos.mean<-apply(SensitivityTodos,2, mean)
Sensitivity.todos.mean
Sensitivity.todos.sd<-apply(SensitivityTodos,2, sd)
Sensitivity.todos.sd
#Specificity
Specificity.todos.mean<-apply(SpecificityTodos,2, mean)
Specificity.todos.mean
Specificity.todos.sd<-apply(SpecificityTodos,2, sd)
Specificity.todos.sd
#F-score
FScore.todos.mean<-apply(FScoreTodos,2, mean)
FScore.todos.mean
FScore.todos.sd<-apply(FScoreTodos,2, sd)
FScore.todos.sd
#Precision
Precision.todos.mean<-apply(PrecisionTodos,2, mean)
Precision.todos.mean
Precision.todos.sd<-apply(PrecisionTodos,2, sd)
Precision.todos.sd
#Recall
Recall.todos.mean<-apply(RecallTodos,2, mean)
Recall.todos.mean
Recall.todos.sd<-apply(RecallTodos,2, sd)
Recall.todos.sd
#AUC
auc.todos.mean<-apply(aucTodos,2, mean)
auc.todos.mean
auc.todos.sd<-apply(aucTodos,2, sd)
auc.todos.sd
#Matthews correlation coefficient
MCC.todos.mean<-apply(MCCTodos,2, mean)
MCC.todos.mean
MCC.todos.sd<-apply(MCCTodos,2, sd)
MCC.todos.sd

#Confusion matrix for Training
round(mean(tpSVMTrain)) 
round(sd(tpSVMTrain))
round(mean(fpSVMTrain))
round(sd(fpSVMTrain))
round(mean(fnSVMTrain))
round(sd(fnSVMTrain))
round(mean(tnSVMTrain))
round(sd(tnSVMTrain))
#Confusion matrix for Testing
round(mean(tpSVMTest))
round(sd(tpSVMTest))
round(mean(fpSVMTest))
round(sd(fpSVMTest))
round(mean(fnSVMTest))
round(sd(fnSVMTest))
round(mean(tnSVMTest))
round(sd(tnSVMTest))