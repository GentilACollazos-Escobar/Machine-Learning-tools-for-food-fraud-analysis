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

#Random Forest (RF)
auc.RFTrain<-NA
tpRFTrain<-NA
fpRFTrain<-NA
fnRFTrain<-NA
tnRFTrain<-NA
FScoreRFTrain<-NA
RecallRFTrain<-NA
PrecisionRFTrain<-NA
SensitivityRFTrain<-NA
SpecificityRFTrain<-NA
AccRFTrain<-NA
MCCRFTrain<-NA
auc.RFTest<-NA
tpRFTest<-NA
fpRFTest<-NA
fnRFTest<-NA
tnRFTest<-NA
FScoreRFTest<-NA
RecallRFTest<-NA
PrecisionRFTest<-NA
SensitivityRFTest<-NA
SpecificityRFTest<-NA
AccRFTest<-NA
MCCRFTest<-NA

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
  
  #RF
  rf<-randomForest(factor(dc.Train[,1])~ .,data=dc.Train[,2:101], method = "class", mtry=30, ntree=550, importance=TRUE, do.trace=FALSE) 
  pred.RFTrain<-prediction(as.numeric(predict(rf, dc.Train[,-c(1)])), dc.Train[,c(1)], label.ordering = NULL)
  auc.RFTrain[i]<-as.numeric(performance(pred.RFTrain,"auc")@y.values)
  ConfusionRFTrain<-confusionMatrix(as.factor(predict(rf, dc.Train[,-c(1)])), as.factor(dc.Train[,c(1)]), mode = "everything", positive="1")
  tpRFTrain[i]<-ConfusionRFTrain$table[4]
  fpRFTrain[i]<-ConfusionRFTrain$table[2]
  fnRFTrain[i]<-ConfusionRFTrain$table[3]
  tnRFTrain[i]<-ConfusionRFTrain$table[1]
  FScoreRFTrain[i]<-ConfusionRFTrain[["byClass"]][["F1"]]
  RecallRFTrain[i]<-ConfusionRFTrain[["byClass"]][["Recall"]]
  PrecisionRFTrain[i]<-ConfusionRFTrain[["byClass"]][["Precision"]]
  SensitivityRFTrain[i]<-ConfusionRFTrain[["byClass"]][["Sensitivity"]]
  SpecificityRFTrain[i]<-ConfusionRFTrain[["byClass"]][["Specificity"]]
  AccRFTrain[i]<-(sum(diag(ConfusionRFTrain[["table"]]))/sum(ConfusionRFTrain[["table"]]))*100
  MCCRFTrain[i]<-(tpRFTrain[i]*tnRFTrain[i]-fpRFTrain[i]*fnRFTrain[i])/sqrt((tpRFTrain[i]+fpRFTrain[i])*(tpRFTrain[i]+fnRFTrain[i])*(tnRFTrain[i]+fpRFTrain[i])*(tnRFTrain[i]+fnRFTrain[i]))
  pred.RFTest<-prediction(as.numeric(predict(rf, dc.Test[,-c(1)])), dc.Test[,c(1)], label.ordering = NULL)
  auc.RFTest[i]<-as.numeric(performance(pred.RFTest,"auc")@y.values)
  ConfusionRFTest<-confusionMatrix(as.factor(predict(rf, dc.Test[,-c(1)])),as.factor(dc.Test[,c(1)]), mode = "everything", positive="1")
  tpRFTest[i]<-ConfusionRFTest$table[4]
  fpRFTest[i]<-ConfusionRFTest$table[2]
  fnRFTest[i]<-ConfusionRFTest$table[3]
  tnRFTest[i]<-ConfusionRFTest$table[1]
  FScoreRFTest[i]<-ConfusionRFTest[["byClass"]][["F1"]]
  RecallRFTest[i]<-ConfusionRFTest[["byClass"]][["Recall"]]
  PrecisionRFTest[i]<-ConfusionRFTest[["byClass"]][["Precision"]]
  SensitivityRFTest[i]<-ConfusionRFTest[["byClass"]][["Sensitivity"]]
  SpecificityRFTest[i]<-ConfusionRFTest[["byClass"]][["Specificity"]]
  AccRFTest[i]<-(sum(diag(ConfusionRFTest[["table"]]))/sum(ConfusionRFTest[["table"]]))*100
  MCCRFTest[i]<-(tpRFTest[i]*tnRFTest[i]-fpRFTest[i]*fnRFTest[i])/sqrt((tpRFTest[i]+fpRFTest[i])*(tpRFTest[i]+fnRFTest[i])*(tnRFTest[i]+fpRFTest[i])*(tnRFTest[i]+fnRFTest[i]))
}


#Goodness of fit
aucTodos<-cbind(auc.RFTrain,auc.RFTest)
FScoreTodos<-cbind(FScoreRFTrain,FScoreRFTest)
RecallTodos<-cbind(RecallRFTrain,RecallRFTest)
PrecisionTodos<-cbind(PrecisionRFTrain,PrecisionRFTest)
MCCTodos<-cbind(MCCRFTrain,MCCRFTest)
AccTodos<-cbind(AccRFTrain,AccRFTest)
SensitivityTodos<-cbind(SensitivityRFTrain,SensitivityRFTest)
SpecificityTodos<-cbind(SpecificityRFTrain,SpecificityRFTest)

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
round(mean(tpRFTrain)) 
round(sd(tpRFTrain))
round(mean(fpRFTrain))
round(sd(fpRFTrain))
round(mean(fnRFTrain))
round(sd(fnRFTrain))
round(mean(tnRFTrain))
round(sd(tnRFTrain))
#Confusion matrix for Testing
round(mean(tpRFTest))
round(sd(tpRFTest))
round(mean(fpRFTest))
round(sd(fpRFTest))
round(mean(fnRFTest))
round(sd(fnRFTest))
round(mean(tnRFTest))
round(sd(tnRFTest))