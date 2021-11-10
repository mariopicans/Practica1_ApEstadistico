library(rpart)
library(rpart.plot)
library(caret)

load("College4.RData")
head(College4)

unis<-College4[,colnames(College4)!="Private"]

unis$Tipo<-factor(College4$Private=="Yes",labels=c('Pública','Privada'))
head(unis)
table(unis$Tipo)

set.seed(40)
nobs<-nrow(unis)
itrain<-sample(nobs,0.8*nobs)
train<-unis[itrain,]
test<-unis[-itrain,]

tree<-rpart(Tipo~.,data=train)
rpart.plot(tree,main="Árbol de clasificación privada-pública")

#Selección paramétro

rpart.rules(tree,style="tall")

tree<-rpart(Tipo~.,data=train,cp=0)
plotcp(tree)


xerror<-tree$cptable[,"xerror"]
imin.xerror<-which.min(xerror)
upper.xerror<-xerror[imin.xerror]+tree$cptable[imin.xerror,"xstd"]
icp<-min(which(xerror<=upper.xerror))
cp<-tree$cptable[icp,"CP"]
tree<-prune(tree,cp=cp)

rpart.plot(tree,main="Árbol de clasificación privada-pública")

#Predicciones
obs<-test$Tipo
head(predict(tree,newdata = test))
pred<-predict(tree,newdata=test,type="class")
table(obs,pred)

caret::confusionMatrix(pred,obs)



#Ejercicio 2

rf.caret<-train(Tipo~.,data=train,method="rf")
mtry.class<-sqrt(ncol(train)-1)
tuneGrid