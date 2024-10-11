rm(list=ls())

###############
#Exo1
###############
library(kernlab)

? spam
data(spam)
View(spam)

#0.75*4601 #3450.75
idx <- sample(1:4601, 3450, replace = F)
dataL <- spam[c(idx),]
dataT <- spam[-c(idx),]
#Les echantillons etant aleatoires, les valeurs dans la suite (en particulier de 
#cp varient d'une simulation a l'autre).

require(rpart)
require(rpart.plot)

arbre=rpart(type ~., data = dataL,method="class")
printcp(arbre)
rpart.plot(arbre)
#cet arbre n'est pas assez profond, on va diminuer la valeur de cp

arbre=rpart(type ~., data = dataL,method="class",cp=0.0001)
printcp(arbre) 
#xerror represente l'erreur cross-validee
#on voit que xerror se stabilise pour certaines valeurs de cp

#Attention : rpart.plot est long a executer ici
#rpart.plot(arbre)
plotcp(arbre)

#fit_pr<-prune(arbre,cp=0.0011)
#rpart.plot(fit_pr)

#arbre final : utiliser rpart ou mieux, utiliser la fct prune
#je choisis une valeur de cp "optimale" en fonction de xerror
#je vais prendre aussi un arbre peu profond pour comparer les
#differences en terme de qualite de classification

arbre_fin=prune(arbre,cp=0.002)
printcp(arbre_fin)
rpart.plot(arbre_fin)

#pour comparer, prenons un autre arbre
arbre_2<-prune(arbre,cp=0.022) #4 splits
printcp(arbre_2)
rpart.plot(arbre_2)

#on devrait trouver un taux de mauvaise classification plus eleve avec
#arbre_2 qu'avec arbre_fin
pred_2<-predict(arbre_2,dataT)
mean((pred_2[,2]>0.5)!=(dataT$type=="spam")) #0.1216069

pred_fin<-predict(arbre_fin,dataT)
mean((pred_fin[,2]>0.5)!=(dataT$type=="spam")) #0.082519

###############
#Exo2
###############
#Q1
setwd("~/Seafile/Enseignement/M2_Classification/2021-22/Data")

data_VIH<-read.table("1625Data.csv",header=TRUE,sep=",")

lettre<-c("G","P","A","V","L","I","M","C","F","Y","W","H","K","R","Q","N","E",
          "D","S","T")
grepl(lettre[1],data_VIH$Amino.acid[1:10]) #illustration de la fonction grepl

data_VIH<-cbind(data_VIH,matrix(NA,dim(data_VIH)[1],20))

for (i in 1:20)
{
  data_VIH[,2+i]<-grepl(lettre[i],data_VIH$Amino.acid)
}
data_VIH=data.frame(data_VIH)
data_VIH=data_VIH[,-1]

#Q2
data_VIH$cleaved<-as.factor(data_VIH$cleaved)
levels(data_VIH$cleaved)<-c("0","1")

#Q3
mean(data_VIH$cleaved==1) #0.2307692

#Q4
#0.8*1625 #1300
idx <- sample(1:1625, 1300, replace = F)
dataL <- data_VIH[c(idx),]
dataT <- data_VIH[-c(idx),]

#Q5
mean(dataT$cleaved==1) #0.2369231: taux erreur classifieur constant

#Q6
require(rpart)
require(rpart.plot)
arbre=rpart(cleaved ~., data = dataL,method="class")

rpart.plot(arbre)

printcp(arbre)

#Q7
? rpart.control
arbre_full<-rpart(cleaved ~., data = dataL,method="class",minbucket=1,cp=0.0001,maxdepth=30)
printcp(arbre_full)

? predict.rpart
head(predict(arbre_full,dataT))
mean((predict(arbre_full,dataT)[,2]>0.5)!=(dataT$cleaved=="1"))#0.1230769

#ou de maniere equivalente en utilisant type="class"
mean(predict(arbre_full,dataT,type="class")!=(dataT$cleaved))

#Q8
arbre_simple<-rpart(cleaved ~., data = dataL,method="class",minbucket=100,maxdepth=2)
rpart.plot(arbre_simple)

#ou en utilisant prune
arbre_simple<-prune(arbre_full,cp=0.06)
printcp(arbre_simple)

mean((predict(arbre_simple,dataT)[,2]>0.5)!=(dataT$cleaved=="1"))#0.1723077

#Q9
library(adabag)
? bagging

arbre_bag<-bagging(cleaved ~., data = dataL,mfinal=20)

mean(predict(arbre_bag,dataT)$class!=(dataT$cleaved))#0.1384615

#Q10
arbre_bag_simple<-bagging(cleaved ~., data = dataL,mfinal=20,control=rpart.control(minbucket=1,cp=0.06,maxdepth=2))
mean(predict(arbre_bag_simple,dataT)$class!=(dataT$cleaved))#0.1784615

#Q11
arbre_bag2<-bagging(cleaved ~., data = dataL,mfinal=20,control=rpart.control(minbucket=1,cp=0.0001,maxdepth=30))
mean(predict(arbre_bag2,dataT)$class!=(dataT$cleaved))#0.09538462

#Q12
#repetitions
result<-matrix(NA,10,6)
mfinal_val=c(1,2,5,10,20,50)
for (i in 1:10)
{
  for (j in 1:6)
  {
    idxnew <- sample(1:1625, 1300, replace = F)
    dataLnew <- data_VIH[c(idxnew),]
    dataTnew <- data_VIH[-c(idxnew),]
    arbre_bagFin<-bagging(cleaved ~., data = dataLnew,mfinal=mfinal_val[j],control=rpart.control(minbucket=1,cp=0.0001,maxdepth=30))
    result[i,j]<-mean(predict(arbre_bagFin,dataTnew)$class!=(dataTnew$cleaved))
  }
  cat("i=",i, ", ", sep="")
}
apply(result,2,mean)
apply(result,2,sd)

#Q13
library(randomForest)
forest1<-randomForest(cleaved ~., data = dataL,ntree=20,mtry=5)
predict(forest1,dataT)

mean(predict(forest1,dataT)!=(dataTnew$cleaved))

seq_mtry<-c(1,2,5,8,10,12,14,16,18)
res=matrix(NA,10,length(seq_mtry))
for (i in 1:10)
{
  idxnew <- sample(1:1625, 1300, replace = F)
  dataLnew <- data_VIH[c(idxnew),]
  dataTnew <- data_VIH[-c(idxnew),]
  for (j in 1:(length(seq_mtry)))
  {
    forest<-randomForest(cleaved ~., data = dataL,ntree=20,mtry=seq_mtry[j])
    res[i,j]<-mean(predict(forest,dataTnew)!=dataTnew$cleaved)
  }
  cat("i=",i,", ",sep="")
}
apply(res,2,mean) #plus bas taux de mauvaise classif pour mtry=12
apply(res,2,sd)

#Q14
seq_ntree<-c(1,2,5,10,20,50,100,200,500)
res2=matrix(NA,10,length(seq_ntree))
for (i in 1:10)
{
  idxnew <- sample(1:1625, 1300, replace = F)
  dataLnew <- data_VIH[c(idxnew),]
  dataTnew <- data_VIH[-c(idxnew),]
  for (j in 1:(length(seq_ntree)))
  {
    forest<-randomForest(cleaved ~., data = dataL,ntree=seq_ntree[j],mtry=12)
    res2[i,j]<-mean(predict(forest,dataTnew)!=dataTnew$cleaved)
  }
  cat("i=",i,", ",sep="")
}

apply(res2,2,mean) #plus bas taux de mauvaise classif pour ntree=200
apply(res,2,sd)





