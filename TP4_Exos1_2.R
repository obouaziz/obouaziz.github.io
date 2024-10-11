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
printcp(arbre) #on voit que xerror se stabilise pour certaines valeurs de cp
rpart.plot(arbre)
plotcp(arbre)

#fit_pr<-prune(arbre,cp=0.0011)
#rpart.plot(fit_pr)

#arbre final : utiliser rpart ou mieux, utiliser la fct prune
#arbre_fin=rpart(type ~., data = dataL,method="class",cp=0.0015)
arbre_fin=prune(arbre,cp=0.0026)
rpart.plot(arbre_fin)

#pour comparer, prenons un autre arbre (moins performant)
arbre_try<-prune(arbre,cp=0.024) #4 splits
rpart.plot(arbre_try)

#on devrait trouver un taux de mauvaise classification plus eleve avec
#arbre_try qu'avec arbre_fin
pred_try<-predict(arbre_try,dataT)
mean((pred_try[,2]>0.5)!=(dataT$type=="spam")) #0.1216069

pred_fin<-predict(arbre_fin,dataT)
mean((pred_fin[,2]>0.5)!=(dataT$type=="spam")) #0.082519

###############
#Exo2
###############
setwd("~/Seafile/Enseignement/M2_Classification/Data")

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

data_VIH$cleaved<-as.factor(data_VIH$cleaved)
levels(data_VIH$cleaved)<-c("0","1")

mean(data_VIH$cleaved==1) #0.2307692

#0.8*1625 #1300
idx <- sample(1:1625, 1300, replace = F)
dataL <- data_VIH[c(idx),]
dataT <- data_VIH[-c(idx),]

mean(dataT$cleaved==1) #0.2369231: taux erreur classifieur naif

require(rpart)
require(rpart.plot)
arbre=rpart(cleaved ~., data = dataL,method="class")

rpart.plot(arbre)

printcp(arbre)

? rpart.control
arbre_full<-rpart(cleaved ~., data = dataL,method="class",minbucket=1,cp=0.0001,maxdepth=30)
printcp(arbre_full)

? predict.rpart
head(predict(arbre_full,dataT))
mean((predict(arbre_full,dataT)[,2]>0.5)!=(dataT$cleaved=="1"))#0.1230769

#ou de maniere equivalente en utilisant type="class"
mean(predict(arbre_full,dataT,type="class")!=(dataT$cleaved))

arbre_simple<-rpart(cleaved ~., data = dataL,method="class",minbucket=100,maxdepth=2)
rpart.plot(arbre_simple)

#ou en utilisant prune
arbre_simple<-prune(arbre_full,cp=0.06)
printcp(arbre_simple)

mean((predict(arbre_simple,dataT)[,2]>0.5)!=(dataT$cleaved=="1"))#0.1723077


