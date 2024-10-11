rm(list=ls())

setwd("~/Seafile/Enseignement/M2_Classification/Data")

data_VIH<-read.table("1625Data.csv",header=TRUE,sep=",")
? strsplit

strsplit(data_VIH$Amino.acid[1],"S")

lettre<-c("G","P","A","V","L","I","M","C","F","Y","W","H","K","R","Q","N","E",
          "D","S","T")
grepl(lettre[1],data_VIH$Amino.acid[1:10]) #illustration fonction grepl


data_VIH<-cbind(data_VIH,matrix(NA,dim(data_VIH)[1],20))

for (i in 1:20)
{
  data_VIH[,2+i]<-grepl(lettre[i],data_VIH$Amino.acid)
}
data_VIH=data.frame(data_VIH)
data_VIH=data_VIH[,-1]

data_VIH$cleaved<-as.factor(data_VIH$cleaved)
levels(data_VIH$cleaved)<-c("0","1")