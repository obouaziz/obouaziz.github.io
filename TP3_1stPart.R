rm(list=ls())

setwd("~/Seafile/Enseignement/M2_Classification/Data")

diabetes=read.table("diabetes.csv",sep=",",header=TRUE)
dim(diabetes) #768 observations, 1 variable reponse (Outcome) et 8 covariables
#Toutes les covariables sont quantitatives continues sauf pregnancies qui est
#quantitative discrete.
attach(diabetes)

hist(SkinThickness) #Grosse classe autour de 0
sum(SkinThickness==0) #227
SkinThickness[SkinThickness==0]<-median(SkinThickness[SkinThickness!=0]) 
#median value is 29
hist(SkinThickness)
boxplot(SkinThickness)
summary(SkinThickness)

hist(Insulin) #Grosse classe autour de 0
sum(Insulin==0) #374
Insulin[Insulin==0]<-median(Insulin[Insulin!=0]) 
hist(Insulin)
boxplot(Insulin)
summary(Insulin)
#median value is 125

boxplot(Glucose) #On voit des valeurs en 0
sum(Glucose==0)#5
Glucose[Glucose==0]<-median(Glucose[Glucose!=0])
hist(Glucose)
boxplot(Glucose)
summary(Glucose)

boxplot(BloodPressure) #On voit des valeurs en 0
sum(BloodPressure==0)#35
BloodPressure[BloodPressure==0]<-median(BloodPressure[BloodPressure!=0])
hist(BloodPressure)
boxplot(BloodPressure)
summary(BloodPressure)

boxplot(BMI) #On voit des valeurs en 0
sum(BMI==0)#11
BMI[BMI==0]<-median(BMI[BMI!=0])
hist(BMI)
boxplot(BMI)
summary(BMI)

table(Pregnancies)
round(prop.table(table(Pregnancies)),3)

hist(Age)
boxplot(Age)
summary(Age)

diabetes$Insulin<-Insulin
diabetes$SkinThickness<-SkinThickness
diabetes$Glucose<-Glucose
diabetes$BloodPressure<-BloodPressure
diabetes$BMI<-BMI

pairs(diabetes[,-9])
#Les donnees manquantes (replacees par la mediane) faussent la representation graphique
#pour Glusose, Insulin et SkinThickness qui en ont beaucoup. Pour bien faire, on pourrait
#aussi regarder les nuages de points quand les donnees manquantes de ces variables sont
#enlevees.

#Au vue de la distribution de cette variable, on peut faire des classes pour pregnancies.
#Par exemple, 0, 1, 2 et 3 ou plus.
preg2<-cut(Pregnancies,c(-0.5,0.5,1.5,2.5,Inf))
levels(preg2)<-c("0","1","2","3+")

by(cbind(preg2,BMI),preg2,summary)
boxplot(BMI~preg2)
boxplot(Age~preg2)
boxplot(Glucose~preg2)
boxplot(SkinThickness~preg2)

#utiliser les pregnancies en classes peut etre interessant aussi pour le modele
#de regression logistique utilise dans la suite. Par souci de simplicite nous
#garderons la variables pregnancies comme quantitative discrete dans la suite.

model_all<-glm(Outcome~.,data=diabetes,family = "binomial")
summary(model_all)

exp(model_all$coefficients)

exp(confint(model_all))

require(broom)
res=tidy(model_all,conf.int = TRUE,exponentiate = TRUE)

require(ggplot2)
#ggplot(res)

ggplot(res, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))

step(model_all)
step(model_all,k=log(768))

model2<-glm(Outcome~Pregnancies+Age+BloodPressure+DiabetesPedigreeFunction
            +BMI+Glucose,data=diabetes,family="binomial")

res2=tidy(model2,conf.int = TRUE,exponentiate = TRUE)
ggplot(res2, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))

#pour aller plus loin : le modele de regression logistique fait de tres grosses
#hypotheses sur les variables quantitatives (continues et discretes) car il 
#suppose que les odds ratios sont les memes pour une augmentation d'une unite 
#de chaque variable. Il serait interessant de regarder ce qu'il se passe quand 
#on decoupe les variable quantitatives continues en classes et qu'on les utilise 
#comme cela dans le glm.



