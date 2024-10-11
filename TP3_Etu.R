rm(list=ls())

setwd("~/Seafile/Enseignement/M2_Classification/2021-22/Data")

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

? step

step(model_all)
step(model_all,k=log(768))

model1 <- glm(Outcome~1,data=diabetes,family = "binomial")
step(model1,scope=list(upper=~diabetes$Pregnancies+diabetes$Glucose+
                         diabetes$BloodPressure+diabetes$SkinThickness+diabetes$Insulin+diabetes$BMI+
                         diabetes$DiabetesPedigreeFunction+diabetes$Age),
     direction="forward",trace="True")


modelBIC <- glm(Outcome~Pregnancies+DiabetesPedigreeFunction
                +BMI+Glucose,data=diabetes,family="binomial")
resBIC=tidy(modelBIC,conf.int = TRUE,exponentiate = TRUE)

#Il ne faut pas hesiter, apres la procedure step, a essayer de rajouter d'autres
#variables pour etre sur qu'on n'a rien rate. Par exemple, on peut vouloir essayer
#de rajouter l'age pour verifier que l'on a bien fait d'omettre cette variable
modelBIC_Age <- glm(Outcome~Pregnancies+DiabetesPedigreeFunction
                    +BMI+Glucose+Age,data=diabetes,family="binomial")
resBIC_Age=tidy(modelBIC_Age,conf.int = TRUE,exponentiate = TRUE)
#Age non significatif


#pour aller plus loin : le modele de regression logistique fait de tres grosses
#hypotheses sur les variables quantitatives (continues et discretes) car il 
#suppose que les odds ratios sont les memes pour une augmentation d'une unite 
#de chaque variable. Il serait interessant de regarder ce qu'il se passe quand 
#on decoupe les variable quantitatives continues en classes et qu'on les utilise 
#comme cela dans le glm.

require(ggeffects)
? ggeffect

ggpredict(modelBIC)
#ou
ggpredict(modelBIC,terms=c("BMI"),condition=c(Pregnancies=1))
ggpredict(modelBIC,terms=c("Glucose"),condition=c(Pregnancies=1))

ggpredict(modelBIC,terms=c("BMI"),condition=c(Pregnancies=1,Glucose=150))


#Calculs a la main
#Preg=1, pedigree=0.47,BMI=60,glucose=150

#avec modelBIC
num=exp(modelBIC$coefficients[1]+modelBIC$coefficients[2]*1+modelBIC$coefficients[3]*0.47+
          modelBIC$coefficients[4]*60+modelBIC$coefficients[5]*150)
num/(1+num) #0.9028203 

#Preg=1, pedigree=0.47,BMI=20,glucose=150
num=exp(modelBIC$coefficients[1]+modelBIC$coefficients[2]*1+modelBIC$coefficients[3]*0.47+
          modelBIC$coefficients[4]*20+modelBIC$coefficients[5]*150)
num/(1+num) #0.2107481

#Courbe ROC
#0.8*768#614.4
idx <- sample(1:768, 614, replace = F)
dataL <- diabetes[c(idx),]
dataT <- diabetes[-c(idx),]

model_train<-glm(Outcome~Pregnancies+DiabetesPedigreeFunction
                 +BMI+Glucose,data=dataL,family="binomial")

#Attention au type dans predict, regarder ? glm.predict
resPred=predict(model_train,newdata=dataT,type="response")
head(resPred)

pred.glm=1*(resPred>=0.5)
mean(pred.glm!=dataT$Outcome) #0.2077922

sval=seq(0,1,by=0.0001)
se=sp=rep(NA,length(sval))
for (j in 1:(length(sval)))
{
  pred.glm=1*(resPred>=sval[j])
  se[j]=sum(pred.glm==dataT$Outcome & dataT$Outcome==1)/(sum(dataT$Outcome==1))
  sp[j]=sum(pred.glm==dataT$Outcome & dataT$Outcome==0)/(sum(dataT$Outcome==0))
}

plot(1-sp,se,type="l",xlab="1-specificite",ylab="sensibilite",main="Courbe ROC")

#Avec le package pROC
require(pROC)
? roc
rocCurve=roc(dataT$Outcome,resPred,print.auc = TRUE,plot=TRUE)
rocCurve$sensitivities

plot(1-rocCurve$specificities,rocCurve$sensitivities,type="l",xlab="1-specificite",ylab="sensibilite",main="Courbe ROC")
lines(1-sp,se,type="l",col="red")


