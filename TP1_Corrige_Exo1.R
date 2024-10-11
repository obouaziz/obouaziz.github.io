rm(list=ls())

###1###
data(airquality)
View(airquality)
? airquality
head(airquality) 
str(airquality)
summary(airquality)
colnames(airquality)
dim(airquality)

airquality$Ozone
Ozone
attach(airquality)
Ozone
detach(airquality)
Ozone

sum(is.na(airquality))
Ozone_new<-Ozone[is.na(Ozone)==FALSE]

dataTemp <- na.omit(airquality)
sum(is.na(dataTemp))
dim(dataTemp)

attach(dataTemp)
summary(Ozone) #Faire attention si il y a des variables quali. 
mean(Ozone)
median(Ozone)
var(Ozone)
sd(Ozone)

quantile(Ozone,c(0.25,0.75))

which.max(Ozone)
#Ou utiliser sort(Ozone,index.return=TRUE)
#pour retrouver les stat d'ordre et l'indice correspondant

mean(Ozone>=80) #0.1531532

boxplot(Ozone)
boxplot(Ozone)$out #135 168

#install.packages("ggplot2")
require(ggplot2)

fig <- ggplot(dataTemp,aes(x = "Ozone", y = Ozone)) +
  geom_boxplot(color = "blue" , fill = "red")+
  labs(title = "boxplot pour la variable Ozone", x = "", y = "valeurs")
fig


fig <- ggplot(data.frame(Ozone))+aes(x = "Ozone", y = Ozone) +
  geom_boxplot(color = "blue" , fill = "red")+
  labs(title = "boxplot pour la variable Ozone", x = "", y = "valeurs")
fig

Month
table(Month)

barplot(table(Month))
ggplot(dataTemp,aes(x = Month,y=Month)) +geom_bar(stat = "identity")

Month2<-as.factor(Month)
levels(Month2)<-c("mai","juin","juillet","aout","septembre")

by(Ozone,Month2,mean)
by(Ozone,Month2,sd)

mean(Ozone[Month2=="mai"])

mean(Ozone[Month2=="juillet"]>50) #0.5769231
mean(Ozone[Month2=="septembre"]>50) #0.137931

df <- data.frame(Ozone = Ozone, Month = Month2)
fig <- ggplot(df) + aes(x = Month, y = Ozone, fill = Month) + geom_boxplot()+labs(x= "Month",y = "Ozone",fill = "Mois")
fig
boxplot(Ozone~Month2)
#Ou
#ggplot(dataTemp,aes(x = as.factor(Month), y = Ozone, fill = Month)) + geom_boxplot()+labs(x= "Month",y = "Ozone",fill = "Mois")

tapply(X= df$Ozone, INDEX = df$Month, function(x)length(boxplot(x)$out))

tapply(X= df$Ozone, INDEX = df$Month, function(x)sum(x>50))
