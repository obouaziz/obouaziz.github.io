rm(list=ls())

data(iris)
library(class)
library(MASS)

###Exo 1
#1
#sigma=1
p<-0.5
mu1<-3
mu0<-1 #mu1>mu0
y<-rbinom(1,1,p)
x<-y*rnorm(1,mu1)+(1-y)*rnorm(1,mu0)
a<-(mu1-mu0)
b<-(mu1^2-mu0^2)/2
pred<-a*x>=b

#2
risque_bayes<-function(n,p,mu0,mu1){
  if (mu1<mu0) stop("Erreur mu1 doit être plus grand que mu0")
  y<-rbinom(n,1,p)
  x<-y*rnorm(n,mu1)+(1-y)*rnorm(n,mu0)
  a<-(mu1-mu0)
  b<-(mu1^2-mu0^2)/2+log((1-p)/p)
  pred<-a*x>=b
  return(mean(pred!=y))
}
p<-0.5
risque_bayes(100000,p,mu0=1,mu1)

mu1=3;mu0=1
1-pnorm((mu1-mu0)/2) #vrai risque oracle (voir TD)
#0.1586553 pour mu1=3, mu0=1
#0.3085375 pour mu1=2, mu0=1

#3
#a
# data_train=data.frame(Y=grouptrain,X1=X[,1],X2=X[,2])
# fit=lda(Y~X1+X2,data=data_train)

n=10
y.train <- rbinom(n,1,p)
x.train <- y.train*rnorm(n,mu1,1)+(1-y.train)*rnorm(n,mu0,1)
datatrain<-data.frame(y=y.train,x=x.train)
model.lda <- lda(y~x,data=datatrain)
y.test <- rbinom(10,1,p)
x.test <- y.test*rnorm(10,mu1)+(1-y.test)*rnorm(10,mu0)
newdata=data.frame(x=x.test,y=y.test)
pred.lda <- predict(model.lda, newdata = newdata) # voir help(predict.lda) 
risque.estim <- mean(pred.lda$class!=y.test)

##Remarque : on peut egalement coder lda de la facon suivante
#model.lda <- lda(as.matrix(x.train),y.train)
#pred.lda <- predict(model.lda, as.matrix(x.test))

#b
risques<-function(n,p,mu0,mu1){
  y.train <- rbinom(n,1,p)
  x.train <- y.train*rnorm(n,mu1,1)+(1-y.train)*rnorm(n,mu0,1)
  datatrain<-data.frame(y=y.train,x=x.train)
  model.lda <- lda(y~x,data=datatrain)
  y.test <- rbinom(1000,1,p)
  x.test <- y.test*rnorm(1000,mu1)+(1-y.test)*rnorm(1000,mu0)
  newdata=data.frame(x=x.test,y=y.test)
  pred.lda <- predict(model.lda, newdata = newdata)
  
  a<-(mu1-mu0)
  b<-(mu1^2-mu0^2)/2
  pred.bayes<-a*x.test>=b
  
  risque.lda <- mean(pred.lda$class!=y.test)
  risque.bayes<-mean(pred.bayes!=y.test)
  return(list(risque.lda=risque.lda,risque.bayes=risque.bayes))
}

risques(100,p,mu0,mu1)
risques(1000,p,mu0,mu1)
risques(100000,p,mu0,mu1)

result1 = result2 = result3 = result4 = rep(NA,1000)
for (i in 1:1000){
  result1[i] <- risques(50,p,mu0,mu1)$risque.lda
  result2[i] <- risques(100,p,mu0,mu1)$risque.lda
  result3[i] <- risques(400,p,mu0,mu1)$risque.lda
  result4[i] <- risques(1000,p,mu0,mu1)$risque.lda
  
}
boxplot(result1,result2,result3,result4)
