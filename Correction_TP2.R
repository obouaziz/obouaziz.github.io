rm(list=ls())

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
  if (p<=0 | p>=1) stop("Erreur p doit être compris entre 0 et 1")
  y<-rbinom(n,1,p)
  x<-y*rnorm(n,mu1)+(1-y)*rnorm(n,mu0)
  a<-(mu1-mu0)
  b<-(mu1^2-mu0^2)/2-log(p/(1-p))
  pred<-a*x>=b
  return(mean(pred!=y))
}
p<-0.5
risque_bayes(100000,p,mu0=1,mu1=3)
risque_bayes(100000,p,mu0=2,mu1=3)
risque_bayes(100000,p,mu0=2.5,mu1=3)
risque_bayes(100000,p,mu0=3,mu1=3)

mu0=1;mu1=3 #changer valeurs de mu0 et mu1 et observer l'evolution du risque
1-pnorm((mu1-mu0)/2) #risque oracle theorique (voir TD)

#3
#a
# data_train=data.frame(Y=grouptrain,X1=X[,1],X2=X[,2])
# fit=lda(Y~X1+X2,data=data_train)

n=100
y.train <- rbinom(n,1,p)
x.train <- y.train*rnorm(n,mu1,1)+(1-y.train)*rnorm(n,mu0,1)
datatrain<-data.frame(y=y.train,x=x.train)
model.lda <- lda(y~x,data=datatrain)
y.test <- rbinom(1000,1,p)
x.test <- y.test*rnorm(1000,mu1)+(1-y.test)*rnorm(1000,mu0)
newdata=data.frame(x=x.test,y=y.test)
pred.lda <- predict(model.lda, newdata = newdata) # voir help(predict.lda) 
risque.estim <- mean(pred.lda$class!=y.test)

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
  risque.bayes <- mean(pred.bayes!=y.test)
  return(list(risque.lda=risque.lda,risque.bayes=risque.bayes))
}

risques(100,p,mu0,mu1)
risques(1000,p,mu0,mu1)
risques(100000,p,mu0,mu1)

###Exo 2
summary(iris)
pairs(iris)
cor(iris[,1:4])

AcpIris <- princomp(iris[,1:4], cor = T, scores =T)

AcpIris$sdev

#Contribution des axes a l'inertie totale
sum(AcpIris$sdev[1:2]^2)/sum(AcpIris$sdev^2)

#Representation des individus sur le premier plan factoriel
plot(AcpIris$scores[,1:2], col = iris$Species, pch = 19, 
     main ="Représentation des  individus sur le premier plan factoriel", 
     xlab = "PC1", ylab = "PC2")
legend("bottomright", legend  = unique(iris$Species) , 
       col = unique(iris$Species), pch = 19)

#ACP a la main
dataMatrix <- iris[,1:4]
cenRed <- function(x){y <- sqrt((1/nrow(dataMatrix))*sum((x - mean(x))^2))
out <- (x - mean(x))/y
return(out)}
dataNew <- apply(dataMatrix, 2, cenRed)


V <- (1/nrow(dataNew))*(t(dataNew)%*%dataNew)
diagoV <- eigen(V)


composantePrincipal <- apply(diagoV[[2]],2, function(x){out <- dataNew%*%x
return(out)})
valeurPrincipal <- diagoV[[1]]
correlations <- matrix(c(sqrt(valeurPrincipal[1])*diagoV[[2]][,1], 
                         sqrt(valeurPrincipal[2])*diagoV[[2]][,2]), ncol = 2)

plot(composantePrincipal[,1],-composantePrincipal[,2], col = iris$Species, pch = 19)

#Cercle des correlations
u <- seq(0,2*pi,0.001)
plot(cos(u), sin(u), type = "l", 
     main = "Cercle des corrélations pour le premier plan factoriel", 
     xlab = "PC1", ylab = "PC2", xlim = c(-2,2))
polygon(c(0,0),c(-1,1),  lty = 3)
polygon(c(-1,1), c(0,0), lty = 3)

aux1 <- sapply(1:ncol(dataNew), 
               function(int){arrows(0,0,correlations[int,1],correlations[int,2], 
                                    angle = 15, length = 0.10)
                 text(correlations[int, 1],correlations[int,2], 
                      labels = colnames(dataNew)[int], cex = 0.4, pos  = 4)})


rownames(correlations) <- colnames(dataNew)
round(apply(correlations,1, function(x){sum(x^2)}), digits =2)

#on travaille sur le jeu de donnees ne contenant pas setosa
iris2 <- iris[iris[,5]!="setosa",]
iris2$Species<-droplevels(iris2$Species) 
#pour enlever la modalite setosa qui est inexistante

Nrep <- 100
error <- matrix(NA, nrow = Nrep, ncol = 2)
colnames(error) <- c("knn",  "lda")

for (ii in 1:Nrep){
  N <- 80
  idx1 <- sample(1:50, N/2, replace = F)
  idx0 <- sample(51:100, N/2, replace = F)
  dataL <- iris2[c(idx1,idx0),]
  dataV <- iris2[-c(idx1,idx0),]
  pred.knn <- knn(train = dataL[,-5], test = dataV[,-5], 
                  cl = dataL[,5], k = 3, prob = F)
  fit.lda <- lda(as.matrix(dataL[,-5]), dataL[,5])
  pred.lda <- predict(fit.lda, newdata = as.matrix(dataV[,-5]))$class
  error[ii,] <- c(mean(pred.knn!= dataV[,5]),
                  mean(pred.lda!= dataV[,5]) )
}

errorFinal <- print(round(apply(error,2,mean), digits=2))


boxplot(error,main  = "boxplot des erreurs de classification", col = 3:5)



