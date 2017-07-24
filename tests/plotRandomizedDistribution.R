library(randomizationTest)

n <- 100
p <- 600

mybreaks <- seq(from=-4,to=4,length.out=25)

#################### normal MA k=3 ###########################
k <- 3
mu <- rep(0,p)
rho <- runif(k + 1, 2, 3)

theData <- movingAverageGen(n, p,
        maParam = list("k" = k, "rho" = rho),
        mu, innov = "normal")

X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}
theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]

postscript("normal3.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="Normal moving average model, k=3",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()

#################### normal MA k=500 ###########################
k <- 500
mu <- rep(0,p)
rho <- runif(k + 1, 2, 3)

theData <- movingAverageGen(n, p,
        maParam = list("k" = k, "rho" = rho),
        mu, innov = "normal")

X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}

theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]
postscript("normal500.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="Normal moving average model, k=500",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()

#################### gamma MA k=3 ###########################
k <- 3
mu <- rep(0,p)
rho <- runif(k + 1, 2, 3)

theData <- movingAverageGen(n, p,
        maParam = list("k" = k, "rho" = rho),
        mu, innov = "gamma")

X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}

theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]
postscript("gamma3.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="Gamma moving average model, k=3",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()

#################### normal MA k=500 ###########################
k <- 500
mu <- rep(0,p)
rho <- runif(k + 1, 2, 3)

theData <- movingAverageGen(n, p,
        maParam = list("k" = k, "rho" = rho),
        mu, innov = "gamma")

X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}

theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]
postscript("gamma500.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="Gamma moving average model, k=500",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()

#################### factor case I ###########################
a <- rep(0.25, p)
b <- rep(0.1, p)
mu <- rep(0,p)

theData <- fanFactorModelGen(n, p, theParam = list("a"=a,"b"=b), mu = rep(0,p))

X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}

theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]
postscript("factor1.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="factor model, case I",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()

#################### factor case II ###########################
mu <- rep(0,p)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
theData <- fanFactorModelGen(n, p, theParam = list("a"=a,"b"=b), mu = rep(0,p))


X <- theData %*% t(theData)
X <- X-diag(diag(X))
Aconstant <- sqrt(sum(X^2)/2)
randomizedStat <- function(ranGT) {
    theSum <- t(ranGT) %*% X %*% ranGT
    return(theSum/2/Aconstant)
}

theDistribution <- rep(0,10000)
for(i in 1:10000){
    theDistribution[i]<-randomizedStat(genGT(n))
}

theDistribution<-theDistribution[theDistribution<=4]
theDistribution<-theDistribution[theDistribution>=-4]
postscript("factor2.eps",width=4,height=3)
par(mar=c(3,4,4,3)+0.1)
hist(theDistribution,breaks=mybreaks,freq=FALSE,col="gray",
     xlab="",ylab="",
     main="factor model, case II",xlim=c(-4,4))
curve(dnorm,add=TRUE,col="red",lty=1)
dev.off()
