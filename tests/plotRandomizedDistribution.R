
n <- 100
p <- 600

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

hist(theDistribution,breaks=30,freq=FALSE,col="gray",main="haha")

curve(dnorm,add=TRUE,col="red",lty=1)
