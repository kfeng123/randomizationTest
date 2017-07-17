
library(randomizationTest)
library(xtable)

n1 <- 10
n2 <- 10
p <- 30
M <- 100
sparse <- TRUE
B <- 100
SNR <- 0

r <-3
beta <- 1
D <- sqrt(runif(r,2,3)*p^beta)
D <- diag(D)
temp <- rnorm(p*r,0,1)
dim(temp) <- c(p,r)
V <- svd(temp)$u

modelParam <- list(theSigmaSq=1,
                   V=V,
                   D=D,
                   r=3)

simTwoSample(n1=n1,
             n2=n2,
             p=p,
             M=M,
             sparse=sparse,
             B=B,
             SNR=SNR,
             modelParam = modelParam,
             needPvalue = FALSE)
