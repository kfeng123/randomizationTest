
library(randomizationTest)
library(xtable)

n <- 200
p <- 1002
M <- 2000
B <- 1000

set.seed(0)
k <- 3
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
finalResult <- NULL
for (SNR in 0.5 * 0:6) {
    temp <- simRanCQ(
        n = n,
        p = p,
        M = M,
        mu = mu,
        sparse = FALSE,
        B = B,
        SNR = SNR,
        innov = list("type" = "compound"),
        maParam = list("k" = k, "rho" = rho),
        factorParam = list("a" = a, "b" = b),
        compoundParam = list("rho"=0.4),
        needPvalue = FALSE
    )
    finalResult <- rbind(finalResult, temp)
}
finalResult[, c(5,6,7)] <- round(finalResult[, c(5,6,7)], 3)
jjj1 <- finalResult[, c( 4, 5, 6, 7)]
#print(xtable(finalResult[,c(7,4,5,6)],auto=TRUE),include.rownames = FALSE)


k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
finalResult <- NULL
for (SNR in 0.5 * 0:6) {
    temp <- simRanCQ(
        n = n,
        p = p,
        M = M,
        mu = mu,
        sparse = FALSE,
        B = B,
        SNR = SNR,
        #innov=list("type"="MA","innov"="gamma"),
        innov = list("type" = "compound"),
        maParam = list("k" = k, "rho" = rho),
        factorParam = list("a" = a, "b" = b),
        compoundParam = list("rho"=0.4),
        needPvalue = FALSE
    )
    finalResult <- rbind(finalResult, temp)
}

finalResult[, c(5,6,7)] <- round(finalResult[, c(5,6,7)], 3)
jjj2 <- finalResult[, c(5, 6, 7)]

k <- 3
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
finalResult <- NULL
for (SNR in 0.5 * 0:6) {
    temp <- simRanCQ(
        n = n,
        p = p,
        M = M,
        mu = mu,
        sparse = TRUE,
        B = B,
        SNR = SNR,
        innov=list("type"="compound"),
        maParam = list("k" = k, "rho" = rho),
        factorParam = list("a" = a, "b" = b),
        compoundParam = list("rho"=0.8),
        needPvalue = FALSE
    )
    finalResult <- rbind(finalResult, temp)
}
finalResult[, c(5,6,7)] <- round(finalResult[, c(5,6,7)], 3)
jjj3 <- finalResult[, c(5, 6, 7)]

k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
finalResult <- NULL
for (SNR in 0.5 * 0:6) {
    temp <- simRanCQ(
        n = n,
        p = p,
        M = M,
        mu = mu,
        sparse = TRUE,
        B = B,
        SNR = SNR,
        innov=list("type"="compound"),
        maParam = list("k" = k, "rho" = rho),
        factorParam = list("a" = a, "b" = b),
        compoundParam = list("rho"=0.8),
        needPvalue = FALSE
    )
    finalResult <- rbind(finalResult, temp)
}
finalResult[, c(5,6,7)] <- round(finalResult[, c(5,6,7)], 3)
jjj4 <- finalResult[, c(5, 6, 7)]


xxxxx1 <- xtable(cbind(jjj1, jjj2, jjj3, jjj4), auto = TRUE)
print(xtable(cbind(jjj1, jjj2, jjj3, jjj4), auto = TRUE), include.rownames = FALSE)
