###################### Moving average model ########################

#' generate data by moving average model
#' @param n n
#' @param p p
#' @param k MA order
#' @param rho MA parameter
#' @param mu mean value
#' @export
movingAverageGen <- function(n, p, maParam, mu, innov = "normal") {
    k <- maParam$k
    rho <- maParam$rho
    if (innov == "normal") {
        Z <- rnorm(n * (p + k))
        dim(Z) <- c(n, (p + k))
    }
    else if (innov == "gamma") {
        Z <- rgamma(n * (p + k), shape = 4, scale = 1) - 4
        Z <- Z / 2
        dim(Z) <- c(n, (p + k))
    }
    X <- apply(Z, 1, function(x) {
        y = rep(0, p)
        for (i in 1:p) {
            y[i] = sum(x[i:(i + k)] * rho)
        }
        y = y + mu
        return(y)
    })
    return(t(X))
}


#' calculate the population variance of moving average model
#' @export
varMovingAverage <- function(p, k, rho) {
    myRightShift <- function(a) {
        c(0, a[-length(a)])
    }

    myTriDiag <- function(p, ele) {
        A <- matrix (0, p, p)
        len <- length(ele)
        for (i in 1:min(len, p)) {
            for (j in 1:(p - i + 1)) {
                A[j, j + i - 1] <- ele[i]
            }
        }
        return(A + t(A))
    }

    res <- rep(0, (k + 1))
    temp1 <- rho
    for (i in 1:(k + 1)) {
        res[i] <- sum(temp1 * rho)
        temp1 <- myRightShift(temp1)
    }
    myTriDiag(p, res)
}


##################### Fan's factor model #####################################

#' generate data by factor model as Fan (2007)
#' @param n n
#' @param p p
#' @export
fanFactorModelGen <- function(n, p, theParam, mu) {
    theEpsilon <- rnorm(n * p)
    dim(theEpsilon) <- c(n, p)
    theChi <- (rchisq(n * 4, df = 6) - 6) / sqrt(12)
    dim(theChi) <- c(n, 4)
    temp1 <- c(theParam$a[1:(p / 3)], rep(0, 2 * p / 3))
    temp2 <- c(rep(0, p / 3), theParam$a[(p / 3 + 1):(2 * p / 3)], rep(0, p /
                                                                           3))
    temp3 <- c(rep(0, 2 * p / 3), theParam$a[(2 * p / 3 + 1):p])
    tempValue <- sqrt(temp1 ^ 2 + temp2 ^ 2 + temp3 ^ 2 + theParam$b ^ 2 +
                          1)

    theData <-
        theChi %*% rbind(temp1, temp2, temp3, theParam$b) + theEpsilon
    return(scale(
        scale(theData, center = FALSE, scale = tempValue),
        center = -mu,
        scale = FALSE
    ))

}


#' calculate the population variance of Fan's factor model
#' @export
varFanFactorModel <- function(p, theParam) {
    temp1 <- c(theParam$a[1:(p / 3)], rep(0, 2 * p / 3))
    temp2 <- c(rep(0, p / 3), theParam$a[(p / 3 + 1):(2 * p / 3)], rep(0, p /
                                                                           3))
    temp3 <- c(rep(0, 2 * p / 3), theParam$a[(2 * p / 3 + 1):p])
    tempValue <- sqrt(temp1 ^ 2 + temp2 ^ 2 + temp3 ^ 2 + theParam$b ^ 2 +
                          1)

    temp <- rbind(temp1, temp2, temp3, theParam$b)
    tempVar <- t(temp) %*% temp + diag(p)

    return(diag(tempValue ^ (-1)) %*% tempVar %*% diag(tempValue ^ (-1)))
}

################################ Spiked model #########################

#' generate normal spiked model
#' @param modelParam the parameters of spiked model, including theSigmaSq, V, D, r
#' @export
genSpikedModel <- function(n,p,mu,modelParam){
    r <- modelParam$r
    V <- modelParam$V
    D <- modelParam$D
    theSigmaSq <- modelParam$theSigmaSq

    U <- rnorm(r*n)
    dim(U) <- c(r,n)
    Z <- rnorm(p*n,mean = 0, sd = sqrt(theSigmaSq))
    dim(Z) <- c(p,n)
    return(t(V %*% D %*% U + Z + mu))
}

################################ Compound symmetry structure model #########################

#' generate the compound symmetry structure model in Katayama et. al. (2013)
#' @param modelParam the parameters of spiked model, including theSigmaSq, V, D, r
#' @export
compoundModelGen <- function(n,p, compoundParam,mu){
    rho <- compoundParam$rho
    Z <- rnorm(p*n,mean = 0, sd = 1)
    dim(Z) <- c(p,n)
    return(t(sqrt(1-rho)*Z+sqrt(rho)*outer(rep(1,p),rnorm(n))+ mu))
}

#' calculate the population variance of Fan's factor model
#' @export
varCompoundModel <- function(p, compoundParam) {
    temp <- rep(compoundParam$rho,p*p)
    dim(temp) <- c(p,p)
    diag(temp) <- 1
    return(temp)
}
