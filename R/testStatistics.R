############### One sample Case ####################################
#' generate the randomization statistic based on Chen's test statistic
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
chenTempStatistic <- function(theData) {
    n <- nrow(theData)
    X <- theData %*% t(theData)
    GTStat <- function(ranGT) {
        theSum <- t(ranGT) %*% X %*% ranGT
        return(theSum)
    }
    return(GTStat)
}

#' generate the randomization statistic based on A1
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
A1TempStatistic <- function(theData, S) {
    n <- nrow(theData)
    X <- theData %*%
        (diag(diag(S) ^ (-1))) %*% t(theData)
    GTStat <- function(ranGT) {
        theSum <- t(ranGT) %*% X %*% ranGT
        return(theSum)
    }
    return(GTStat)
}

#' generate the randomization statistic based on A2
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
A2TempStatistic <- function(theData, S) {
    n <- nrow(theData)
    X <- theData %*%
        MASS::ginv(S) %*% t(theData)
    GTStat <- function(ranGT) {
        theSum <- t(ranGT) %*% X %*% ranGT
        return(theSum)
    }
    return(GTStat)
}

#' generate the randomization statistic based on A3
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
A3TempStatistic <- function(theData, S) {
    n <- nrow(theData)
    mySvd <- svd(S)
    A3 <- mySvd$u[,n:p]%*%t(mySvd$u[,n:p])
    X <- theData %*%
        A3 %*% t(theData)
    GTStat <- function(ranGT) {
        theSum <- t(ranGT) %*% X %*% ranGT
        return(theSum)
    }
    return(GTStat)
}

#' generate the randomization statistic based on A4
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
A4TempStatistic <- function(theData, S) {
    n <- nrow(theData)
    X <- theData %*%
        S %*% t(theData)
    GTStat <- function(ranGT) {
        theSum <- t(ranGT) %*% X %*% ranGT
        return(theSum)
    }
    return(GTStat)
}





#' Chen and Qin's test
#' @export
CQTest <- function(theData,
                   trSquaredPopVar = NULL,
                   alpha = 0.05) {
    n <- nrow(theData)
    tempInner <- theData %*% t(theData)
    temp <- tempInner - diag(diag(tempInner))
    theStat <- sum(temp) / 2
    #1 - pnorm(theStat / sqrt(n * (n - 1) / 2 * trSquaredPopVar))

    # variance estimation
    theColMeans <- colMeans(theData)
    tempMeanInner <- theData %*% theColMeans
    trSig1sq = 0
    seq1 = 1:n
    for (i in seq1) {
        for (j in seq1[-i]) {
            trSig1sq = trSig1sq +
                (tempInner[i, i] / (n - 2) + (n - 1) / (n - 2) * tempInner[i, j] -
                     n / (n - 2) * tempMeanInner[i]) *
                ((n - 1) / (n - 2) * tempInner[i, j] + tempInner[j, j] /
                     (n - 2) - n / (n - 2) * tempMeanInner[j])
        }
    }
    trSig1sq <- trSig1sq / n / (n - 1)

    1 - pnorm(theStat / sqrt(n * (n - 1) / 2 * trSig1sq))
}


################### Two Sample Case #############################
#' generate the randomization statistic based on the new method for spiked model
#' @param n1 n1
#' @param n2 n2
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
spikedTempStatistic <- function(n1, n2, r, theData) {
    outFun <- function(ranGT) {
        theData1 <- theData[ranGT$S1, ]
        theData2 <- theData[ranGT$S2, ]
        bar1 <- colMeans(theData1)
        bar2 <- colMeans(theData2)
        var1 <- var(theData1)
        var2 <- var(theData2)
        VHat <-
            eigen((var1 * (n1 - 1) + var2 * (n2 - 1)) / (n1 + n2 - 2), symmetric = TRUE)$vectors[, 1:r]
        e1 <-
            sum(eigen(var1, symmetric = TRUE, only.values = TRUE)$values[-(1:r)]) /
            n1
        e2 <-
            sum(eigen(var2, symmetric = TRUE, only.values = TRUE)$values[-(1:r)]) /
            n2
        return(sum((t(VHat) %*% (bar1 - bar2)) ^ 2) - e1 - e2)
    }
    return(outFun)
}

#' generate the randomization statistic based on Chen Qin's statistic
#' @param n1 n1
#' @param n2 n2
#' @param theData the data
#' @return ranStat a function of random group transformation
#' @export
twoSampleTempStatistic <- function(n1, n2, theData) {
    theTemp <- theData %*% t(theData)
    outFun <- function(ranGT) {
        temp <- theTemp[, c(ranGT$S1, ranGT$S2)]
        temp <- theTemp[c(ranGT$S1, ranGT$S2), ]
        temp1 <- temp[1:n1, 1:n1]
        temp2 <- temp[(n1 + 1):(n1 + n2), (n1 + 1):(n1 + n2)]
        temp3 <- temp[1:n1, (n1 + 1):(n1 + n2)]

        theT <- (sum(temp1) - sum(diag(temp1))) / n1 / (n1 - 1) +
            (sum(temp2) - sum(diag(temp2))) / n2 / (n2 - 1) -
            2 * (sum(temp3) / n1 / n2)
        return(theT)
    }
}
