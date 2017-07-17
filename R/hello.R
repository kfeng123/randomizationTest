


########## generate random group transformation

#' generate random group transformation
#' @return ranGT a random group transformation
#' @export
genGT <- function(n) {
    ranGT = rbinom(n, 1, 0.5) * 2 - 1
    return(ranGT)
}

#' generate k sample permutation
#' @return a list of k component
#' @export
kSamplePermutation <- function(k, Ns,ran =sample.int(sum(Ns))) {
    ans <- 0
    out <- list()
    for (i in 1:k) {
        name <- paste0('S', i)
        out[[name]] <- ran[(ans + 1):(ans + Ns[i])]
        ans <- ans + Ns[i]
    }
    return(out)
}

#' randomization test
#' @param initGT the identity of group transformation
#' @param genGT a function to generate random group transformation
#' @param GTStat a function of random group trasformation. It calculate the randomization test statistic
#' @param alpha test level
#' @param B randomization times
#' @export
randomizationTest <- function(GTStat,
                              genGT,
                              initGT,
                              B = 100,
                              alpha = 0.05,
                              needPvalue = FALSE) {
    theSum <- 0
    theTestStat <- GTStat(initGT)
    if (needPvalue) {
        for (i in 1:B) {
            if (GTStat(genGT()) >= theTestStat) {
                theSum <- theSum + 1
            }
        }
        theSum <- theSum + 1
        pvalue <- theSum / (B + 1)
        return(pvalue)
    }
    else{
        for (i in 1:B) {
            if (GTStat(genGT()) >= theTestStat) {
                theSum <- theSum + 1
                if (theSum > (B + 1) * alpha - 1) {
                    return(1)
                }
            }
            else{
                if (i - theSum >= B - (B + 1) * alpha + 1) {
                    return(0)
                }
            }
        }
    }
}







#' simulation of randomization test and CQ test
#' @param n n
#' @param p p
#' @param M simulate M times test procedure to compute empirical power
#' @param k Moving average parameter
#' @param rho Moving average parameter
#' @param mu mean value
#' @param B resample B times to get randomization test
#' @param SNR the signal noise ratio we need, mean value will scale to fit this SNR
#' @export
simRanCQ <- function(n,
                     p,
                     M,
                     mu,
                     sparse,
                     B,
                     SNR,
                     innov = list("type" = "MA", "innov" = "normal"),
                     maParam = NULL,
                     factorParam = NULL,
                     needPvalue = FALSE) {
    if (innov$type == "MA") {
        popVar <- varMovingAverage(p, maParam$k, maParam$rho)
    }
    else{
        popVar <- varFanFactorModel(p, factorParam)
    }
    trSquaredPopVar <- sum(popVar ^ 2)

    if (sparse) {
        mu[-sample(p, floor(p * 0.05))] <- 0
    }
    theSNR <-
        sqrt(n * (n - 1)) * sum(mu ^ 2) / sqrt(2 * trSquaredPopVar)

    # scale the mean
    mu <- mu / sqrt(theSNR) * sqrt(SNR)
    # the SNR
    theNewSNR <-
        sqrt(n * (n - 1)) * sum(mu ^ 2) / sqrt(2 * trSquaredPopVar)

    thePvalue <- rep(0, M)
    CQPvalue <- rep(0, M)
    for (i in 1:M) {
        if (innov$type == "MA") {
            theData <- movingAverageGen(n, p, maParam, mu, innov = innov$innov)
        }
        else if (innov$type == "factor") {
            theData <- fanFactorModelGen(n, p, theParam = factorParam, mu = mu)
        }
        thePvalue[i] <- randomizationTest(
            B = B,
            GTStat = chenTempStatistic(theData),
            genGT = function() {
                ranGT = rbinom(n, 1, 0.5) * 2 - 1
                return(ranGT)
            },
            initGT = rep(1, n),
            alpha = 0.05,
            needPvalue = needPvalue
        )
        CQPvalue[i] <- CQTest(theData, trSquaredPopVar)
    }
    theResult <- list()
    theResult$n <- n
    theResult$p <- p
    theResult$k <- k
    theResult$theoreticalPower <- pnorm(qnorm(0.05) + theNewSNR)
    theResult$randomizationPower <- mean(thePvalue <= 0.05)
    theResult$CQPower <- mean(CQPvalue <= 0.05)
    theResult$SNR <- theNewSNR
    unlist(theResult)
}

#' simulation of two sample permutation test for spiked model
#' @param n n
#' @param p p
#' @param M simulate M times test procedure to compute empirical power
#' @param B resample B times to get randomization test
#' @param SNR the signal noise ratio we need, mean value will scale to fit this SNR
#' @param modelParam the parameters of spiked model, including theSigmaSq, V, D, r
#' @export
simTwoSample <- function(n1,
                         n2,
                         p,
                         M,
                         #mu,
                         sparse,
                         B,
                         SNR,
                         modelParam,
                         needPvalue = FALSE) {
    tau <- 1 / n1 + 1 / n2
    if (sparse) {
        mu <- runif(p, 2, 3)
        mu[-sample(p, floor(p * 0.05))] <- 0
    }
    else{
        mu <- runif(p, 2, 3)
    }
    theSNR <-
        (sum(mu ^ 2) - sum((t(V) %*% mu) ^ 2)) / modelParam$theSigmaSq / sqrt(2 * tau ^
                                                                                  2 * p)

    # scale the mean
    mu <- mu / sqrt(theSNR) * sqrt(SNR)
    # the SNR
    theNewSNR <-
        (sum(mu ^ 2) - sum((t(V) %*% mu) ^ 2)) / modelParam$theSigmaSq / sqrt(2 * tau ^
                                                                                  2 * p)

    # permutation function
    perFun <- function(){
        kSamplePermutation(2,c(n1,n2))
    }
    iniPer <- kSamplePermutation(2,c(n1,n2),seq(n1+n2))

    thePvalue <- rep(0, M)
    CQPvalue <- rep(0, M)
    for (i in 1:M) {
        theData1 <- genSpikedModel(
            n = n1,
            p = p,
            mu = 0,
            modelParam = modelParam
        )
        theData2 <- genSpikedModel(
                n = n2,
                p = p,
                mu = mu,
                modelParam = modelParam
            )
        theData <- rbind(theData1,theData2)
        thePvalue[i] <- randomizationTest(
            B = B,
            GTStat = spikedTempStatistic(n1,n2,r=modelParam$r,theData),
            genGT = perFun,
            initGT = iniPer,
            alpha = 0.05,
            needPvalue = needPvalue
        )
        CQPvalue[i] <- randomizationTest(
            B = B,
            GTStat = twoSampleTempStatistic(n1,n2,theData),
            genGT = perFun,
            initGT = iniPer,
            alpha = 0.05,
            needPvalue = needPvalue
        )
    }
    theResult <- list()
    theResult$n1 <- n1
    theResult$n2 <- n2
    theResult$p <- p
    theResult$theoreticalPower <- pnorm(qnorm(0.05) + theNewSNR)
    theResult$randomizationPower <- mean(thePvalue <= 0.05)
    theResult$CQPower <- mean(CQPvalue <= 0.05)
    theResult$SNR <- theNewSNR
    unlist(theResult)
}





#' simulation of the level of A_1,...,A_4
#' @param n n
#' @param p p
#' @param M simulate M times test procedure
#' @param k Moving average parameter
#' @param rho Moving average parameter
#' @param mu mean value
#' @param B resample B times to get randomization test
#' @export
simLevelA14 <- function(n,
                     p,
                     M,
                     B,
                     innov = list("type" = "MA", "innov" = "normal"),
                     maParam = NULL,
                     factorParam = NULL,
                     needPvalue = FALSE) {
    A1 <- rep(0, M)
    A2 <- rep(0, M)
    A3 <- rep(0, M)
    A4 <- rep(0, M)
    pb <- txtProgressBar(style=3)
    for (i in 1:M) {
        if (innov$type == "MA") {
            theData <- movingAverageGen(n, p, maParam, mu=rep(0,p), innov = innov$innov)
        }
        else if (innov$type == "factor") {
            theData <- fanFactorModelGen(n, p, theParam = factorParam, mu = rep(0,p))
        }
        S <- var(theData)
        A1[i] <- randomizationTest(
            B = B,
            GTStat = A1TempStatistic(theData,S),
            genGT = function() {
                ranGT = rbinom(n, 1, 0.5) * 2 - 1
                return(ranGT)
            },
            initGT = rep(1, n),
            alpha = 0.05,
            needPvalue = needPvalue
        )
        A2[i] <- randomizationTest(
            B = B,
            GTStat = A2TempStatistic(theData,S),
            genGT = function() {
                ranGT = rbinom(n, 1, 0.5) * 2 - 1
                return(ranGT)
            },
            initGT = rep(1, n),
            alpha = 0.05,
            needPvalue = needPvalue
        )
        A3[i] <- randomizationTest(
            B = B,
            GTStat = A3TempStatistic(theData,S),
            genGT = function() {
                ranGT = rbinom(n, 1, 0.5) * 2 - 1
                return(ranGT)
            },
            initGT = rep(1, n),
            alpha = 0.05,
            needPvalue = needPvalue
        )
        A4[i] <- randomizationTest(
            B = B,
            GTStat = A4TempStatistic(theData,S),
            genGT = function() {
                ranGT = rbinom(n, 1, 0.5) * 2 - 1
                return(ranGT)
            },
            initGT = rep(1, n),
            alpha = 0.05,
            needPvalue = needPvalue
        )
        setTxtProgressBar(pb,i/M)
    }
    close(pb)
    theResult <- list()
    theResult$A1 <- A1
    theResult$A2 <- A2
    theResult$A3 <- A3
    theResult$A4 <- A4
    unlist(theResult)
}
