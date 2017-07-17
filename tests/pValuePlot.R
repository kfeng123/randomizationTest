library(randomizationTest)

n <- 100
p <- 600
M <- 1000
B <- 1000

####################### factor case I ##############
set.seed(0)
k <- 3
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "factor"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)
hist(temp$A2,breaks = 10)

################# factor case II #########3
k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "factor"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)

####################### MA Gamma k=3 ##############
set.seed(0)
k <- 3
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "MA","innov"="gamma"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)


################# MA Gamma k=500 #########3
k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "MA","innov"="gamma"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)

####################### MA normal k=3 ##############
set.seed(0)
k <- 3
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "MA","innov"="normal"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)


################# MA normal k=500 #########3
k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
temp <- simLevelA14(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "MA","innov"="normal"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)
