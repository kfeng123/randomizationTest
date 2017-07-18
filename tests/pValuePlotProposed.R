library(randomizationTest)
library(ggplot2)

n <- 100
p <- 600
M <- 2000
B <- 1000


####################### factor case I ##############
set.seed(0)
k <- 3
rho <- runif(k + 1, 2, 3)
a <- rep(0.25, p)
b <- rep(0.1, p)
factor1 <- simLevelProposed(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "factor"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)




################# factor case II #########3
k <- 500
mu <- runif(p, 2, 3)
rho <- runif(k + 1, 2, 3)
a <- runif(p, 0, 0.4)
b <- runif(p, 0, 0.2)
factor2 <- simLevelProposed(
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
Gamma3 <- simLevelProposed(
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
Gamma500 <- simLevelProposed(
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
normal3 <- simLevelProposed(
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
normal500 <- simLevelProposed(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "MA","innov"="normal"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    needPvalue = TRUE
)


temp1 <- data.frame(
    x=c(factor1$thePvalue,factor1$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Factor model: Case I"
)
temp2 <- data.frame(
    x=c(factor2$thePvalue,factor2$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Factor model:Case II"
)
temp3 <- data.frame(
    x=c(Gamma3$thePvalue,Gamma3$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Gamma moving average, k=3"
)
temp4 <- data.frame(
    x=c(Gamma500$thePvalue,Gamma500$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Gamma moving average, k=500"
)
temp5 <- data.frame(
    x=c(normal3$thePvalue,normal3$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Normal moving average, k=3"
)
temp6 <- data.frame(
    x=c(normal500$thePvalue,normal500$CQPvalue),
    y=c(rep("RM",M),rep("AM",M)),
    z="Normal moving average, k=500"
)

df <- rbind(temp5,temp6,temp3,temp4,temp1,temp2)


myPlot <- ggplot(df,aes(x,colour=y))+
    stat_ecdf(size=0.8)+
    annotate("segment",x=0,xend=1,y=0,yend=1,linetype="dashed")+
    ylab("ECDF")+
    guides(colour=guide_legend(title=NULL))+
    theme_bw()+
    facet_wrap(~z,nrow=3)+
    expand_limits(x=c(0,1),y=c(0,1))

ggsave("pValuePlotProposed.png",myPlot,width=20,height=20,units="cm")
