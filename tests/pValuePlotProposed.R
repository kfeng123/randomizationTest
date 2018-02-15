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

################ Compound symmetry structure rho=0.4 ##############
rhoC=0.4
compound4 <- simLevelProposed(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "compound"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    compoundParam = list("rho"=rhoC),
    needPvalue = TRUE
)

################ Compound symmetry structure rho=0.8 ##############
rhoC=0.8
compound8 <- simLevelProposed(
    n = n,
    p = p,
    M = M,
    B = B,
    innov = list("type" = "compound"),
    maParam = list("k" = k, "rho" = rho),
    factorParam = list("a" = a, "b" = b),
    compoundParam = list("rho"=rhoC),
    needPvalue = TRUE
)



temp1 <- data.frame(
    x=c(factor1$thePvalue,factor1$CQPvalue,factor1$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Factor model: Case I"
)
temp2 <- data.frame(
    x=c(factor2$thePvalue,factor2$CQPvalue,factor2$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Factor model: Case II"
)
temp3 <- data.frame(
    x=c(Gamma3$thePvalue,Gamma3$CQPvalue,Gamma3$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Gamma moving average, k=3"
)
temp4 <- data.frame(
    x=c(Gamma500$thePvalue,Gamma500$CQPvalue,Gamma500$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Gamma moving average, k=500"
)
temp5 <- data.frame(
    x=c(normal3$thePvalue,normal3$CQPvalue,normal3$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Normal moving average, k=3"
)
temp6 <- data.frame(
    x=c(normal500$thePvalue,normal500$CQPvalue,normal500$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Normal moving average, k=500"
)
temp7 <- data.frame(
    x=c(compound4$thePvalue,compound4$CQPvalue,compound4$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Compound symmetry structure, c=0.4"
)
temp8 <- data.frame(
    x=c(compound8$thePvalue,compound8$CQPvalue,compound8$bootstrapPvalue),
    y=c(rep("RM",M),rep("AM",M),rep("BM",M)),
    z="Compound symmetry structure, c=0.8"
)

df <- rbind(temp5,temp6,temp3,temp4)

myPlot <- ggplot(df,aes(x,color=y,linetype=y))+
    stat_ecdf(size=1)+
    scale_linetype_manual(values=c(4,2,1))+
    annotate("segment",x=0,xend=1,y=0,yend=1,linetype=1,size=0.4)+
    ylab("ECDF")+
    guides(colour=guide_legend(title=NULL),linetype=guide_legend(title=NULL))+
    theme_bw()+
    theme(legend.position=c(0.08,0.92))+
    facet_wrap(~z,nrow=2)+
    expand_limits(x=c(0,1),y=c(0,1))

ggsave("pValuePlotProposed.eps",myPlot)#,width=20,height=20,units="cm")

df2 <- rbind(temp1,temp2,temp7,temp8)
myPlot <- ggplot(df2,aes(x,color=y,linetype=y))+
    stat_ecdf(size=1)+
    scale_linetype_manual(values=c(4,2,1))+
    annotate("segment",x=0,xend=1,y=0,yend=1,linetype=1,size=0.4)+
    ylab("ECDF")+
    guides(colour=guide_legend(title=NULL),linetype=guide_legend(title=NULL))+
    theme_bw()+
    theme(legend.position=c(0.08,0.92))+
    facet_wrap(~z,nrow=2)+
    expand_limits(x=c(0,1),y=c(0,1))

ggsave("pValuePlotProposed2.eps",myPlot)#,width=20,height=20,units="cm")
