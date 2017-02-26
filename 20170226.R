require(rms)
getHdata(prostate)
library(trellis)
attach(prostate)

bone <- factor(bm,labels=c("no mets","bone mets"))
dd <- datadist(age, ap, bone)
options(datadist="dd")

dtime[dtime==0] <- .5
S <- Surv(dtime, status!="alive")

f <- psm(S ~ rcs(age,3) + rcs(log(ap),5) + bone, dist="gauss")
a <- anova(f)
latex(a)
plot(a, pch=1)

s <- summary(f)
page(s)
plot(s, col=1:5, xfrac=.45, cex.t=.9)

latex(f)

srv <- Survival(f)
srv5 <- function(x) srv(5, x)
nomogram(f, ap=c(.1,.5,1,seq(5,30,by=5)), xfrac=.53,
         fun=srv5, funlabel="5y Survival", cex.var=.8)