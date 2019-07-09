## ----echo=FALSE----------------------------------------------------------
require(knitr)
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::opts_chunk$set(fig.path="figures/") # cache=TRUE
knitr::opts_chunk$set(dev=c("pdf","postscript"))
knitr::opts_chunk$set(size="small")
options(width = 62)
#render_sweave()

## ----loaddateusilc, echo=FALSE, warning=FALSE, message=FALSE-------------
library(laeken)
data(eusilc)

## ----mod0----------------------------------------------------------------
intrudersKnowledge <- data.frame("hsize" = 3,
                                 "db040" = "Tyrol",
                                 "age"= 34,
                                 "pl030" = "2",
                                 "pb220a" = "AT",
                                 "eqIncome" = 16090)
intrudersKnowledge 

## ----mod1, echo=TRUE, eval=TRUE, tidy=FALSE------------------------------
data(eusilc)
mod1 <- lm(log(py010n) ~ hsize + db040 + age + 
                         pl030 + pb220a + eqIncome, 
           data=eusilc[eusilc[,"py010n"] > 0, ])
s1 <- summary(mod1)
s1$r.squared

## ----mod2, echo=TRUE, eval=TRUE, tidy=FALSE------------------------------
exp(predict(mod1, intrudersKnowledge))

## ----mod3, echo=TRUE, eval=TRUE, tidy=FALSE------------------------------
exp(predict(mod1, intrudersKnowledge, interval = "prediction"))

## ----eval=FALSE----------------------------------------------------------
## install.packages("laeken")
## data(eusilc)
## ?eusilc

## ----eval=FALSE----------------------------------------------------------
## data(ses)
## ?ses

## ----rumap, echo=FALSE, eval=FALSE, results='hide', warning=FALSE, message=FALSE----
## require(sdcMicro)
## f1 <- function(x){
##   truth <- weighted.mean(x$earningsMonth, x$GrossingUpFactor.x)
## 	SEQ <- c(0.1, 1, 5, seq(10,100,10))
## 	risk <- risk2 <- risk3 <- risk4 <- utility <- utility2 <- utility3 <- utility4 <- perturbed <- perturbed2 <- perturbed3 <- perturbed4 <- numeric(length(SEQ))
## 	j <- 0
## 	for(i in SEQ){
## 		j=j+1
## 		ad <- addNoise(x[,c("earnings","earningsMonth")], noise=i, method="restr")
## 		ad2 <- microaggregation(x[,c("earnings","earningsMonth")], aggr=j+1, method="pca")
##     ad3 <- addNoise(x[,c("earnings","earningsMonth")], noise=i, method="additive")
## #    ad4 <- swappNum(x[,c("earnings","earningsMonth")], w=1:2, p=i)
## 		perturbed[j] <- weighted.mean(ad$xm[,2], x$GrossingUpFactor.x)
## 		perturbed2[j] <- weighted.mean(ad2$mx[,2], x$GrossingUpFactor.x)
##   	perturbed3[j] <- weighted.mean(ad3$xm[,2], x$GrossingUpFactor.x)
##  #   perturbed4[j] <- weighted.mean(ad4$xm[,2], x$GrossingUpFactor.x)
## 		utility[j] <- dUtility(ad$x, ad$xm)		
## 		risk[j] <- dRisk(ad$x, ad$xm, k=0.01)
## 		utility2[j] <- dUtility(ad$x, ad2$mx)		
## 		risk2[j] <- dRisk(ad$x, ad2$mx, k=0.01)
##     utility3[j] <- dUtility(ad$x, ad3$xm)		
## 		risk3[j] <- dRisk(ad$x, ad3$xm, k=0.01)
## #    utility4[j] <- dUtility(ad$x, ad4$xm)  	
## #		risk4[j] <- dRisk(ad$x, ad4$xm, k=0.01)
## 	}	
## 	list(truth=truth, perturbed=perturbed, perturbed2=perturbed2, perturbed3=perturbed3, perturbed4=perturbed4,
##       utility=utility, utility2=utility2, utility3=utility3, utility4=utility4,
##       risk=risk, risk2=risk2, risk3=risk3, risk4=risk4,
##       SEQ=SEQ)
## }
## set.seed(123)
## #load("data/ses.RData")
## #res <- f1(x)
## #save(res, file="data/res.RData")
## load("data/res.RData")
## par(cex.lab=1.5, mar=c(5,4.5,1,0.1))
## plot(cbind(res$risk, res$utility), type="n",
## 	xlab="disclosure risk", ylab="information loss",
## 	xlim=c(0.02,0.7), ylim=c(0,6))
## library(plotrix)
## segments(x0=rep(-0.1,2000),
##          y0=seq(16, 0, length.out=2000),
##          x1=rep(2,2000),
##          y1=seq(0,-16,length.out=2000), lwd=1.4,
##          col=smoothColors("black",1998,"white"))
## lines(cbind(res$risk, res$utility), lty=1)
## lines(cbind(res$risk3, res$utility3), lty=2)
## lines(cbind(res$risk2, res$utility2), lty=3)
## #lines(cbind(res$risk4, res$utility4), lty=4)
## text(x=res$risk, y=res$utility, res$SEQ)
## text(x=res$risk2, y=res$utility2, 2:14, cex=0.7)
## text(x=res$risk3, y=res$utility3, res$SEQ)
## #text(x=res$risk4, y=res$utility4, res$SEQ)
## text(x=0.32,y=0.1, "disclosive", cex=1.5)
## text(x=0.3,y=1.8, "disclosive and low quality", cex=1.5)
## text(x=0.03,y=0.1, "good", cex=1.5)
## text(x=0.064,y=2.2, "low quality", cex=1.5)
## legend("right", legend=c("adding correlated noise","adding additive noise","mdav (aggr. level 2-14)"), lty=c(1:3))	

## ----figru2, echo=FALSE, fig.height=4.5, fig.width=6, fig.align='center',out.width='3.1in'----
r1 <- sort(rlnorm(10), decreasing = TRUE)
r2 <- sort(cumsum(rnorm(10,2)), 
                    decreasing = TRUE)
r3 <- sort(abs(cumsum(log(runif(10)))), 
                    decreasing = TRUE)
m <- max(r1,r2,r3)
r1 <- r1 / m 
r1 <- sqrt(r1)
r1 <- r1 / max(r1)
r1 <- r1 -0.1
r2 <- r2 / m
r2 <- r2 - 0.02
r3 <- r3 / m
u1 <- sort(runif(10), decreasing = TRUE)
u2 <- sort(rlnorm(10), decreasing=TRUE)
u3 <- sort(abs(rnorm(10)), decreasing = TRUE)
m <- max(u1, u2, u3)
u1 <- u1 / max(m)
u1 <- sqrt(sqrt(u1)) 
u1 <- u1 + 0.3
u2 <- u2 / max(m)
u3 <- u3 / max(m)
u3 <- u3 - 0.05


df <- data.frame(
      "method"=rep(c("method 1",  "method 2", 
                    "method 3"), each=11),
      "risk"=c(1, r1, 1, r2, 1, r3),
      "utility"=c(1, u1, 1, u2, 1, u3),
      "parameters"=rep(0:10,3)
)
library(ggplot2)
gg <- ggplot(df, 
             aes(x=risk, y=utility, shape=method)) + geom_line(aes(colour=parameters)) + geom_point(aes(colour=parameters)) + geom_text(aes(colour=parameters), label=df$parameters, vjust=-0.5) + theme_bw() + ylab("utility") + xlab("disclosure risk")
print(gg)

