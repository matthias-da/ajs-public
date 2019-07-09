## ------------------------------------------------------------------------
## sample frequency counts
library(laeken)
data(eusilc)
table(eusilc[,c("db040", "pb220a")])

## ------------------------------------------------------------------------
dt <- data.table(eusilc)
dt[, .N ,by = list(db040, hsize, pb220a)]

## ----get frequencies -------------------------------------------------
sdc <- createSdcObj(eusilc, 
          keyVars = c("db040", "hsize", "pb220a"),
          weightVar = "rb050", hhId = "db030")
sdc@risk$individual[, "fk"]
fk <- freq(sdc, type = "fk")
# histogram:
hist(fk)
# how many uniques in the sample?
sum(fk == 1)
# which observations are unique?
eusilc[fk == 1,]

## ----get individual risk-------------------------------------------------
head(get.sdcMicroObj(sdc, type="risk")$individual)

## ------------------------------------------------------------------------
args(freqCalc)

## ----echo=TRUE-----------------------------------------------------------
## information on frequencies are assigned to each observation
counts <- freqCalc(eusilc, 
        keyVars = c("db040", "hsize", "pb220a"))$fk
## add counts to data
eusilc <- cbind(eusilc, counts)
## first 6 rows of the sample 
head(eusilc[,c("db040","hsize", "pb220a", "counts")])

## ------------------------------------------------------------------------
X <- aggregate(counts ~ db040 + hsize + pb220a, eusilc, mean)
## number of keys
nrow(X)
## unique keys (frequency = 1)
sum(X$counts == 1)
## first 6 counts for keys (out of 164)
head(X[, c("db040", "hsize", "pb220a", "counts")])

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## library(ggplot2)
## hist <- qplot(X$counts, xlab="Cell size", ylab="Count") +
##   geom_bar(fill="grey50")
## print(hist)

## ----ch3cellsizes, echo=FALSE, fig.height=4.5, fig.width=6, fig.align='center', out.width='3.1in', message=FALSE, warning=FALSE----
library(ggplot2)
hist <- qplot(X$counts, xlab="Cell size", ylab="Count") + 
  geom_bar(fill="grey50") 
print(hist)

## ----echo = FALSE, message=FALSE, warning=FALSE--------------------------
library(simPop)
set.seed(23)
daten <- read.csv2(file = "EasyExampleData.csv", header = T)
daten <- daten[,-3]
Weight <- round(sample(50:150, size=length(daten[,1]), replace=T), digits=-1)
daten <- data.frame(daten,Weight)
toyData <- daten
source("Latex_table.R")

## ----echo=FALSE, eval=FALSE, results='hide'------------------------------
df <- data.frame("key1" = c(1,1,2,NA),
                 "key2" = c(1,1,1,1),
                 "key3" = c(3,NA,3,NA),
                 w = c(10,20,30,40))
f1 <- freqCalc(df, keyVars = 1:3, w = 4, alpha = 1)
f0 <- freqCalc(df, keyVars = 1:3, w = 4, alpha = 0)
f01 <- freqCalc(df, keyVars = 1:3, w= 4, alpha = 0.1)
d <- data.frame("f1"=f1$fk, "f0"=f0$fk, "f01"=f01$fk)
cbind(df, d, data.frame("F1"=f1$Fk, "F0"=f0$Fk, "F01"=f01$Fk))

## ----ldiversity----------------------------------------------------------
res1 <- ldiversity(testdata, 
                   keyVars=c("urbrur","water","sex","age"),
                   ldiv_index="income")
res1

## ------------------------------------------------------------------------
head(res1[,1:5])

## ----echo=FALSE----------------------------------------------------------
library(sdcMicro)
set.seed(23)

#data <- read.csv2(file="/Users/teml/workspace/sdc-springer/book/EasyExampleData.csv", header=TRUE)


## ------------------------------------------------------------------------
load("sdcToy.rda") # use the correct path to
# the data!
toyData <- data
toyData$Weight <- c(110, 70, 80,
                    120,130,90,
                    150,150,130,
                    150,140,120,90,80)
sdcToy <- createSdcObj(toyData,
            keyVars = c("Gender", "Citizenship",
                        "Occupation"),
            numVars = "Income",
            weightVar = "Weight"
            )

## ------------------------------------------------------------------------
toy2 <- sdcToy@risk$individual[, 2:3]
# alternative:
toy2 <- cbind(toyData[,c(4:6,8)], 
            get.sdcMicroObj(sdcToy, 
                      "risk")$individual[,2:3])  
toy2

## ------------------------------------------------------------------------
set.seed(12)
U <- data.frame("var1" = sample(1:2, 16, replace=TRUE),
                "var2" = sample(1:3, 16, replace=TRUE))

## ------------------------------------------------------------------------
set.seed(12445)
select <- sample(1:nrow(U), 4)
S <- U[select, ]
S$weights = rep(4, nrow(S))
S

## ------------------------------------------------------------------------
f <- freqCalc(S, c("var1", "var2"), w=3)
cbind(S, fk=f$fk, Fk=f$Fk)

## ------------------------------------------------------------------------
Fktrue <- freqCalc(U, c("var1", "var2"))
cbind(U[select, ], Fk=f$Fk, Fktrue=Fktrue$Fk[select])

## ------------------------------------------------------------------------
library("laeken")
data(eusilc)
sdc <- createSdcObj(eusilc, 
                    keyVars = c("age", "pb220a", "rb090", "db040"), 
                    weightVar = "rb050")

## ----fig.height=4.5, fig.width=6, fig.align='center',out.width='3.1in'----
risk <- slot(sdc, "risk")$individual
freq <- data.frame(risk[, c("fk", "Fk")])

library("ggplot2")
gg <- ggplot(freq, aes(x=fk, y=Fk)) + geom_point() + 
  xlab("sample frequency counts") + 
  ylab("estimated population frequency counts") 
print(gg)

## ----fig.height=4.5, fig.width=6, fig.align='center',out.width='3.1in'----
eusilc$fk <- freq$fk
eusilc$Fk <- freq$Fk
library(ggplot2)
gg <- ggplot(eusilc, 
             aes(x=fk, y=Fk, 
                 shape=pb220a, colour=rb050, 
                 size=eqIncome)) + 
             geom_point() + 
             xlab("sample frequency counts") + 
             ylab("estimated population frequency counts")
print(gg)

## ----fig.height=4.5, fig.width=6, fig.align='center',out.width='3.1in'----
gg <- ggplot(eusilc, aes(x=fk, y=Fk, colour=db040)) + 
            geom_point() + xlab("sample frequency counts") + 
            ylab("estimated population frequency counts") 
print(gg)

## ----fig.height=4.5, fig.width=6, fig.align='center',out.width='3.1in'----
gg <- ggplot(eusilc, 
             aes(x=fk, y=Fk, shape=pb220a, 
                 colour=rb050, size=eqIncome)) + 
             geom_point() + scale_x_log10() + scale_y_log10() +
             xlab("sample frequency counts (log-scale)") + 
             ylab("estimated population frequency counts (log-scale)")
print(gg)

## ----echo=TRUE-----------------------------------------------------------
tab <- data.frame("age" = c(rep("20s", 7), "60s"), 
                  "gender" = c(rep("male", 4), rep("female", 3), "male"),
                  "income" = c("50k+", "50k+", rep("50k-", 6)),
                  "education" = c(rep("highschool", 4), "university", 
                          "highschool", "middleschool", "university"))
su <- suda2(tab)
## print dis suda scores summary 
su

## ------------------------------------------------------------------------
names(su)

## ------------------------------------------------------------------------
su$score
su$disScore

## ------------------------------------------------------------------------
su$contributionPercent

## ----suda2---------------------------------------------------------------
sdc <- suda2(sdc)

## ------------------------------------------------------------------------
su_silc <- slot(sdc, "risk")$suda
names(su_silc)
su_silc

## ------------------------------------------------------------------------
r <- measure_risk(tab,
  keyVars=c("age","gender","income","education"))$Res

## ----againSDCtestdata----------------------------------------------------
sdc <- createSdcObj(testdata,
          keyVars=c('urbrur','water','sex','age'), 
          numVars=c('expend','income','savings'),
          pramVars=c("walls"), 
          w='sampling_weight', 
          hhId='ori_hid')

## ----get individual risk2------------------------------------------------
risk <- get.sdcMicroObj(sdc, type="risk")$individual
head(risk)

## ------------------------------------------------------------------------
head(cbind("household-ID"=testdata$ori_hid, risk))

## ----ch3hierriskdensity,echo=FALSE, fig.height=4.5, fig.width=6, fig.align='center',out.width='4in'----
testdata$hier_risk <- risk[, "hier_risk"]
gg <- ggplot(testdata, aes(x=hier_risk)) + geom_density()
gg

## ----ch3frequ, echo=FALSE, fig.height=4.5, fig.width=6, fig.align='center',out.width='4in'----
testdata$hier_risk <- risk[, "hier_risk"]
testdata$Fk <- risk[, "Fk"]
testdata$fk <- risk[, "fk"]
gg <- ggplot(testdata, aes(x=Fk, y=hier_risk, colour=fk)) + geom_point() 
gg

## ----print risk 3, echo=TRUE, linewidth=72-------------------------------
print(sdc, "risk")

## ----echo=TRUE-----------------------------------------------------------
data(eusilc)
keyVars <- c("db040", "hsize", "rb090", "age", "pb220a", "pl030")
sdc <- createSdcObj(eusilc, keyVars = keyVars,
                    weightVar = "rb050", hhId = "db030")
form <- as.formula(paste(" ~ ", "db040 + hsize + rb090 + 
             age + pb220a + age:rb090 + age:hsize + 
             hsize:rb090")) 
standardLLM <- as.formula(paste(c("fk", 
                      as.character(form)), 
                      collapse = ""))
standardLLM
standardLLM_log <- as.formula(paste(c("log(fk)", 
                                  as.character(form)), 
                                collapse = ""))
standardLLM_log
pseLLM <- as.formula(paste(c("Nk", 
                      as.character(form)), 
                      collapse = ""))
pseLLM
weightedLLM <- as.formula(paste(c("fk", 
                    as.character(as.formula(
                      paste(c(form,"Fk"),
                            collapse="+")))), 
                            collapse = ""))
weightedLLM

## ----linewidth=72--------------------------------------------------------
## get frequencies
fk <- freqCalc(eusilc, keyVars, w="rb050") 
## assign it to the data set
eusilc$fk <- as.numeric(fk$fk)
eusilc$Fk <- as.numeric(fk$Fk)
## aggregate, to have it for each key only
mu <- aggregate(fk ~ hsize + rb090 + age + db040 + pb220a + pl030, 
                eusilc, unique)
## aggregate the weights
Fk <- aggregate(Fk ~ hsize + rb090 + age + db040 + pb220a + pl030, 
                eusilc, unique)
## save it in a new data.frame
counts <- data.frame(mu, Fk=Fk$Fk)
#counts <- counts[,c(keyVars,"fk","weights")]
counts$age <- as.numeric(counts$age)
head(counts)

## ------------------------------------------------------------------------
mod_standard <- glm(standardLLM, data = counts, family = poisson())
lambda_standard <- fitted(mod_standard)
summary(lambda_standard)

mod_standard_log <- glm(standardLLM_log, data = counts, family = poisson())
lambda_standard_log <- fitted(mod_standard_log)
summary(lambda_standard_log)

## ----results='asis', echo=FALSE------------------------------------------
xt <- xtable(summary(mod_standard), caption="Fitted regression coefficients, standard errors, value of the test statistics and p-value from the standard log-linear model.", label="xtstandard")
print(xt, caption.placement="top", sanitize.text.function = function(x){x},
      hline.after=NULL,  add.to.row=list(pos=list(-1,0, nrow(xt)),  command=c("\\toprule ", "\\midrule ", "\\bottomrule ")))

## ------------------------------------------------------------------------
EC <- counts$fk/counts$Fk 
EC <- log(EC + 0.1)
mod_EC <- glm(standardLLM, data = counts, family = poisson(), offset = EC)
lambda_EC <- fitted(mod_EC)
summary(lambda_EC)

## ----results='asis', echo=FALSE------------------------------------------
xt <- xtable(summary(mod_EC), caption="Fitted regression coefficients, standard errors, value of the test statistics and p-value from the Clogg and Eliason method.", label="xtec")
print(xt, caption.placement="top", sanitize.text.function = function(x){x},
      hline.after=NULL,  add.to.row=list(pos=list(-1,0, nrow(xt)),  command=c("\\toprule ", "\\midrule ", "\\bottomrule ")))

## ------------------------------------------------------------------------
## Pseudo Likelihood: Nk ~ keyVars
f <- sum(counts$fk) / sum(counts$Fk)
N_k <- round(counts$Fk * f) #round
counts <- data.frame(counts, Nk = N_k)
mod_pse <- glm(pseLLM, data = counts, family = poisson())
lambda_pse <- fitted(mod_pse)
summary(lambda_pse)

## ----results='asis', echo=FALSE------------------------------------------
xt <- xtable(summary(mod_pse), caption="Fitted regression coefficients, standard errors, value of the test statistics and p-value from the pseudo maximum likelihood method.", label="xtp")
print(xt, caption.placement="top", sanitize.text.function = function(x){x},
      hline.after=NULL,  add.to.row=list(pos=list(-1,0, nrow(xt)),  command=c("\\toprule ", "\\midrule ", "\\bottomrule ")))

## ------------------------------------------------------------------------
mod_w <- glm(weightedLLM, data = counts, family = poisson())
lambda_w <- fitted(mod_w)
summary(lambda_w)

## ----results='asis', echo=FALSE------------------------------------------
xt <- xtable(summary(mod_w), caption="Fitted regression coefficients, standard errors, value of the test statistics and p-value from the weighted  log-linear method.", label="xtw")
print(xt, caption.placement="top", sanitize.text.function = function(x){x},
      hline.after=NULL,  add.to.row=list(pos=list(-1,0, nrow(xt)),  command=c("\\toprule ", "\\midrule ", "\\bottomrule ")))

## ----modelresults--------------------------------------------------------
## risk for unmodified data using six key variables
m1 <- modRisk(sdc, method = "default", weights = eusilc$rb050, 
         formulaM = form, bound = 5)@risk$model[1:2]
m2 <- modRisk(sdc, method = "CE", weights = eusilc$rb050, 
         formulaM = form, bound = 5)@risk$model[1:2]
m3 <- modRisk(sdc, method = "PML", weights = eusilc$rb050, 
         formulaM = form, bound = 5)@risk$model[1:2]
m4 <- modRisk(sdc, method = "weightedLLM", weights = eusilc$rb050, 
         formulaM = form, bound = 5)@risk$model[1:2]
modelrisk <- data.frame(
               "method" = c("standard", "CE", "PML", "weightedLLM"),
               "tau1" = c(m1$gr1, m2$gr1, m3$gr1, m4$gr1),
               "tau2" = c(m1$gr2, m2$gr2, m3$gr2, m4$gr2))
modelrisk

## ------------------------------------------------------------------------
sdc <- suda2(sdc)
slot(sdc, "risk")$suda2

## ----linewidth=72--------------------------------------------------------
print(sdc, "risk")

## ------------------------------------------------------------------------
sdc <- createSdcObj(testdata,
          keyVars=c('urbrur','water','sex','age'), 
          numVars=c('expend','income','savings'),
          pramVars=c("walls"), 
          w='sampling_weight', 
          hhId='ori_hid')

## ----print numeric risk--------------------------------------------------
print(sdc, "numrisk")

## ----print numeric risk 2------------------------------------------------
sdc <- microaggregation(sdc)
print(sdc, "numrisk")
## try another method
sdc <- undolast(sdc)
sdc <- addNoise(sdc, method="correlated2")
print(sdc, "numrisk")

## ----robust risk, cache=TRUE---------------------------------------------
sdc <- dRiskRMD(sdc)

