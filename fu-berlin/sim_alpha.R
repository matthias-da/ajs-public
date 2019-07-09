library(simPop)
data(eusilcP)
head(eusilcP, 2)
# eusilcP$region[sample(1:nrow(eusilcP), 2000)] <- NA
# eusilcP$age[sample(1:nrow(eusilcP), 2000)] <- NA
# eusilcP$gender[sample(1:nrow(eusilcP), 2000)] <- NA
# eusilcP$citizenship[sample(1:nrow(eusilcP), 2000)] <- NA
f <- freqCalc(x = eusilcP, keyVars = c("region","age","gender","citizenship"))
library(simFrame)
s <- draw(eusilcP, size = 15000)
s$region[sample(1:nrow(s), 5000)] <- NA
s$age[sample(1:nrow(s), 5000)] <- NA
s$gender[sample(1:nrow(s), 5000)] <- NA
s$citizenship[sample(1:nrow(s), 5000)] <- NA
dim(s)
sdc <- createSdcObj(s, keyVars = c("region","age","gender","citizenship"),
                    weightVar = ".weight")
g <- modRisk(sdc, formulaM = formula("~region + age + gender + citizenship"))
g@risk$model
sum(g@risk$individual[, "risk"]) / 15000 *100
g2 <- modRisk(sdc, formulaM = formula("~region*citizenship + age*ecoStat + gender * ecoStat + hsize"))
g2@risk$model
## why then to use more than the key variables?

truerisk <- sum(f$fk == 1) / 15000 * 100
truerisk

## Idee: mache alpha dependend on how good the categories can be estimated.

## different alpha
sdc0 <- createSdcObj(s, keyVars = c("region","age","gender","citizenship"),
                    weightVar = ".weight", alpha = 0)
g <- modRisk(sdc, formulaM = formula("~region + age + gender + citizenship"))
g@risk$model
sum(g@risk$individual[, "risk"]) / 15000 *100
sdc@risk$individual[, "Fk"]
sdc@risk$individual[, "fk"]


### without alpha
data(eusilcP)
f <- freqCalc(x = eusilcP, keyVars = c("region","age","gender","citizenship"))
library(simFrame)
s <- draw(eusilcP, size = 15000)
sdc <- createSdcObj(s, keyVars = c("region","age","gender","citizenship"),
                    weightVar = ".weight")
g <- modRisk(sdc, formulaM = formula("~region + age + gender + citizenship"))
g@risk$model
sum(g@risk$individual[, "risk"]) / 15000 *100
g2 <- modRisk(sdc, formulaM = formula("~region*citizenship + age*ecoStat + gender * ecoStat + hsize"))
g2@risk$model
## why then to use more than the key variables?

w <- f$fk == 1
w <- w[as.numeric(rownames(s))]
truerisk1 <- sum(sdc@risk$individual[, "fk"] == 1 & w) 
truerisk1 / 15000 * 100
truerisk1
sum(sdc@risk$individual[,"risk"])

truerisk2 <- sum(sdc@risk$individual[, "fk"] == 1 & w) + 
  1/sum(sdc@risk$individual[, "fk"] == 2 & w) + 
  1/sum(sdc@risk$individual[, "fk"] == 3 & w) + 
  1/sum(sdc@risk$individual[, "fk"] == 4 & w) + 
  1/sum(sdc@risk$individual[, "fk"] == 5 & w) + 
  1/sum(sdc@risk$individual[, "fk"] == 6 & w) 
truerisk2 / 15000 * 100
truerisk2

