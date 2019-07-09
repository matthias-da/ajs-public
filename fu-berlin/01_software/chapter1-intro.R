## ----pack, echo=FALSE, message=FALSE, warning=FALSE----------------------
#require(simFrame)
#require(Rcpp)
#v install.packages(c("sdcMicro", "simPop", "laeken"))
library(data.table)
library(devtools)
library(ggplot2)
library(knitr)
library(laeken)
library(MASS)
library(sdcMicro)
#library(sdcMicroGUI)
library(sets)
library(simPop)
library(VIM)

## ------------------------------------------------------------------------
5 + 2 * log(3 * 3)
2 + 3
"+"(2, 3)

## ------------------------------------------------------------------------
set.seed(123)
mean(rnorm(10))

## ----eval=FALSE----------------------------------------------------------
## res1 <- name_of_function(v1) # an input argument
## res2 <- name_of_function(v1, v2) # two input arguments
## res3 <- name_of_function(v1, v2, v3) # three input arguments
## # ...

## ------------------------------------------------------------------------
require(sdcMicro)
args(addNoise)

## ----eval=FALSE----------------------------------------------------------
## ?addNoise

## ------------------------------------------------------------------------
x <- rnorm(10)
x

## ----install, eval=FALSE-------------------------------------------------
## install.packages("sdcMicro")
## install.packages("simPop")

## ----eval=FALSE----------------------------------------------------------
## sdcApp()

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## require("devtools")
## install_github("sdcTools/sdcMicro")

## ----eval=FALSE----------------------------------------------------------
## help.start()

## ----help, eval=FALSE----------------------------------------------------
## help(package=sdcMicro)

## ----eval=FALSE----------------------------------------------------------
## library(sdcMicro)
## ?rankSwap

## ----help2, eval=FALSE---------------------------------------------------
## data(testdata)

## ----eval=FALSE----------------------------------------------------------
## help.search("adding noise")

## ----echo=FALSE----------------------------------------------------------
rm(list=ls())

## ----eval=TRUE-----------------------------------------------------------
apropos("risk")

## ----eval=FALSE----------------------------------------------------------
## RSiteSearch("adding noise")

# ## ------------------------------------------------------------------------
# ls()
# 
# ## ------------------------------------------------------------------------
# getwd()
# 
# ## ----echo=FALSE----------------------------------------------------------
# g <- getwd()
# 
# ## ------------------------------------------------------------------------
# # paste creates a string
# p <- paste(getwd(), "/data", sep="")
# p
# # now change the working directory
# setwd(p)
# # has it changed? Yes...
# getwd()
# 
# ## ----echo=FALSE----------------------------------------------------------
# setwd(g)

## ------------------------------------------------------------------------
v.num <- c(1,3,5.9,7)
v.num
is.numeric (v.num)

## ------------------------------------------------------------------------
v.num > 3

## ----warning=FALSE, message=FALSE----------------------------------------
v1 <- c(1,2,3)
v2 <- c(4,5)
v1 + v2 

## ------------------------------------------------------------------------
v2 <- c (100, TRUE, "A", FALSE)
v2
is.numeric (v2)

## ----eval=TRUE-----------------------------------------------------------
require("laeken")
data("eusilc")
# extract for 10 observations of variable age from eusilc data
age <- eusilc[1:10, "age"] 
age
# positive indexing:
age[c(3,6,7)]
# negative indexing:
age[-c(1,2,4,5,8:10)]
# logical indexing:
age < 15
# a logical expression can be written directly in []
age[age < 15]

## ------------------------------------------------------------------------
## measure risk on data frames
data(Tarragona)
x <- Tarragona[, 5:7]
## add noise
y <- addNoise(x)
## estimate the risk, result is a list
risk <- dRiskRMD(x, xm=y$xm)
class(risk)
str(risk)
names(risk)
## access elements from the named list
risk$wrisk2

## ------------------------------------------------------------------------
## access a vector with the dollar operator
gender <- eusilc$rb090
## first six values:
head(gender)

## ------------------------------------------------------------------------
## babies of age 1 living in households of size 2:
babies1 <- eusilc$age == 1 & eusilc$hsize == 2
str(babies1)
## select some variables, e.g. including 
## family/children related allowances
cn <- colnames(eusilc) %in% c("rb090", "db040", "hy050n")
str(cn)
eusilc[babies1, cn]
## or for short:
eusilc[eusilc$age == 1 & 
         eusilc$hsize == 2, 
       c("rb090", "db040", "hy050n")]
library(dplyr)
detach(package:MASS)
eusilc %>% 
  filter(age == 1 & hsize == 2) %>% 
  select(rb090, db040, hy050n)

## ------------------------------------------------------------------------
sum(is.na(eusilc))

## ------------------------------------------------------------------------
27200 / (nrow(eusilc) * ncol(eusilc)) * 100

## ----aggrfig, echo=TRUE, fig.height=4.5, fig.width=6.5, fig.align='center',out.width='4in'----
require("VIM")
aggr(eusilc)

## ------------------------------------------------------------------------
## how often summary is overloaded with methods 
## on summary for certain classes
length(methods(summary))
class(eusilc$hsize)
summary(eusilc$hsize)
## convert it to a factor
eusilc$hsize <- as.factor(eusilc$hsize)
class(eusilc$hsize)
summary(eusilc$hsize)

data(eusilc)
plot(eusilc$hsize)
head(eusilc$hsize)
class(eusilc$hsize)
eusilc$hsize <- factor(eusilc$hsize)
head(eusilc$hsize)
class(eusilc$hsize)
plot(eusilc$hsize)
class(eusilc$hsize) <- "myclass"
plot(eusilc$hsize)

plot.myclass <- function(x, y, ...){
  print("there is no plot for myclass")
}
plot(eusilc$hsize)


## ------------------------------------------------------------------------
sdc <- createSdcObj(eusilc,
  keyVars=c('age','rb090','db040','hsize','pb220a'),
  numVars=c('eqIncome'), w='rb050')
class(sdc)

## ------------------------------------------------------------------------
slotNames(sdc)

## ----tidy=TRUE, echo=FALSE, results='hide'-------------------------------
str(sdc@risk)
str(sdc@risk$global)
sdc@risk$global$risk_pct

## ----guiopen, eval=FALSE-------------------------------------------------
## require("sdcMicroGUI")
## sdcGUI()

## ----create sdcMicroObj--------------------------------------------------
require("sdcMicro")
data("testdata", package="sdcMicro")
sdc <- createSdcObj(testdata,
          keyVars=c('urbrur','water','sex','age'), 
          numVars=c('expend','income','savings'),
          pramVars=c("walls"), 
          w='sampling_weight', 
          hhId='ori_hid')

## ----slots, echo=TRUE----------------------------------------------------
slotNames(sdc)

## ----show-method---------------------------------------------------------
print(sdc)

## ------------------------------------------------------------------------
microaggregation(testdata[,c("expend","income","savings")])

## ----eval=FALSE----------------------------------------------------------
## sdc <- microaggregation(sdc)

## ----access, tidy=FALSE--------------------------------------------------
## data utility:
ut <- slot(sdc, "utility")
## index of categorical key variables:
cat <- slot(sdc, "keyVars")
## key variables, orginal data
head(testdata[, cat], 3)

## ----linewidth=72, tidy=FALSE--------------------------------------------
dat <- extractManipData(sdc)
str(dat)

## ----print risk, linewidth=72--------------------------------------------
print(sdc, "risk")

## ----print methods, results="hide", eval=FALSE---------------------------
## print(sdc)
## print(sdc, "ls")
## print(sdc, type="recode")
## print(sdc, type="risk")
## print(sdc, type="numrisk")
## print(sdc, type="pram")

## ----linewidth=72--------------------------------------------------------
args(report)

## ----eval=FALSE----------------------------------------------------------
## sdcApp()

