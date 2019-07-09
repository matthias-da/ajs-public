# Tasks:

# Question 2.2 Choice of variables (II)
# Please have a brief look at another popular 
# data set, the Structural Earnings Statistics in 
# R-package laeken (Alfons and Templ 2013). 
# Read the help for this data set by typing in R:
#   

library(laeken)
data(ses) 
?ses
View(ses)
str(ses)

# Determine which of the variables 
# should be defined as (a) direct identifiers (if any)
# (b) categorical key variables
# (c) continuous key variables
# (d) sensitive variables

keys <- c("location","age", 
          "sex", "education",
          "NACE1","economicFinanc")

table(ses$size)
hist(ses$earnings)
boxplot(ses$earnings)
ses[which.max(ses$earnings),]

# Question 2.3 Frequencies of key variables
# Use again the EU-SILC data set from package laeken.
# Is a re-identification of observation 8 
# possible assuming region (db040), age, 
# gender (rb090) and economic status (pl030) 
# as categorical key variables. 
# Is re-identification of observation 3 easily be possible?

?createSdcObj
?eusilc
keyVars = c("db040", "age", "rb090", "pb220a") # one possible scenario
sdc <- createSdcObj(eusilc, 
                    keyVars = keyVars,
                    weightVar = "rb050",
                    hhId = "db030",
                    alpha = 0.5) 
sum(is.na(eusilc[, keyVars]))
apply(eusilc[, keyVars], 2, 
      function(x) sum(is.na(x)))
# alpha: how missings are treated. Irrelevant here, 
# because we do not have missings in the key 
# variables of eusilc.
sdc
slotNames(sdc)
slot(sdc, "risk")
# or by using the @ operator
head(sdc@risk$individual)
sdc@risk$individual[, "fk"]
# observation 8:
sdc@risk$individual[c(3,8), ]
eusilc[c(3,8), ]

# Inferential disclosure
# Question: estimate the income on py010

mod <- lm(log(py010n) ~ age + rb090 + pl030 + I(age^2) + pb220a,
          data = eusilc[eusilc$py010n > 0, ])
summary(mod)

intrudersKnowledge <- data.frame("hsize" = 3, 
                                 "db040" = "Tyrol",
                                 "age"= 34,
                                 "pl030" = "2",
                                 "pb220a" = "AT",
                                 "rb090" = "male",
                                 "eqIncome" = 16090)
exp(predict(mod, newdata = intrudersKnowledge,
            interval = "prediction"))


# Question:
# Create a sdcObj and access and plot the 
# estimated pop. freq. counts Fk.
# Plot the individual risk.

class(sdc)
library(ggplot2)
risk <- data.frame(sdc@risk$individual)
ggplot(risk, aes(x = Fk)) + geom_histogram()
ggplot(risk, aes(x = risk)) + geom_histogram()

# Question:
# Now use the ses data, 
# define a disclosure scenario and report on 
# k-anonymity, suda scores and individual risk

?sdcApp
# sdcApp()
data(ses, package = "laeken")
keys <- c("location","age", 
          "sex", "education",
          "NACE1","economicFinanc")
sdc_ses <- createSdcObj(ses,
                        keyVars = keys,
                        weightVar = "weights",
                        hhId = "IDunit"
                        )
sdc_ses
print(sdc_ses, "risk")
sdc_ses <- suda2(sdc_ses)
names(sdc_ses@risk)
sdc_ses@risk$suda2
risk <- data.frame(sdc_ses@risk$individual)
ggplot(risk, aes(x = Fk)) + geom_histogram()
ggplot(risk, aes(x = risk)) + geom_histogram()

r <- risk[, "risk"]
ses[which.max(r), keys]
ses[which.max(r), ]
