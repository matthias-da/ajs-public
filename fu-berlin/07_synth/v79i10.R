set.seed(1234)

## ----Section 4.1. loadSilc------
library("simPop")
data("eusilcS", package = "simPop")
origData <- eusilcS
dim(origData)

## ----Section 4.1. household size---------------
length(unique(origData$db030))

## ----Section 4.2. dataObj-------------------------
inp <- specifyInput(origData, hhid = "db030", hhsize = "hsize", 
                    strata = "db040", weight = "rb050")

## ----Section 4.2. print dataObj------------------------------------------
print(inp)

## ----Section 4.3. load data------
data("totalsRGtab", package = "simPop")

## ----Section 4.3. load data 2--------------------------------
data("totalsRG", package = "simPop")
print(totalsRG)

## ----Section 4.3. print totals-------
data("totalsRGtab", package = "simPop")
print(totalsRGtab)

## ----Section 4.3. ipft---------------------------------------
totalsRG$Freq <- totalsRG$Freq / 100
totalsRGtab <- totalsRGtab / 100

weights.df <- calibSample(inp, totalsRG)
weights.tab <- calibSample(inp, totalsRGtab)

identical(weights.df, weights.tab)

## ----Section 4.3. addweights---------------------------------
addWeights(inp) <- calibSample(inp, totalsRGtab)

## ----Section 4.4. simStructure-------------------------------------------
synthP <- simStructure(data = inp, 
                       method = "direct", 
                       basicHHvars = c("age", "rb090", "db040"))

## ----Section 4.5. simStructure--------------------------------
synthP <- simStructure(data = inp, method = "direct",
  basicHHvars = c("age", "rb090", "db040"))

## ----Section 4.5. simCategorical----------------
synthP <- simCategorical(synthP, 
                         additional = c("pl030", "pb220a"), 
                         method = "multinom",
                         nr_cpus = 1)

## ----Section 4.5. print object---------------------------
print(synthP)

## ----Section 4.6. simContinuous-------------------
synthP <- simContinuous(synthP, additional = "netIncome",
                        upper = 200000, equidist = FALSE,
                        imputeMissings = FALSE,
                        nr_cpus = 1)

## stop here

## ----Section 4.6. children------------------------
ageinc <- pop(synthP, var = c("age", "netIncome"))
ageinc$age <- as.numeric(as.character(ageinc$age))
ageinc[age < 16, netIncome := NA]
pop(synthP, var = "netIncome") <- ageinc$netIncome

## ----Section 4.6. simContinuous netIncome--------------------
## synthP <- simContinuous(synthP, additional = "netIncome", method = "lm",
##   nr_cpus = 1)

## ----Section 4.7. manageSimPopObj---------------
sIncome <- manageSimPopObj(synthP, var = "netIncome", sample = TRUE)
sWeight <- manageSimPopObj(synthP, var = "rb050", sample = TRUE)
pIncome <- manageSimPopObj(synthP, var = "netIncome")
breaks <- getBreaks(x = sIncome, w = sWeight, upper = Inf, 
                     equidist = FALSE)
synthP <- manageSimPopObj(synthP, var = "netIncomeCat", 
                           sample = TRUE, set = TRUE, 
                           values = getCat(x = sIncome, breaks))
synthP <- manageSimPopObj(synthP, var = "netIncomeCat", 
                           sample = FALSE, set = TRUE,
                           values = getCat(x = pIncome, breaks))

## ----Section 4.7. simComponents-----------------
synthP <- simComponents(simPopObj = synthP, total = "netIncome",
                         components = c("py010n", "py050n", "py090n", 
                         "py100n", "py110n", "py120n", "py130n", "py140n"), 
                         conditional = c("netIncomeCat", "pl030"),
                         replaceEmpty = "sequential", seed = 1)

## ----Section 4.7. simComponents print----
synthP

## ----Section 4.8. simspatial---------------------------------
simulate_districts <- function(inp) {
  hhid <- "db030"
  region <- "db040"
  a <- inp[!duplicated(inp[, hhid]), c(hhid, region)]
  spl <- split(a, a[, region])
  regions <- unique(inp[, region])

  tmpres <- lapply(1:length(spl), function(x) {
    codes <- paste(x, 1:sample(10:90, 1), sep = "")
    spl[[x]]$district <- sample(codes, nrow(spl[[x]]), replace = TRUE)
    spl[[x]]
  })
  tmpres <- do.call("rbind", tmpres)
  tmpres <- tmpres[, -2]
  out <- merge(inp, tmpres, by.x = hhid, by.y = hhid, all.x = TRUE)
  invisible(out)
}
data("eusilcS", package = "simPop")
census <- simulate_districts(eusilcS)
head(table(census$district))

## ----Section 4.8. simSpat2-----------------------
tabHH <- as.data.frame(xtabs(rb050~ db040 + district, data = census[!duplicated(census$db030),]))
tabP <- as.data.frame(xtabs(rb050~ db040 + district, data = census))
colnames(tabP) <- colnames(tabHH) <- c("db040", "district", "Freq")
## ----Section 4.8. simSpat3-------------------------------
synthP <- simInitSpatial(synthP, additional = "district",
                         region = "db040", tspatialHH = tabHH, tspatialP = tabP, nr_cpus = 1)
head(popData(synthP), 2)

## ----Section 4.9. eusilcPload----
census <- simStructure(data = inp, method = "direct",
                        basicHHvars = c("age", "rb090", "db040"))
census <- simCategorical(census, 
                         additional = c("pl030", "pb220a"), 
                         method = "multinom",
                         nr_cpus = 1)

## ----Section 4.9. eusilcPload2----
census <- data.frame(popData(census))
margins <- as.data.frame(xtabs(~ db040 + rb090 + pl030, data = census))
margins$Freq <- as.numeric(margins$Freq)
synthP <- addKnownMargins(synthP, margins)

## ----Section 4.9. simulatedAnnealing----
synthPadj <- calibPop(synthP, split = "db040", temp = 1, 
                      eps.factor = 0.00005, maxiter = 200, 
                      temp.cooldown = 0.975, factor.cooldown = 0.85,
                      min.temp = 0.001, verbose = TRUE, nr_cpus = 1)

## ----Section 4.9. checkPop-------------------------
pop <- data.frame(popData(synthP))
popadj <- data.frame(popData(synthPadj))

## ----Section 4.9. checkPop2------------------------
tab.census <- ftable(census[, c("rb090", "db040", "pl030")])
tab_afterSA <- ftable(popadj[, c("rb090", "db040", "pl030")])
tab.census - tab_afterSA

## ----Section 5.1. univariate---------------------------------------------
dat <- data.frame(sampleData(synthP))
tableWt(dat$pl030, weights = dat$rb050)

## ----Section 5.1. univariate2--------------------------------------------
table(popData(synthP)$pl030)

## ----Section 5.1. plotMosaic-----------------
tab <- spTable(synthP, select = c("rb090", "db040", "hsize"))
spMosaic(tab, labeling = labeling_border(abbreviate = c(db040 = TRUE)))

## ----Section 5.1. plotMosaic2----------------
tab <- spTable(synthP, select = c("rb090", "pl030"))
spMosaic(tab, method = "color")

## ----Section 5.1. cdfs----
spCdfplot(synthP, "netIncome", cond = "rb090", layout = c(1, 2))

## ----Section 5.1. box-----------
spBwplot(synthP, x = "netIncome", cond = "rb090", layout = c(1, 2))

## ----Section 5.1. cdfs2----
spCdfplot(synthP, "netIncome", cond = "db040", layout = c(3, 3))

## ----Section 5.1. coefficients----

myPlot <- function(lm1,lm2,scale=TRUE){
    s1 <- confint(lm1)
    p1 <- summary(lm1)$coefficients[, 1]
    sig1 <- as.logical(summary(lm1)$coef[, 4] < 0.05)
    s2 <- confint(lm2)
    p2 <- summary(lm2)$coefficients[, 1]
    sig2 <- summary(lm1)$coef[, 4] < 0.05
    ## scaled
    if(scale){
      p1 <- scale(p1, center = 270.7126, 760.8413)
      p2 <- scale(p2, center = 270.7126, 760.8413)
      s1 <- scale(s1, center = c(270.7126, 270.7126), c(760.8413, 760.8413))
      s2 <- scale(s2, center = c(270.7126, 270.7126), c(760.8413, 760.8413))
    }
    ## without intersept
    p1 <- p1[2:length(p1)]
    p2 <- p2[2:length(p2)]
    s1 <- s1[2:length(p1), ]
    s2 <- s2[2:length(p2), ]
    ylims <- c(min(c(s1, s2)), max(c(s1, s2)))
    par(mar = c(5,5,0,0))
    plot(x = (1:length(p1)), y = p1, pch = 1, 
         ylab = expression(beta[i]), xlab = "index of regression coefficients", 
         cex.lab = 1.4, type = "n", ylim = ylims)
    points(x = (1:length(p1))[!sig1], y = p1[!sig1], cex.lab = 1.4)
    points(x = (1:length(p1))[sig1], y = p1[sig1], cex.lab = 1.8, pch = 18)
    points(x = 1:length(p2)+0.2, y = p2, col = "gray", pch = 20)
    segments(x0 = 1:length(p1), x1 = 1:length(p1), y0 = s1[, 1], y1 = s1[, 2])
    abline(h = 0, col = "gray", lty = 1)
    legend("topleft", legend = c("original - significant", "original - non-significant", 
               "simulated population"), lty = c(1,1,1),
               pch = c(18,1,20), col = c("black", "black", "gray"))
}
samp <- data.frame(sampleData((synthP)))
form <- formula("netIncome ~ age + rb090 + db040 + pb220a +pl030 + hsize+ py010n + py050n + py090n + py100n")
lm1 <- lm(form, data=samp, weights=samp$rb050)
pop <- data.frame(popData(synthP))
pop$age <- as.numeric(pop$age)
lm2 <- lm(form, data=pop)
myPlot(lm1, lm2, scale = FALSE)
