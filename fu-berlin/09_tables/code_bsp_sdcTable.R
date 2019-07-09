require(sdcTable)
require(dplyr)
d_nace <- data.frame(
 levels=c('@','@@',rep('@@@',3), "@@","@@@",rep('@@@@',3),rep('@@@',2)),
 codes=c("Total",'55','55.1','55.2','55.3', '56','56.1','56.11','56.12','56.13','56.2','56.3'),
 stringsAsFactors=FALSE)
d_region <- data.frame(
 levels=c('@',rep('@@',3)),
 codes=c("Total",'R1','R2','R3'),
 stringsAsFactors=FALSE)

dat <- expand.grid(nace=d_nace$codes, region=d_region$codes) %>%
  mutate(nace=as.character(nace), region=as.character(region), val=NA)

dat <- filter(dat, region!="Total" & !nace %in% c("Total","55","56.1","56"))
dat$val <- c(20,8,17,9,4,27,2,20,
             50,19,32,28,7,15,20,30,
             10,22,12,5,6,9,18,25)

dims <- list(nace=d_nace, region=d_region)
sdcProb <- makeProblem(dat, dimList=dims, dimVarInd=1:2, freqVarInd=3)
sdcProb@problemInstance@sdcStatus[c(16,33,34,35,27,42)] <- "u"
# Primaerunterdrueckung

dt <- sdcTable:::g_df(sdcProb)
dt

res <- protectTable(sdcProb, method="OPT", useC=TRUE)
filter(res@finalData, sdcStatus=="x")

fd <- res@finalData
fd[,ii:=0]
fd[sdcStatus=="u", ii:=1]
fd[sdcStatus=="x", ii:=2]
xtabs(ii~nace+region, data=fd)

