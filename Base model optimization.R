### load data and packages ###

# setwd("C:/Users/kylek/Dropbox/RegWork/Malaria")

df <- read.csv('mdata_concise.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')
library(mgcv); library(dplyr); library(ggplot2); library(ggregplot); library(magrittr)
set.seed(888)

## base model with single smooth for space and month as RE ##
bm.gam <- gam(Pf_per ~ s(Month, bs = "re") + s(Latitude.x, Longitude.y), data = df, method = "REML")

## trying our different smoothing terms on the spatial variable ##

s2 <- gam(Pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y), data = df, method = "REML")
s1s2 <- gam(Pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) +  s(Latitude.x, Longitude.y), data = df, method = "REML")
s2ti <- gam(Pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) + ti(Latitude.x, Longitude.y), data = df, method = "REML")
te <- gam(Pf_per ~ s(Month, bs = "re") + te(Latitude.x, Longitude.y), data = df, method = "REML")
t2 <- gam(Pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) + t2(Latitude.x, Longitude.y), data = df, method = "REML")

## compare deviance ##

deviance(bm.gam)
deviance(s2)
deviance(s1s2)
deviance(s2ti)
deviance(te)
deviance(t2)

## s1s2 seems to fit best ##

## view model fits and performance ##

summary(s1s2)
## only 58.7 deviance explained ##

gam.check(s1s2)
## spatial terms are significant and many 0 values are being overestimated ##

## try zero-inflated poisson model instead?? ##

## transform response variable to integers ##

trans.Pf.per <- df$Pf_per*100
df$trans_pf_per <- round(trans.Pf.per)

## fit ziP model with best working smoother ##

working.gam <- working <- gam(trans_pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) +  s(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
deviance(working.gam)
## = 6108 ... dont really know if that is good or bad ... ##

summary(working.gam)
## 74.2% of deviance explained ##

gam.check(working.gam)
## seems to fit better ##

## re-check with the different smoothing terms ##
s1 <- gam(trans.Pf.per ~ s(Month, bs = "re") + s(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
s2 <- gam(trans.Pf.per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y), family = ziP(), data = df, method = "REML")
s1s2 <- gam(trans.Pf.per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) +  s(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
s2ti <- gam(trans.Pf.per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) + ti(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
te <- gam(trans.Pf.per ~ s(Month, bs = "re") + te(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
t2 <- gam(trans.Pf.per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) + t2(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

## now the t2 term is the best fitting! ##
working <- t2
gam.check(working)

## spatial terms are still significant ... mess with the k parameter ##
working <- gam(trans.Pf.per ~ 
                 as.factor(Month) +
                 # s(Month, bs = "re") + 
                 s(Latitude.x, k = 40) + s(Longitude.y, k = 40) +
                 t2(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")
deviance(working)
## deviance down a lot ##
summary(working)
## 80.6 deviance explained ##
gam.check(working)
## looking MUCH better ##

## smallest K terms with no significance ##
working <- gam(trans_pf_per ~ s(Month, bs = "re") + s(Latitude.x) + s(Longitude.y) +  s(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

working2 <- gam(trans_pf_per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y) +  s(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

working3 <- gam(trans_pf_per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y) +  te(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

working4 <- gam(trans_pf_per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y) +  ti(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

working5 <- gam(trans_pf_per ~ as.factor(Month) + t2(Latitude.x, Longitude.y), family = ziP(), data = df, method = "REML")

sapply(list(working, working2, working3, working4, working5), deviance)

# Adding model addition ####

AddCovar <- c("s(Longitude.y)", "Longitude.y", 
              "s(Latitude.x)", "Latitude.x",
              "ti(Latitude.x, Longitude.y)",
              "s(Latitude.x, Longitude.y)",
              "te(Latitude.x, Longitude.y)",
              "t2(Latitude.x, Longitude.y)",
              "s(Latitude.x, Longitude.y, bs = 'ad')"
)

ClashList <- list(AddCovar[1:2],
                  AddCovar[1:2+2],
                  AddCovar[5:7],
                  AddCovar[c(1:4, 6:7)])

df %<>% mutate_at("Month", as.factor)

BAM1 <- BAMModelAdd(
  
  Response =  "trans.pf.per",
  Data = df,
  Explanatory = "Month",
  Add = AddCovar,
  Clashes = ClashList,
  Family = ziP()
  
)
