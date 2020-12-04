## Prepare Environment ##

library(ggplot2); library(mgcv); library(dplyr); library(tidyverse)
df <- read.csv('mdata_concise.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')
set.seed(888)
trans.pf.per <- df$Pf_per*100
df$trans.pf.per <- round(trans.pf.per)
LatRange <- seq(from = min(df$Latitude.x),
                to = max(df$Latitude.x),
                length = 101) %>%
  c(mean(df$Latitude.x))

LonRange <- seq(from = min(df$Longitude.y),
                to = max(df$Longitude.y),
                length = 101) %>%
  c(mean(df$Longitude.y))

FitList <- expand.grid(#HRO = HRORange,
  Longitude.y = LonRange,
  Latitude.x = LatRange,
  Month = df$Month %>% unique %>% sort
)



## Singular S terms and a Ti ##
model.s.ti <- gam(trans.pf.per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y) + ti(Latitude.x, Longitude.y),
                family = ziP(), data = df, method = "REML")

FitPredictions.s.ti <- predict.gam(model.s.ti,
                                   newdata = FitList,
                                   se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions.s.ti, cbind(fit, fit - se.fit, fit + se.fit))
fit.s.ti <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)

         ## Adjusting K ##
model.s.ti.k <- gam(trans.pf.per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y) + ti(Latitude.x, Longitude.y, k = 17),
             family = ziP(), data = df, method = "REML")

FitPredictions <- predict.gam(model.s.ti.k,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.s.ti.k <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)

## Just Ti
model.ti <- gam(trans.pf.per ~ as.factor(Month) + ti(Latitude.x, Longitude.y),
             family = ziP(), data = df, method = "REML")

FitPredictions <- predict.gam(model.ti,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.ti <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)
 
         ## Adjusting K ##
model.ti.k <- gam(trans.pf.per ~ as.factor(Month) + ti(Latitude.x, Longitude.y, k = 15),
             family = ziP(), data = df, method = "REML")

FitPredictions <- predict.gam(model.ti.k,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.ti.k <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)


## Just T2 ##
model.t2 <- gam(trans.pf.per ~ as.factor(Month) + t2(Latitude.x, Longitude.y), 
                family = ziP(), data = df, method = "REML")

FitPredictions <- predict.gam(model.t2,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.t2 <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)

        ## Cannot get T2 K terms to fall into non-significance ##

## A simple spatial model for comparisons ##
model.s <- gam(trans.pf.per ~ as.factor(Month) + s(Latitude.x) + s(Longitude.y), 
                  family = ziP(), data = df, method = "REML")
FitPredictions <- predict.gam(model.s,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.s <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)

         ## Adjusting K ##
model.s.k <- gam(trans.pf.per ~ as.factor(Month) + s(Latitude.x, k = 7) + s(Longitude.y, k = 77), 
                  family = ziP(), data = df, method = "REML")
FitPredictions <- predict.gam(model.s.k,
                              newdata = FitList,
                              se.fit = T)

FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
fit.s.k <- FitList %>% filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>%
  ggplot(aes(Longitude.y, Latitude.x)) +
  geom_tile(aes(fill = Fit)) + labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)

## Compare ##
deviance(model.s.ti)
deviance(model.s.ti.k)
deviance(model.ti)
deviance(model.ti.k)
deviance(model.t2)
deviance(model.s)
deviance(model.s.k)

## Deviance points to the model with both individual s and a Ti term to be the best, which also required little k hacking

fit.s.ti
fit.s.ti.k
fit.ti
fit.ti.k
fit.t2
fit.s
fit.s.k

## Fit ti.k seems to work best, but I am not quite sure how to interpret these.
