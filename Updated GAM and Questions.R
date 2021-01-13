df <- read.csv('mdata.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')

base <- gam(trans_pf_per ~ te(Latitude.x,k=20,bs='cr') + te(Longitude.y,k=35,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
            family = ziP(), data = df, method = "REML")
FitList <- 
  unique(df[,c("trans_pf_per", "Latitude.x", "Longitude.y")]) %>% 
  mutate(Month = df$Month[[5]])
FitPredictions <- predict.gam(base,
                                   newdata = FitList,
                                   se.fit = T)
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList %>% ggplot(aes(trans_pf_per, exp(Fit))) + geom_point() + geom_smooth(method = lm) + theme_minimal()
## 6 values with really poor fits? 

FitList %>% ggplot(aes(Latitude.x, Longitude.y, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Bad points are at extreme end of long range


mod1 <- gam(trans_pf_per ~ te(ALAN.trans,k=21) + te(Precip) + ti(ALAN.trans, Precip) + 
              s(Latitude.x,k=40,bs='cr') + s(Longitude.y,k=50,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
              family = ziP(), data = df, method = "REML")
FitList <- 
  unique(df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "Precip", "ALAN.trans")]) %>% 
  mutate(Month = df$Month[[5]])
FitPredictions <- predict.gam(mod1,
                              newdata = FitList,
                              se.fit = T)
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList %>% ggplot(aes(trans_pf_per, exp(Fit))) + geom_point() + geom_smooth(method = lm)
## Still a couple points that are really bad


FitList %>% ggplot(aes(Latitude.x, Longitude.y, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Similar but better


sapply(list(base,mod1), deviance)
sapply(list(base,mod1), AIC)
## Overall fit is a lot better though


FitList %>% ggplot(aes(Precip, ALAN.trans, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Poor fit is at weird sites with lots of ALAN and no Precip


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "Precip", "ALAN.trans", 'Month')]
FitPredictions <- predict.gam(mod1,
                              newdata = FitList,
                              se.fit = T,
                              type = 'response')
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList <- FitList %>% subset(Month == 5)
FitList %>% ggplot(aes(Precip, ALAN.trans, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Hotspot at low precip/high alan?
## How would I create a heatmap with this data like below?


visreg2d(mod1, xvar = 'Precip', yvar = 'ALAN.trans', scale = 'response', cond = list(Month=5))
## Doesn't match though


mod2 <- gam(trans_pf_per ~ s(AvgTemp, k=100) +  
              s(Latitude.x,k=40,bs='cr') + s(Longitude.y,k=50,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
            family = ziP(), data = df, method = "REML")
gam.check(mod2)
## The s(AvgTemp) term will not work

mod3 <- gam(trans_pf_per ~ te(AvgTemp, k=25) + te(ALAN.trans, k=25) + ti(AvgTemp, ALAN.trans) +  
              s(Latitude.x,k=40,bs='cr') + s(Longitude.y,k=50,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
            family = ziP(), data = df, method = "REML")
mod4 <- gam(trans_pf_per ~ te(AvgTemp, k=15) + te(Precip, k=15) + ti(AvgTemp, Precip) +  
              s(Latitude.x,k=50,bs='cr') + s(Longitude.y,k=50,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
            family = ziP(), data = df, method = "REML")

sapply(list(base,mod1,mod2,mod3,mod4), deviance)
sapply(list(base,mod1,mod2,mod3,mod4), AIC)
## Temp/Prec model is slightly better than Prec/ALAN (I have other more advanced models later..)


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "ALAN.trans", 'Month')]
FitPredictions <- predict.gam(mod3,
                              newdata = FitList,
                              se.fit = T)
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList %>% ggplot(aes(trans_pf_per, exp(Fit))) + geom_point() + geom_smooth(method = lm) + theme_minimal()
FitList %>% ggplot(aes(Latitude.x, Longitude.y, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Looks really good (?)


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "ALAN.trans", 'Month')]
FitPredictions <- predict.gam(mod3,
                              newdata = FitList,
                              se.fit = T,
                              type = 'response')
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList <- FitList %>% subset(Month == 5)
FitList %>% ggplot(aes(AvgTemp, ALAN.trans, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Hotspot at highish alan and low temp


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "Precip", 'Month')]
FitPredictions <- predict.gam(mod4,
                              newdata = FitList,
                              se.fit = T)
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList %>% ggplot(aes(trans_pf_per, exp(Fit))) + geom_point() + geom_smooth(method = lm) + theme_minimal()
FitList %>% ggplot(aes(Latitude.x, Longitude.y, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()
## Looks even better


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "Precip", 'Month')]
FitPredictions <- predict.gam(mod4,
                              newdata = FitList,
                              se.fit = T,
                              type = 'response')
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList <- FitList %>% subset(Month == 5)
FitList %>% ggplot(aes(AvgTemp, Precip, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()


mod5 <- gam(trans_pf_per ~ s(ALAN.trans, k=30) + te(AvgTemp, k=15) + te(Precip, k=15) + ti(ALAN.trans, AvgTemp) +
              ti(ALAN.trans,Precip) + ti(AvgTemp, Precip) +  
              s(Latitude.x,k=45,bs='cr') + s(Longitude.y,k=50,bs='cr') +
              ti(Latitude.x, Longitude.y, Month, bs = c('cr','fs'), d=c(2,1), k=c(40,12)),
            family = ziP(), data = df, method = "REML")
sapply(list(base,mod1,mod2,mod3,mod4,mod5), deviance)
sapply(list(base,mod1,mod2,mod3,mod4,mod5), AIC)
anova(mod4,mod5, test = 'Chisq')
## By far the best yet


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "Precip", 'ALAN.trans', 'Month')]
FitPredictions <- predict.gam(mod5,
                              newdata = FitList,
                              se.fit = T)
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList %>% ggplot(aes(trans_pf_per, exp(Fit))) + geom_point() + geom_smooth(method = lm) + theme_minimal()
FitList %>% ggplot(aes(Latitude.x, Longitude.y, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal()


FitList <- 
  df[,c("trans_pf_per", "Latitude.x", "Longitude.y", "AvgTemp", "Precip", 'ALAN.trans', 'Month')]
FitPredictions <- predict.gam(mod5,
                              newdata = FitList,
                              se.fit = T,
                              type = 'response')
FitList[,c("Fit", "Lower", "Upper")] <-
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))
FitList <- FitList %>% subset(Month == 5)
FitList %>% ggplot(aes(ALAN.trans, Fit, color=Fit)) + geom_point(size=4) + 
  scale_color_viridis_c() + theme_minimal() + geom_smooth(method = 'loess')
