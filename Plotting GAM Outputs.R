
# Plotting GAM outputs ####

library(tidyverse)

Model <- working2

# Effects ####

LatRange <- seq(from = min(df$Latitude.x),
                to = max(df$Latitude.x),
                length = 101)  %>% 
  c(mean(df$Latitude.x))

LonRange <- seq(from = min(df$Longitude.y),
                to = max(df$Longitude.y),
                length = 101)  %>% 
  c(mean(df$Longitude.y))

FitList <- expand.grid(#HRO = HRORange,
  Longitude.y = LonRange,
  Latitude.x = LatRange,
  Month = df$Month %>% unique %>% sort
)

FitPredictions  <- predict.gam(Model, 
                               newdata = FitList, 
                               se.fit = T)

FitList[,c("Fit","Lower", "Upper")] <- 
  with(FitPredictions, cbind(fit, fit - se.fit, fit + se.fit))

# Plotting the model effects ####

FitList %>%
  filter_at(c("Longitude.y", "Latitude.x"), ~.x != last(.x)) %>% 
  ggplot(aes(Longitude.y, Latitude.x)) + 
  geom_tile(aes(fill = Fit)) +
  labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = df) +
  facet_wrap(~Month)
