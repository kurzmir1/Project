install.packages("devtools") # <- if you havent installed devtools already
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")

library(ComputationalMovementAnalysisData)

head(wildschwein_BE)

ComputationalMovementAnalysisData::wildschwein_BE

wildschwein_BE

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(zoo)

load(file='wildschwein_BE.rda')

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

##Calculate steplength

wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

##Calculate timelag
wildschwein_BE<-wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))


##Calculate speed
wildschwein_BE <- wildschwein_BE %>%
  mutate(speed=steplength/timelag)

##Calculate mean speed for different rolling windows
wildschwein_BE_win <- wildschwein_BE
wildschwein_BE_win$k4 <- rollmean(wildschwein_BE_win$speed, k=4, fill = NA, align = "left")
wildschwein_BE_win$k8 <- rollmean(wildschwein_BE_win$speed, k=8, fill = NA, align = "left")
wildschwein_BE_win$k12 <- rollmean(wildschwein_BE_win$speed, k=12, fill = NA, align = "left")
wildschwein_BE_win$k16 <- rollmean(wildschwein_BE_win$speed, k=16, fill = NA, align = "left")
wildschwein_BE_win$k20 <- rollmean(wildschwein_BE_win$speed, k=20, fill = NA, align = "left")

##k4 values below 0.1 = static

wildschwein_BE_win <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static = k4 < 0.1)

##Filter for K4 static stretches
wildschwein_BE_static <- wildschwein_BE_win %>%
  filter(static)


##Visualize static stretches (k4) per animal
ggplot(data = wildschwein_BE_win, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=static))+
  coord_equal()+
  facet_wrap(~ TierName, ncol = 5)

##K20 values below 0.1 = static
wildschwein_BE_win <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(staticK20 = k20 < 0.1)

##Filter for K20 static stretches
wildschwein_BE_staticK20 <- wildschwein_BE_win %>%
  filter(staticK20)


##Visualize filtered K20 static stretches
ggplot(data = wildschwein_BE_staticK20, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=staticK20))+
  coord_equal()+
  facet_wrap(~ TierName, ncol = 5)

##K4 values below 0.01
wildschwein_BE_win <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(verystatic = k4 < 0.01)


##K8 values below 0.01
wildschwein_BE_win <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(verystaticK8 = k8 < 0.01)

##Visualize very static stretches (k8) per animal
ggplot(data = wildschwein_BE_win, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=verystaticK8))+
  coord_equal()+
  facet_wrap(~ TierName, ncol = 5)

##Filter for K8 very static stretches
wildschwein_BE_verystaticK8 <- wildschwein_BE_win %>%
  filter(verystaticK8)

##visualize filtered very static K8 stretches
ggplot(data = wildschwein_BE_staticK20, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=staticK20))+
  coord_equal()+
  facet_wrap(~ TierName, ncol = 5)

##Filter for K8 very static stretches of Ueli
Ueli_verystaticK8 <- wildschwein_BE_verystaticK8 %>%
  filter(TierName == "Ueli")

##FInding the most static points of Ueli (0.001)
Ueli_verystaticK8<- Ueli_verystaticK8 %>% 
  ungroup() %>%
  mutate(moststaticK8 = k8 < 0.001)

##visualize most static points of Ueli
ggplot(data = Ueli_verystaticK8, mapping = aes(x= E, y=N) )+
  geom_path()+
  geom_point(aes(colour=moststaticK8))+
  coord_equal()
