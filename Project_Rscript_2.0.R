install.packages("devtools") # <- if you havent installed devtools already
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")

library(ComputationalMovementAnalysisData)

ComputationalMovementAnalysisData::wildschwein_BE


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

ggplot(data = wildschwein_BE, mapping = aes(steplength))+
      geom_histogram(binwidth = 1)+
      xlim(0,500)
      ylim(0,50000)
      
mean(wildschwein_BE$steplength, na.rm=TRUE)
      
median(wildschwein_BE$steplength, na.rm=TRUE)
    

##Calculate timelag
wildschwein_BE<-wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs")))



##Calculate speed
wildschwein_BE <- wildschwein_BE %>%
  mutate(speed=steplength/timelag)

mean(wildschwein_BE$speed, na.rm=TRUE)
median(wildschwein_BE$speed, na.rm=TRUE)


##calculate mean distance for different rolling windows
wildschwein_BE_win <- wildschwein_BE
wildschwein_BE_win$k4 <- rollmean(wildschwein_BE$steplength, k=4, fill = NA, align = "left")
wildschwein_BE_win$k8 <- rollmean(wildschwein_BE$steplength, k=8, fill = NA, align = "left")
wildschwein_BE_win$k12 <- rollmean(wildschwein_BE$steplength, k=12, fill = NA, align = "left")
wildschwein_BE_win$k16 <- rollmean(wildschwein_BE$steplength, k=16, fill = NA, align = "left")
wildschwein_BE_win$k20 <- rollmean(wildschwein_BE$steplength, k=20, fill = NA, align = "left")

##Make values below 10 static, in new dataframes for each k
wildschwein_BE_win_k4 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k4_10 = k4 < 10)

wildschwein_BE_win_k8 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k8_10 = k8 < 10)

wildschwein_BE_win_k12 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k12_10 = k12 < 10)

wildschwein_BE_win_k16 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k16_10 = k16 < 10)

wildschwein_BE_win_k20 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k20_10 = k20 < 10)

##Function for making segments
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

##k4
##Making segments
wildschwein_BE_win_k4 <-wildschwein_BE_win_k4 %>%
  mutate(
    segment_ID = rle_id(static_k4_10)
  )

##Calculate time span of each static segment
wildschwein_BE_win_k4_segments <- wildschwein_BE_win_k4 %>%
  st_drop_geometry() %>%
  filter(static_k4_10 == "TRUE") %>%
  group_by(TierID, segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##k8
##Making segments
wildschwein_BE_win_k8 <-wildschwein_BE_win_k8 %>%
  mutate(
    segment_ID = rle_id(static_k8_10)
  )

##Calculate time span of each static segment
wildschwein_BE_win_k8_segments <- wildschwein_BE_win_k8 %>%
  st_drop_geometry() %>%
  filter(static_k8_10 == "TRUE") %>%
  group_by(TierID, segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##k12
##Making segments
wildschwein_BE_win_k12 <-wildschwein_BE_win_k12 %>%
  mutate(
    segment_ID = rle_id(static_k12_10)
  )

##Calculate time span of each static segment
wildschwein_BE_win_k12_segments <- wildschwein_BE_win_k12 %>%
  st_drop_geometry() %>%
  filter(static_k12_10 == "TRUE") %>%
  group_by(TierID, segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%  
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##k16
##Making segments
wildschwein_BE_win_k16 <-wildschwein_BE_win_k16 %>%
  mutate(
    segment_ID = rle_id(static_k16_10)
  )

##Calculate time span of each static segment
wildschwein_BE_win_k16_segments <- wildschwein_BE_win_k16 %>%
  st_drop_geometry() %>%
  filter(static_k16_10 == "TRUE") %>%
  group_by(TierID, segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%  
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##Add the timespan of rolling window to the time span of each segment
wildschwein_BE_win_k12_segments <-wildschwein_BE_win_k12_segments %>% 
  mutate (timediff_plusrollingwindow = timediff + 180)

##Calculate mean static time per animal
mean_static_animal <- wildschwein_BE_win_k12_segments %>% 
  group_by(TierID) %>%
  summarise_at(vars(timediff_plusrollingwindow), list(name = mean))

##Filter for static k12 segments 
wildschwein_BE_win_k12_static <- wildschwein_BE_win_k12 %>%
  filter(static_k12_10 == "TRUE")

##Visualizing during what daytime the static segments are
day <- table(wildschwein_BE_win_k12_static$day)
barplot(day)


##############Habitat stuff
##Reading in habitat data
crop_fanel <- read_sf("Feldaufnahmen_Fanel.gpkg")
ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht))

##Sorting everything into Wald or not Wald
crop_fanel <- crop_fanel %>% 
  ungroup() %>%
  mutate(Wald = Frucht == "Wald")

##Visualizing the habitat Wald and not Wald
ggplot(crop_fanel) +
  geom_sf(aes(fill = Wald))

##Annotating habitat to static segments
wildschwein_k12_static_habitat <- 
  st_join(wildschwein_BE_win_k12, crop_fanel)



