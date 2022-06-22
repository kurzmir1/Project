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

##############Habitat stuff
##Reading in habitat data
crop_fanel <- read_sf("Feldaufnahmen_Fanel.gpkg")

##Sorting everything into Wald or not Wald
crop_fanel <- crop_fanel %>% 
  ungroup() %>%
  mutate(Wald = Frucht == "Wald")

##Annotating habitat to all points
wildschwein_BE_win_k12_segments_withgeometry_2h_withcrop <- 
  st_join(wildschwein_BE_win_k12_segments_withgeometry_2h, crop_fanel)

##Calculate steplength

wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

mean(wildschwein_BE$steplength, na.rm=TRUE)

median(wildschwein_BE$steplength, na.rm=TRUE)


##calculate mean distance for different rolling windows
wildschwein_BE_win <- wildschwein_BE
wildschwein_BE_win$k12 <- rollmean(wildschwein_BE$steplength, k=12, fill = NA, align = "left")


##Make values below 10 static, in new dataframe
wildschwein_BE_win_k12 <- wildschwein_BE_win %>% 
  ungroup() %>%
  mutate(static_k12_10 = k12 < 10)

##Function for making segments
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

##Making segments
wildschwein_BE_win_k12 <-wildschwein_BE_win_k12 %>%
  mutate(
    segment_ID = rle_id(static_k12_10)
  )

##Calculate time span of each static segment
wildschwein_BE_win_k12_segments <- wildschwein_BE_win_k12 %>%
  st_drop_geometry() %>%
  filter(static_k12_10 == "TRUE") %>%
  group_by(segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%  
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##Giving each segment the first coordinate of the segment
wildschwein_segments_withE <- wildschwein_BE_win_k12 %>%
  st_drop_geometry() %>%
  group_by(segment_ID) %>%
  summarise(segment_E = first(E))

wildschwein_segments_withN <- wildschwein_BE_win_k12 %>%
  st_drop_geometry() %>%
  group_by(segment_ID) %>%
  summarise(segment_N = first(N))

wildschwein_BE_win_k12_segments_withgeometry <-
  left_join(wildschwein_BE_win_k12_segments, wildschwein_segments_withE, by = "segment_ID")

wildschwein_BE_win_k12_segments_withgeometry <-
  left_join(wildschwein_BE_win_k12_segments_withgeometry, wildschwein_segments_withN, by = "segment_ID")

wildschwein_BE_win_k12_segments_withgeometry <- st_as_sf(wildschwein_BE_win_k12_segments_withgeometry, coords = c("segment_E", "segment_N"), crs = 2056, remove = FALSE)



##Filter out all static segments below 2h
wildschwein_BE_win_k12_segments_withgeometry_2h <- wildschwein_BE_win_k12_segments_withgeometry %>%
  filter(timediff > 120)


##Calculate mean static time per animal
mean_static_animal <- wildschwein_BE_win_k12_segments %>% 
  group_by(TierID) %>%
  summarise_at(vars(timediff_plusrollingwindow), list(name = mean))

##Filter for static k12 segments 
wildschwein_BE_win_k12_static <- wildschwein_BE_win_k12 %>%
  filter(static_k12_10 == "TRUE")


##Make dataframe for one week in 2015 for Ruth, Rosa, Isabelle, Caroline, Nicole, Sabine
girls_January2015 <- wildschwein_BE %>%
  filter(TierName %in% girls) %>%
  filter(DatetimeUTC >=as.Date("2015-01-01")& DatetimeUTC <=as.Date("2015-01-08"))

girls <- c("Sabine", "Ruth", "Rosa", "Isabelle", "Caroline", "Nicole")

rm(girls_Januar2015)

#########Visualization
##Visualizing during what daytime the static segments are
day <- table(wildschwein_BE_win_k12_static$day)
barplot(day)

day

##Visualizing in what habitat the static segments are
Frucht <- table(wildschwein_BE_win_k12_segments_withgeometry_2h_withcrop$Frucht)
barplot(Frucht)

Wald <- table(wildschwein_BE_win_k12_static$Wald)
barplot(Wald)

write.csv(mean_static_animal)

##Visualize steplength of girls_January2015 during time

ggplot(girls_January2015, mapping = aes(DatetimeUTC, steplength))+
  geom_line(aes(colour=TierName))+
  facet_wrap(~TierName)



##Visualize length of static segments

ggplot(wildschwein_BE_win_k12_segments, mapping = aes(timediff))+
  geom_histogram()+
  xlim(0,1000)

















