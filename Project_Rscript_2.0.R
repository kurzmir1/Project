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

##Make values below 10 static, in newe dataframes for each k
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

##!!!!!! From here on I only worked with k4 and it finally worked!
##Making segments
  wildschwein_BE_win_k4 <-wildschwein_BE_win_k4 %>%
    mutate(
      segment_ID = rle_id(static_k4_10)
    )
  
  wildschwein_BE_win_segments_k4 <- wildschwein_BE_win_k4 %>%

###!!!!!This didn't work and I don't know why.... 
##Caluclate time span of each static segment
segment_time <- function(pig, id){
  name = paste(pig,id, sep = "")
  name <- pig %>%
    filter(pig$segment_ID == id)
  start = 1
  end = nrow(name)
  name$timeDiff <- as.numeric(difftime(name$DatetimeUTC[end],name$DatetimeUTC[start], units = "mins"))
  
  return(name)
}

?apply

apply("segment_time", wildschwein_BE_win_segments_k4$segment_ID)

result = segment_time(wildschwein_BE_win_segments_k4,1)

