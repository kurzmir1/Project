######New idea without rolling windows

##Steps smaller than 10 m
wildschwein_BE <- wildschwein_BE %>% 
  ungroup() %>%
  mutate(steplength < 10)

##Making segments
wildschwein_BE_segments <-wildschwein_BE %>%
  mutate(
    segment_ID = rle_id(steplength < 10)
  )

##Calculate time length of segments with steps < 10m
wildschwein_BE_segments_time <- wildschwein_BE_segments %>%
  st_drop_geometry() %>%
  filter(`steplength < 10` == "TRUE") %>%
  group_by(TierName, segment_ID) %>%
  summarise(min = min(DatetimeUTC), max = max(DatetimeUTC)) %>%
  mutate(timediff = as.integer(difftime(max, min, units = "mins")))

##Filter for segments longer than 2 h
wildschwein_BE_segments_time_2h <- wildschwein_BE_segments_time %>%
  ungroup() %>%
  mutate(timediff_2h = timediff > 120) %>%
  filter(timediff_2h == "TRUE")

##Histogram for timediff_2h
ggplot(data = wildschwein_BE_segments_time_2h, mapping = aes(timediff))+
  geom_histogram(binwidth = 10)+
  xlim(120,1000)

mean(wildschwein_BE_segments_time_2h$timediff)
