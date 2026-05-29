library(tidyverse)

points <- read.csv("C:/Users/awsmilor/Documents/ArcGIS/Projects/grassland_bird_survey/Midewin Bird Survey Lat-Long for Tracts and Points(Holman).csv")

surveys <- read.csv("C:/Users/awsmilor/Documents/ArcGIS/Projects/grassland_bird_survey/FieldPoint_Year_DataPresence.csv")

all <- left_join(points, surveys, by = join_by(Point_Full))

df_long <- all %>%
  pivot_longer(
    cols = starts_with("Year_"),
    names_to = "Year",
    values_to = "Visited"
  ) %>%
  mutate(
    Year = gsub("Year_", "", Year),
    Year = as.integer(Year)
  ) %>%
  filter(Visited == TRUE) %>% 
  select(-c("X"))

df_long <- df_long %>%
  mutate(Date = as.Date(paste0(Year, "-06-08")))

write.csv(all, file = "C:/Users/awsmilor/Documents/ArcGIS/Projects/grassland_bird_survey/midewin_survey_points.csv")

write.csv(df_long, file = "C:/Users/awsmilor/Documents/ArcGIS/Projects/grassland_bird_survey/midewin_survey_points_long.csv")

recent <- all %>% 
  group_by(Tract) %>% 
  drop_na() %>% 
  summarize(mean = mean(Last_Surveyed))

df <- all %>% 
  left_join(recent, by = join_by(Tract)) %>% 
  filter(mean > 2016)

tracts <- unique(df$Tract)

set.seed(101)

for (i in tracts) {
  tmp <- df %>% 
    filter(Tract == i)
  
  if (i %in% c(74, 209, 210)) {
    sel <- tmp %>% 
      select(Tract, Point, Latitude, Longitude, Point_Full)
    
    assign(paste("tract",i, sep = "_"), sel)
  } else{
    
  count <- length(unique(tmp$Point))

  amount <- round(count/2, digits = 0)
  
  t <- tmp %>% 
    filter(Year_2025 == TRUE) %>% 
    slice_sample(n = round(amount/2, 0))
  
  f <- tmp %>% 
    filter(Year_2025 == FALSE) %>% 
    slice_sample(n = round(amount/2+1, 0))
  
  sel <- bind_rows(t, f) %>% 
    select(Tract, Point, Latitude, Longitude, Point_Full)
  
  assign(paste("tract",i, sep = "_"), sel)
  }
}

df_list <- mget(ls(pattern = "^tract_.*"))

points_2026 <- df_list %>% 
  reduce(full_join)

rm(list = ls(pattern = "^tract_.*"))

points_2026 %>% 
  ggplot(aes(x = Longitude, y = Latitude)) +
  geom_point()+
  theme_minimal()

write.csv(points_2026, file = "GBS_Midewin_2026_Points.csv")
 