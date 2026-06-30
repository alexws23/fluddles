library(lubridate)
library(sf)
library(raster)
library(dggridR)
library(pdp)
library(mgcv)
library(fitdistrplus)
library(viridis)
library(fields)
library(tidyverse)
library(paletteer)

ebird <- read_csv("data/ebd_fluddle_spr_2026_zf.csv") %>% 
  mutate(protocol_type = factor(observation_type, 
                                levels = c("Stationary" , "Traveling"))) %>%
  # remove observations with no count
  filter(!is.na(observation_count))


fluddle <- ebird %>% 
  filter(str_detect(checklist_comments, "FluddlesIL|FluddleIL|FloodleIL|FloodlesIL|Fluddlesil|fluddlesil|fluddlesIL|fluddleil|Fluddleil|fluddleIL")|
           observer_id=="obsr9063609") %>% 
  filter(all_species_reported == 1)

sites <- unique(fluddle$locality_id)

# Larger dataset
all_pts <- st_as_sf(
  ebird,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Selected points
selected_pts <- st_as_sf(
  fluddle,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Transform to a projected CRS (meters)
all_pts_proj <- st_transform(all_pts, 3857)
selected_pts_proj <- st_transform(selected_pts, 3857)

# 0.1 miles = 160.934 meters
buffer_dist <- 0.1 * 160.934

# Create buffers around selected points
buffers <- st_buffer(selected_pts_proj, dist = buffer_dist)

# Find all points within any buffer
within_idx <- st_intersects(all_pts_proj, st_union(buffers), sparse = FALSE)

result <- all_pts_proj[within_idx, ]

sites2 <- unique(result$locality_id)

all_sites <- ebird %>% 
  filter(locality_id %in% sites2 |
           str_detect(locality, regex("Fluddle|Fluddles|Floodle|Floodles|fiddle|fuddle|fluddlw|Darrell Road Marsh", ignore_case = T))|
           str_detect(checklist_comments, regex("Fluddle|Fluddles|Floodle|Floodles|fiddle|fuddle|fluddlw", ignore_case = T))
  ) %>% 
  filter(all_species_reported == 1)

kane <- all_sites %>% 
  filter(locality_id %in% c("L7929112","L61395562","L61395335","L9326413")) %>% 
  mutate(week = week(observation_date)) %>% 
  group_by(week,locality, scientific_name) %>% 
  summarise(observation_count = max(observation_count)) %>% 
  ungroup() %>% 
  mutate(occ_shorb = ifelse(str_detect(scientific_name, "Charadrius|Calidris|Tringa|Limnodromus|Limosa|Recurvirostra|Himanotopus|Pluvialis|Numenius|Gallinago") & observation_count > 0, 1, 0),
         occ_water = ifelse(str_detect(scientific_name, "Anas|Branta|Anser|Cygnus|Spatula|Mareca|Aythya|Mergus|Oxyura|Bucephela") & observation_count > 0, 1, 0),
         occ = ifelse(observation_count > 0, 1, 0)) %>% 
  filter(occ == 1) %>% 
  group_by(week,locality) %>% 
  summarise(observation_count = mean(observation_count),
            richness = sum(occ),
            shorb_r = sum(occ_shorb),
            water_r = sum(occ_water),
            perc_shorb = shorb_r/richness,
            perc_water = water_r/richness,) %>% 
  ungroup()
  
kane %>% 
  #filter(str_detect(scientific_name, "Calidris")) %>% 
  ggplot(aes(x=week#,color = scientific_name
             )) + 
  geom_point(aes(y=richness), color = "#B8B69EFF")+
  geom_line(aes(y=richness),color = "#B8B69EFF") +
  geom_point(aes(y=shorb_r), color = "#B88244FF")+
  geom_line(aes(y=shorb_r),color = "#B88244FF") +
  geom_point(aes(y=water_r), color = "#527E87FF")+
  geom_line(aes(y=water_r),color = "#527E87FF") +
  facet_wrap(~ locality) +
  theme_minimal() +
  labs(y = "Species Richness")

kane_bar <- all_sites %>% 
  filter(locality_id %in% c("L7929112","L61395562","L61395335","L9326413")) %>% 
  mutate(other = ifelse(str_detect(scientific_name, 
                                   "Charadrius|Calidris|Tringa|Limnodromus|Limosa|Recurvirostra|Himanotopus|Pluvialis|Numenius|Gallinago|Anas|Branta|
                                   |Anser|Cygnus|Spatula|Mareca|Aythya|Mergus|Oxyura|Bucephela|
                                   |Larus|Antigone|Ardea|Nannopterum|Hydroprogne|Butorides|
                                   |Turdus migratorius|Agelaius phoeniceus|Tachycineta bicolor|Anthus rubescens|Hirundo rustica"), scientific_name, "Other")) %>% 
  mutate(other = ifelse(str_detect(other,"Charadrius|Calidris|Tringa|Limnodromus|Limosa|Recurvirostra|Himanotopus|Pluvialis|Numenius|Gallinago"
                                   ), "Shorebird", other),
         other = ifelse(str_detect(other,
                                   "Anas|Branta|Anser|Cygnus|Spatula|Mareca|Aythya|Mergus|Oxyura|Bucephela"),
                                   "Waterfowl", other),
         other = ifelse(str_detect(other, "Larus|Antigone|Ardea|Nannopterum|Hydroprogne|Butorides")
                                   ,"Other Waterbirds", other),
         other = ifelse(str_detect(other, "Turdus migratorius|Agelaius phoeniceus|Tachycineta bicolor|Anthus rubescens|Hirundo rustica")
                        ,"Other Fluddle Birds", other)) %>% 
  mutate(week = week(observation_date)) %>% 
  group_by(scientific_name, week,locality, other) %>% 
  summarise(observation_count = max(observation_count)) %>%
  ungroup() %>% 
  group_by(other, week,locality) %>% 
  summarise(observation_count = sum(observation_count)) %>% 
  ungroup() %>% 
  group_by(week,locality) %>% 
  mutate(total = sum(observation_count)) %>% 
  mutate(perc = observation_count/total) %>% 
  ungroup() %>% 
  mutate(perc = ifelse(is.nan(perc), NA, perc))

kane_bar %>% 
  ggplot(aes(fill=other, y=perc, x=week, group = locality)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~ locality)+
  theme_minimal()+
  scale_fill_manual(values = c("#B8B69EFF","#446455FF","#FDD262FF","#B88244FF","#527E87FF"))+
  scale_x_continuous(breaks = 11:21,minor_breaks = NULL)+
  labs(x = "Week",
       y = "Percentage of Total Observed Individuals")

kane_effort <- all_sites %>% 
  filter(locality_id %in% c("L7929112","L61395562","L61395335","L9326413")) %>% 
  mutate(week = week(observation_date)) %>% 
  group_by(week, locality) %>% 
  summarise(lists = n_distinct(sampling_event_identifier),
            obs = n_distinct(observer_id)) %>% 
  ungroup()

kane_effort %>% 
  ggplot(aes(x = week))+
  geom_point(aes(y = lists)) +
  geom_line(aes(y = lists))+
  geom_point(aes(y = obs), color = "#527E87FF") +
  geom_line(aes(y = obs), color = "#527E87FF")+
  facet_wrap(~ locality, scales = "free_y")

kane_r <- all_sites %>% 
  filter(locality_id %in% c("L7929112","L61395562","L61395335","L9326413")) %>% 
  mutate(week = week(observation_date)) %>%
  mutate(shorb = ifelse(str_detect(scientific_name,"Charadrius|Calidris|Tringa|Limnodromus|Limosa|Recurvirostra|Himanotopus|Pluvialis|Numenius|Gallinago"
          ), scientific_name, NA),
         water = ifelse(str_detect(scientific_name,
                                   "Anas|Branta|Anser|Cygnus|Spatula|Mareca|Aythya|Mergus|Oxyura|Bucephela"),
                        scientific_name, NA)) %>% 
  group_by(sampling_event_identifier, week, locality) %>% 
  filter(observation_count > 0) %>% 
  summarise(list_r = n_distinct(scientific_name),
            list_s = n_distinct(shorb),
            list_w = n_distinct(water)) %>% 
  ungroup() %>% 
  group_by(week, locality) %>% 
  summarise(week_r = mean(list_r),
            week_s = mean(list_s),
            week_w = mean(list_w)) %>% 
  ungroup()
  
kane_r %>% 
  ggplot(aes(x = week))+
  geom_point(aes(y=week_r))+
  geom_line(aes(y=week_r))+
  geom_point(aes(y=week_s), color = "#B88244FF")+
  geom_line(aes(y=week_s),color = "#B88244FF") +
  geom_point(aes(y=week_w), color = "#527E87FF")+
  geom_line(aes(y=week_w),color = "#527E87FF") +
  facet_wrap(~ locality)
