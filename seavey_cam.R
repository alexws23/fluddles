library(tidyverse)

cam <- read.csv("R:/Fluddle/Data/Seavey_Camera_2026.csv") %>% 
  mutate(DateTime = as_datetime(DateTime)) %>% 
  mutate(dup = ifelse(difftime(DateTime,lag(DateTime)) < 5, T, F)) %>% 
  mutate(dup = ifelse(is.na(dup), F, dup)) %>% 
  filter(dup == F)

cam_pivot <- cam %>% 
  mutate(row_id = row_number()) %>%   # temporary ID to keep rows together
  select(-c(RootFolder, RelativePath,NightFlag, Empty, Problem, Comment, Analyst, dup)) %>% 
  pivot_longer(
    cols = -c(File, row_id, DateTime, Depth, Temperature),
    names_to = c(".value", "num"),
    names_pattern = "(Species|Count)(\\d+)"
  ) %>%
  select(-num, -row_id) %>% 
  filter(Species != "")

cam_hour <-  cam_pivot %>% 
  filter(Species %in% c("Greater Yellowlegs", "Tringa sp.", "Lesser Yellowlegs")) %>% 
  mutate(hour = hour(DateTime))

seavey <- ebd %>% 
  filter(locality_id == "L7929112") %>% 
  filter(all_species_reported == T)

# Explore depth data

depth <- cam %>% 
  select(c(DateTime, Depth)) %>% 
  drop_na(Depth) %>% 
  filter(Depth>0) %>% 
  mutate(Depth = Depth - 0.17)

ggplot(depth,aes(DateTime, Depth))+
  geom_point() +
  geom_smooth(method = "gam")

# Model Depth vs shorebird abundance
depth_by_day <- depth %>% 
  mutate(date = date(DateTime)) %>% 
  group_by(date) %>% 
  summarise(mean_depth = mean(Depth)) %>% 
  ungroup()

by_day <- seavey %>% 
  filter(observation_date >= as_date("2026-04-17") & observation_date < as_date("2026-04-27"),
         taxonomic_order %in% 5770:6356) %>% 
  mutate(observation_count = as.numeric(observation_count),
         date = date(observation_date)) %>% 
  group_by(date) %>% 
  summarise(max = sum(observation_count)) %>% 
  ungroup()

seavey_pivot <- by_day %>% 
  pivot_wider(
    names_from = common_name,
    values_from = max
  )

seavey_pivot[is.na(seavey_pivot)] <- 0

shannon <- seavey_pivot %>% 
  select(-c(date)) %>% 
  diversity(index = "shannon",)

print(shannon)

shannon <- as.data.frame(shannon) %>% 
  mutate(date = seavey_pivot$date)

seavey_diversity <- seavey_pivot %>% 
  left_join(shannon) 

model <- by_day %>% 
  left_join(depth_by_day)

glm(shannon ~ mean_depth, family = "gaussian", data = model)

ggplot(model,aes(mean_depth,max))+
  geom_point() +
  geom_smooth(method = "glm")

cam_pivot %>% 
  group_by(Species) %>% 
  ggplot(aes(x = DateTime, y = Count, color = Species)) +
  geom_point()+
  geom_line()
