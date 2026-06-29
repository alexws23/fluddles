library(tidyverse)
library(vegan)

ebd <- read.csv(file = "ebird_IL_toMay.csv")

seavey <- ebd %>% 
  filter(locality_id == "L7929112") %>% 
  filter(all_species_reported == T)

seavey_water <- seavey %>% 
  filter(#taxonomic_order %in% 243:792 | 
           taxonomic_order %in% 5770:6356) %>% 
  mutate(week = week(observation_date),
         observation_count = as.numeric(observation_count))

seavey_weekly <- seavey_water %>% 
  group_by(week, common_name) %>% 
  summarise(mean = mean(observation_count)) %>% 
  ungroup()

seavey_pivot <- seavey_weekly %>% 
  pivot_wider(
    names_from = common_name,
    values_from = mean
  )

seavey_pivot[is.na(seavey_pivot)] <- 0

shannon <- seavey_pivot %>% 
  select(-c("week")) %>% 
  diversity(index = "shannon",)

print(shannon)

shannon <- as.data.frame(shannon) %>% 
  mutate(week = 11:17)

seavey_diversity <- seavey_pivot %>% 
  left_join(shannon) 

year <- 2026
week <- seavey_diversity$week
day  <- 1 # 1 represents Monday

# Combine and convert to a Date object
seavey_diversity$weekdate <- as.Date(paste(year, week, day, sep = "-"), "%Y-%W-%u")-7

##### Calculate total diversity
all <- seavey %>% 
  mutate(week = week(observation_date),
         observation_count = as.numeric(observation_count))

all_weekly <- all %>% 
  group_by(week, common_name) %>% 
  summarise(mean = mean(observation_count)) %>% 
  ungroup()

all_pivot <- all_weekly %>% 
  pivot_wider(
    names_from = common_name,
    values_from = mean
  )

all_pivot[is.na(all_pivot)] <- 0

shannon2 <- all_pivot %>% 
  select(-c("week")) %>% 
  diversity(index = "shannon",)

print(shannon2)

shannon2 <- as.data.frame(shannon2) %>% 
  mutate(week = 11:18) %>% 
  mutate(week = ifelse(week==18,21,week))

all_diversity <- all_pivot %>% 
  left_join(shannon2) 

year <- 2026
week <- all_diversity$week
day  <- 1 # 1 represents Monday

# Combine and convert to a Date object
all_diversity$weekdate <- as.Date(paste(year, week, day, sep = "-"), "%Y-%W-%u")-7

ggplot(seavey_diversity, aes(x = weekdate, y = shannon)) +
  geom_point() +
  geom_line()+
  geom_point(data = all_diversity, aes(x = weekdate, y = shannon2), color = "indianred")+
  geom_line(data = all_diversity, aes(x = weekdate, y = shannon2), color = "indianred")+
  geom_vline(xintercept = as.Date("2026-04-25"))+
  theme_minimal()
