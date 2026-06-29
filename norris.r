library(tidyverse)
library(vegan)

ebd <- read.csv(file = "ebird_IL_toMay.csv")

norris <- ebd %>% 
  filter(locality_id == "L61395335") %>% 
  filter(all_species_reported == T)

norris_water <- norris %>% 
  filter(#taxonomic_order %in% 243:792 | 
    taxonomic_order %in% 5770:6356) %>% 
  mutate(week = week(observation_date),
         observation_count = as.numeric(observation_count))

norris_weekly <- norris_water %>% 
  group_by(week, common_name) %>% 
  summarise(mean = mean(observation_count)) %>% 
  ungroup()

norris_pivot <- norris_weekly %>% 
  pivot_wider(
    names_from = common_name,
    values_from = mean
  )

norris_pivot[is.na(norris_pivot)] <- 0

shannon <- norris_pivot %>% 
  select(-c("week")) %>%
  #specnumber()
  diversity(index = "shannon")

print(shannon)

shannon <- as.data.frame(shannon) %>% 
  mutate(week = 15:19) %>% 
  mutate(week = ifelse(week==19,21,week))

norris_diversity <- norris_pivot %>% 
  left_join(shannon) %>% 
  ungroup()

year <- 2026
week <- norris_diversity$week
day  <- 1 # 1 represents Monday

# Combine and convert to a Date object
norris_diversity$weekdate <- as.Date(paste(year, week, day, sep = "-"), "%Y-%W-%u")-7

##### Calculate total diversity
all <- norris %>% 
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
  #specnumber()
  diversity(index = "shannon")

print(shannon2)

shannon2 <- as.data.frame(shannon2) %>% 
  mutate(week = 14:19)%>% 
  mutate(week = ifelse(week==19,21,week))

all_diversity <- all_pivot %>% 
  left_join(shannon2) %>% 
  ungroup()

year <- 2026
week <- all_diversity$week
day  <- 1 # 1 represents Monday

# Combine and convert to a Date object
all_diversity$weekdate <- as.Date(paste(year, week, day, sep = "-"), "%Y-%W-%u")-7

ggplot(norris_diversity, aes(x = weekdate, y = shannon)) +
  geom_point() +
  geom_line()+
  geom_point(data = all_diversity, aes(x = weekdate, y = shannon2), color = "indianred")+
  geom_line(data = all_diversity, aes(x = weekdate, y = shannon2), color = "indianred")+
  theme_minimal()
