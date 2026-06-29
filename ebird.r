library(tidyverse)
library(sf)
library(rnaturalearth)
library(usmap)
library(spatstat)
library(auk)

input.file <- "ebd_US-IL_202601_202606_smp_relMay-2026/ebd_US-IL_202601_202606_smp_relMay-2026.txt"

ebd <- input.file %>% 
  read_ebd()
  #read_delim(file = "C:/Users/awsmilor/Downloads/ebd_US-IL_202601_202606_smp_relMay-2026/ebd_US-IL_202601_202606_smp_relMay-2026.txt", delim = "\t")

write.csv(ebd, file = "ebird_IL_toMay.csv")

fluddle <- ebd %>% 
  filter(str_detect(`CHECKLIST COMMENTS`, "FluddlesIL|FluddleIL|FloodleIL|FloodlesIL|Fluddlesil|fluddlesil|fluddlesIL|fluddleil|Fluddleil|fluddleIL")|
           `OBSERVER ID`=="obsr9063609") %>% 
  filter(`ALL SPECIES REPORTED` == 1)

sites <- unique(fluddle$`LOCALITY ID`)

latitude <- unique(fluddle$LATITUDE)
longitude <- unique(fluddle$LONGITUDE)

# Larger dataset
all_pts <- st_as_sf(
  ebd,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = 4326
)

# Selected points
selected_pts <- st_as_sf(
  fluddle,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = 4326
)

# Transform to a projected CRS (meters)
all_pts_proj <- st_transform(all_pts, 3857)
selected_pts_proj <- st_transform(selected_pts, 3857)

# 0.1 miles = 160.934 meters
buffer_dist <- 0.1 * 1609.34

# Create buffers around selected points
buffers <- st_buffer(selected_pts_proj, dist = buffer_dist)

# Find all points within any buffer
within_idx <- st_intersects(all_pts_proj, st_union(buffers), sparse = FALSE)

result <- all_pts_proj[within_idx, ]

sites2 <- unique(result$`LOCALITY ID`)

all_sites <- ebd %>% 
  filter(locality_id %in% sites2 |
         str_detect(locality, regex("Fluddle|Fluddles|Floodle|Floodles|fiddle|fuddle|fluddlw|Darrell Road Marsh", ignore_case = T))|
        str_detect(checklist_comments, regex("Fluddle|Fluddles|Floodle|Floodles|fiddle|fuddle|fluddlw", ignore_case = T))
        ) %>% 
  filter(all_species_reported == 1)

all_shorbs <- all_sites %>% 
  filter(`TAXONOMIC ORDER` %in% 243:792 | 
         `TAXONOMIC ORDER` %in% 5770:6356)

all_shorbs %>% 
  group_by(LOCALITY) %>% 
  summarise(species = n_distinct(`SAMPLING EVENT IDENTIFIER`)) %>% 
  arrange(desc(species)) %>% 
  head(10)

agpl_f <- sum(as.numeric(all_sites$`OBSERVATION COUNT`), na.rm = T)


all <- unique(all_sites$locality_id)

`%ni%` <- Negate(`%in%`) 

has_shorb <- ebd %>% 
  filter(locality_id %ni% all) %>% 
  filter(all_species_reported == 1) %>% 
  group_by(locality_id) %>% 
  filter(n_distinct(common_name[taxonomic_order %in% 5770:6356]) >= 12)
  #filter(any(taxonomic_order %in%5770:6356))

unique(has_shorb$locality)

not_fluddle <- unique(other$LOCALITY)

view(not_fluddle)

agpl_o <- sum(as.numeric(other$`OBSERVATION COUNT`), na.rm = T)

obs <- other %>% 
  mutate(`GROUP IDENTIFIER` == ifelse(is.na(`GROUP IDENTIFIER`), yes = `SAMPLING EVENT IDENTIFIER`,no = `GROUP IDENTIFIER`))# %>% 
  distinct(`GROUP IDENTIFIER`, .keep_all = T) %>% 
  group_by(LOCALITY) %>% 
  count() %>% 
  left_join(fluddle %>% select(LOCALITY,LATITUDE, LONGITUDE)) %>% 
  distinct(LOCALITY, .keep_all = T) %>% 
  ungroup()

lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
                     returnclass = "sf")
states <- ne_states(returnclass = "sf")

xmin <- min(fluddle$LONGITUDE) - 1
xmax <- max(fluddle$LONGITUDE) + 1
ymin <- min(fluddle$LATITUDE) - 1
ymax <- max(fluddle$LATITUDE) + 1

map <- ggplot()+
  geom_sf(data = states,fill="gray98",color = NA)+
  geom_sf(data = lakes, colour = NA, fill = "gray80")+
  geom_sf(data = states,fill=NA)+
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  geom_point(data = other, aes(x= LONGITUDE, y = LATITUDE))+
  theme_bw() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray80"),
    #legend.position = "none"
    )

map

all_sites %>% 
  filter(common_name  %in% c("American Golden-Plover", "Dunlin", "Pectoral Sandpiper", "Least Sandpiper", "Killdeer", "Semipalmated Sandpiper", "Spotted Sandpiper", "Semipalmated Plover", "Long-billed Dowitcher", "Greater Yellowlegs","Lesser Yellowlegs","White-rumped Sandpiper"),
             #LATITUDE > 41,
             observation_date > "2026-03-15") %>% 
  ggplot(aes(x = observation_date, y = log(as.numeric(observation_count)/(duration_minutes)), group = common_name)) +
  #geom_point() +
  geom_point(aes(),
  color = "gray40",
  alpha = .5)+
  #geom_smooth(method = "gam", span = 0.2, se = FALSE) +
  geom_smooth(method = "gam", span = 0.2, se = T, color = "gray20") +
  geom_point(data = has_shorb %>% filter(common_name  %in% c("American Golden-Plover", "Dunlin", "Pectoral Sandpiper", "Least Sandpiper", "Killdeer", "Semipalmated Sandpiper", "Spotted Sandpiper", "Semipalmated Plover", "Long-billed Dowitcher", "Greater Yellowlegs","Lesser Yellowlegs","White-rumped Sandpiper"),
                                        #LATITUDE > 41,
                                        observation_date > "2026-03-15"),
             aes(x = observation_date, y = log(as.numeric(observation_count)/(duration_minutes)), group = common_name),
             color = "indianred",
             alpha = .5)+
  #geom_smooth(method = "gam", span = 0.2, se = FALSE) +
  geom_smooth(data = has_shorb %>% filter(common_name  %in% c("American Golden-Plover", "Dunlin", "Pectoral Sandpiper", "Least Sandpiper", "Killdeer", "Semipalmated Sandpiper", "Spotted Sandpiper", "Semipalmated Plover", "Long-billed Dowitcher", "Lesser Yellowlegs", "Greater Yellowlegs","White-rumped Sandpiper"),
                                                 #LATITUDE > 41,
                                                 observation_date > "2026-03-15"),
              aes(x = observation_date, y = log(as.numeric(observation_count)/(duration_minutes)), group = common_name),
              method = "gam", span = 0.2, se = T, color = "indianred") +
  theme_minimal()+
  facet_wrap(~ common_name, scales ="free_y")

fluddle %>% 
  group_by(`COMMON NAME`) %>% 
  count() %>% 
  arrange(desc(n))

occupancy<- all_sites %>% 
  filter(common_name == "American Golden-Plover",
         observation_date > "2026-03-15") %>% 
  group_by(observation_date) %>% 
  summarise(count = n())

lists <- all_sites %>%
  filter(observation_date > "2026-03-15") %>% 
  group_by(observation_date) %>% 
  summarise(lists = n_distinct(sampling_event_identifier))

freq <- occupancy %>% 
  full_join(lists)

freq <- freq %>% 
  mutate(count = ifelse(is.na(count), 0, count),
         perc = count/lists,
         )

freq %>% 
  ggplot(aes(observation_date, perc))+
  geom_point()+
  geom_smooth(method = "gam")

#### Other sites
occupancy2<- has_shorb %>% 
  filter(common_name == "American Golden-Plover",
         observation_date > "2026-03-15") %>% 
  group_by(observation_date) %>% 
  summarise(count = n())

lists2 <- has_shorb %>% 
  filter(observation_date > "2026-03-15") %>% 
  group_by(observation_date) %>% 
  summarise(lists = n_distinct(sampling_event_identifier))

freq2 <- occupancy2 %>% 
  full_join(lists2)

freq2 <- freq2 %>% 
  mutate(count = ifelse(is.na(count), 0, count),
         perc = count/lists,
  )

freq2 %>% 
  ggplot(aes(observation_date, perc))+
  geom_point(color = "#00688B")+
  geom_smooth(method = "gam", color = "#00688B")+
  geom_point(data = freq, aes(observation_date, perc), color = "indianred")+
  geom_smooth(data = freq, method = "gam", color = "indianred") +
  theme_minimal()+
  labs(title = "Least Sandpiper",
       x = "Observation Date",
       y = "Frequency")

mean <- all_sites %>% 
  group_by(observation_date, locality, common_name) %>%
  summarise(count = mean(as.numeric(observation_count)),
            effort = mean(duration_minutes))
  
mean %>% 
  filter(common_name == "Least Sandpiper") %>% 
  ggplot(aes(observation_date, count/effort))+
  geom_point()+
  geom_smooth(method = "gam")
