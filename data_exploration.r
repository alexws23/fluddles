library(tidyverse)
library(sf)
library(rnaturalearth)
library(usmap)
library(spatstat)

setwd(dir = "C:/Users/awsmilor/Git/Ward Lab/fluddles/data")
getwd()

IL <- us_map(regions = "counties",
                 include = "IL") %>% 
  st_transform(26916)

ebird <- read.csv(file = "MyEBirdData.csv") %>% 
  mutate(datetime = paste(Date, Time),
         Count = as.numeric(Count),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude)) %>% 
  filter(All.Obs.Reported == 1,
         Distance.Traveled..km. < 0.5 | is.na(Distance.Traveled..km.)) %>% 
  mutate(doy = lubridate::yday(Date))

ebird$datetime <- parse_date_time(ebird$datetime, orders = "ymd IM p")

seavey <- ebird %>% 
  filter(Location.ID == "L7929112") %>% 
  mutate(datetime = paste(Date, Time))

ebird %>% 
  filter(Common.Name  %in% c("Greater Yellowlegs", "Lesser Yellowlegs", "Pectoral Sandpiper", "Blue-winged Teal"),
         Latitude > 41) %>% 
  ggplot(aes(x = datetime, y = Count, color = Common.Name, group = Common.Name)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_minimal()



ebird %>% 
  filter(Latitude > 41,
         Common.Name %in% c("Greater Yellowlegs", "Lesser Yellowlegs", "Pectoral Sandpiper", "Blue-winged Teal")) %>% 
  ggplot(aes(x = doy, y = Common.Name, group = Common.Name, weight = Count)) +
  geom_density(fill = "darkgreen", alpha = 0.6)+
  theme_minimal()

ebird_lists <- ebird %>% 
  filter(State.Province == "US-IL") %>% 
  group_by(Location.ID) %>% 
  summarise(Longitude = mean(Longitude),
            Latitude = mean(Latitude))

ebird_st <- ebird_lists %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(26916)

ebird_st <- ebird %>% 
  filter(State.Province == "US-IL",
         Common.Name == "Dunlin") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(26916)

ppp_points <- as.ppp(ebird_st)

Window(ppp_points) <- as.owin(IL)

density_spatstat <- ppp_points %>% 
  density(dimyx = 256, weights = ppp_points$marks$Count
          )

density_stars <- stars::st_as_stars(density_spatstat)

density_sf <- st_as_sf(density_stars) %>% 
  st_set_crs(26916)

ggplot() +
  geom_sf(data = density_sf, aes(fill = v), col = NA) +
  scale_fill_viridis_c() +
  geom_sf(data = st_boundary(IL)) +
  geom_sf(data = ebird_st, size = 2, col = "black")

ebird %>% 
  ggplot() +
  geom_sf(data = IL,fill=NA) +
  geom_point(aes(x = Longitude, y = Latitude))

ggplot() +
  stat_density_2d(data = ebird_st, 
                  mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                                y = purrr::map_dbl(geometry, ~.[2]),
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  geom_sf(data = IL, fill = NA) + 
  geom_sf(data = ebird_st, color = 'red') + 
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  theme_test()

arb <- ebird %>% 
  filter(#Location == "46th Rd. and 21st Rd. Fluddle",
         Taxonomic.Order %in% c(5770:6356))

arb %>% ggplot(aes(x=datetime, y = Count, color = Common.Name, group = Common.Name))+
  #geom_point()+
  geom_smooth(method = "gam", se = F)+
  theme_minimal() +
  facet_wrap(vars(Common.Name), scales = "free")
