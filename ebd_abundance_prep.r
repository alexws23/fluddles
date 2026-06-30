library(tidyverse)
library(auk)

ebd <- auk_ebd("data/ebd_US-IL_202601_202606-2026.txt", 
               file_sampling = "data/ebd_US-IL_202601_202606_sampling.txt")

ebd_filters <- ebd %>% 
  # march-june, use * to get data from any year
  auk_date(date = c("2026-03-01", "2026-06-01")) %>% 
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()
ebd_filters

f_ebd <- file.path("data/ebd_fluddle_spr_2026.txt")
f_sampling <- file.path("data/ebd_fluddle_checklists_spr_2026.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
ebd_zf <- ebd_zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(observation_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    hour_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

# additional filtering
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # 10 or fewer observers
    number_observers <= 10)

ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, locality, latitude, longitude,
         observation_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km,
         number_observers, checklist_comments)
write_csv(ebird, "data/ebd_fluddle_spr_2026_zf.csv", na = "")

