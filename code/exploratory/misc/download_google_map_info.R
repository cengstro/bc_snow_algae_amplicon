# get google map

library(tidyverse)
library(here)
library(googleway)
library(ggmap)


# get base map --------------
key=
register_google(key = key) 

algae_map <- get_googlemap(c(-123.004722, 49.850556), zoom = 7, maptype = "satellite")


# get city data ------------------
cities <- c("Vancouver, BC, Canada", "Squamish, BC, Canada")

# get coordinates from google
google_output <- cities %>% 
  map(~googleway::google_geocode(.x, key=key))
glimpse(google_output)

# functions to reach into the list and extract wanted elements
city_name <- function(x) x$results$address_components[[1]][1,2]
lat <- function(x) x$results$geometry$location[1,1]
lon <- function(x) x$results$geometry$location[1,2]

# make tbl with city and coords
city_coords <- google_output %>% {
  tibble(
    name = map_chr(., .f=city_name),
    lat = map_dbl(., .f=lat),
    lon = map_dbl(., .f=lon)
  )
}

head(city_coords)

# save data
dir.create(here::here("output/map"))

save(algae_map, file = here::here("output/map/raw_google_map.rda"))
write_csv(city_coords, here::here("output/map/city_coords.csv"))
