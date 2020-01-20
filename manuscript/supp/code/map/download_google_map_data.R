# get map data
# 1-10

library(tidyverse)
library(googleway)
library(ggmap)
library(png)

# download data from googleway: do only once to avoid API overcharges
key <-
register_google(key = key)

# get map
googlemap <- get_googlemap(c(-123.004722, 49.850556), zoom = 7, maptype = "satellite")
my_map <- ggmap(googlemap)
my_map
# get city coords -----------------------
cities <- c("Vancouver, BC, Canada", "Squamish, BC, Canada")
city_coords <- cities %>% 
  map(~googleway::google_geocode(.x, key=key))
glimpse(google_output) # ugly lists

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

city_coords # a pretty tbl

# save data
ggsave(filename = here::here("data/map_downloads/googlemap.png"), plot=my_map, 
       width = 225, height = 225, units="mm", dpi = 300) # didnt end up using this
save(googlemap, file=here::here("data/map_downloads/googlemap.rda")) # ended up using this... next time, dont bother with clunky API, download the map data directly as png ("do one thing and do it well")

write_csv(city_coords, here::here("data/map_downloads/map_data_cities.csv"))

