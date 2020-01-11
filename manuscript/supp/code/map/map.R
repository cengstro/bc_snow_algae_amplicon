# draw sample map
# 1-10

library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
# library(kableExtra)
# library(magick)
# library(formattable)
# library(ggdark)

# it wouldve been easier to just download the map as png, and the city data instead of using the clumsy API
# next time I will do this, but for now using the Rda map data

field <- read_csv(here::here("data/02_tidied/tidied_field.csv"))
cities <- read_csv(here::here("output/map/city_coords.csv"))
load(here::here("output/map/raw_google_map.rda"))

# only plot vancouver
cities <- cities %>% 
  filter(name == "Vancouver")

# select field data to plot (only mtns, not individual sites)
field_mtns <- field %>% 
  mutate(mtn_letter = alias %>% str_sub(1,1))

field_sites <- field_mtns %>% 
  distinct(mtn_letter) 

field_site_coords <- field_mtns %>% 
  group_by(mtn_letter) %>% 
  drop_na(lat, lon) %>% 
  summarise(n=n(), 
            mean_elev = mean(elev_m), 
            first_visit=min(date) %>% format("%b %d"), 
            last_visit= max(date) %>% format("%b %d"), 
            n_visits= length(unique(date)),
            lat=mean(lat),
            lon=mean(lon),
            date=min(date)) %>% 
  arrange(date) %>% 
  select(-date)

my_map <- ggmap(algae_map, extent = "device") +
  geom_point(data = field_site_coords, aes(x=lon, y=lat),
             size = 1, alpha=1, 
             color = "red") +
  # geom_text(data=field_site_coords, aes(label = mtn_letter)) # dosen't map site A, too far south for map
  geom_label_repel(data=field_site_coords, aes(label = mtn_letter),
    size = 4, 
    box.padding = 0.2, point.padding = 0.3,
    segment.color = 'grey50') +
  geom_point(data = cities, shape = 15, color="white") +
  geom_text(data = cities, aes(label = name), hjust = 1.1, vjust = 2, size = color="white")

my_map

# save
ggsave(filename = here::here("manuscript/supp/map_1-10.tiff"), plot=my_map, width = 200, height = 200, units="mm",dpi=300)
