# this script takes in the clean, filtered 18s data, and limits it taxonomically to just algae

library(tidyverse)
library(here)


depth <- read_csv(here("data/03_filtered/18s/depth_18s_rel_abund.csv"))
main <- read_csv(here("data/03_filtered/18s/filtered_cleaned_18s_rel_abund.csv"))
protocol <- read_csv(here("data/03_filtered/18s/protocol_comparison_18s_rel_abund.csv"))
scrape <- read_csv(here("data/03_filtered/18s/scrape_comparison_18s_rel_abund.csv"))
water <- read_csv(here("data/03_filtered/18s/water_controls_18s_rel_abund.csv"))
white <- read_csv(here("data/03_filtered/18s/white_snow_18s_rel_abund.csv"))

taxonomy <- read_csv(here("data/02_tidied/tidied_silva_18s_assignments.csv"))

algae_asv_ids <- taxonomy %>%
  filter(phylum == "Chlorophyta") %>% 
  pull(asv_id)

filter_to_algae <- function(tbl){
  tbl %>% 
    left_join(taxonomy, by="asv_id") %>% 
    filter(asv_id %in% algae_asv_ids) %>% 
    select(1:4)
}

# run filters
depth_2 <- depth %>% filter_to_algae()
main_2 <- main %>% filter_to_algae()
protocol_2 <- protocol %>% filter_to_algae()
scrape_2 <- scrape %>% filter_to_algae()
water_2 <- water %>% filter_to_algae()
white_2 <- white %>% filter_to_algae()

# save 
dir.create(here("data/03_filtered/18s_algae"))


write_csv(depth_2, here("data/03_filtered/18s_algae/18s_algae.depth_rel_abund.csv"))
write_csv(main_2, here("data/03_filtered/18s_algae/18s_algae.filtered_cleaned_rel_abund.csv"))
write_csv(protocol_2, here("data/03_filtered/18s_algae/18s_algae.protocol_rel_abund.csv"))
write_csv(scrape_2, here("data/03_filtered/18s_algae/18s_algae.scrape_rel_abund.csv"))
write_csv(water_2, here("data/03_filtered/18s_algae/18s_algae.water_rel_abund.csv"))
write_csv(white_2, here("data/03_filtered/18s_algae/18s_algae.white_rel_abund.csv"))
