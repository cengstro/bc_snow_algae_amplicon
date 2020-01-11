# which samples are in the main, and depth analysis (for looking for photos)
library(tidyverse)
library(here)

main <- read_csv(here("data/03_filtered/rbcl/filtered_cleaned_rbcl_rel_abund.csv"))
depth <- read_csv(here("data/03_filtered/rbcl/depth_rbcl_rel_abund.csv"))
field <- read_csv(here("data/02_tidied/tidied_field.csv"))

main %>% 
  bind_rows(depth) %>% 
  distinct(sample_id) %>% 
  view()

main %>% 
  distinct(sample_id) %>% 
  left_join(field, by="sample_id") %>% 
  arrange(-elev_m) %>% 
  select(sample_id, alias, elev_m) %>% 
  view()
  