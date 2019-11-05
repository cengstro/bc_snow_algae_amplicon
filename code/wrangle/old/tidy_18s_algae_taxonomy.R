# tidy 18s taxonomy-- just snow algae
library(tidyverse)
library(here)

taxonomy_tbl <- read_csv(here("data/01_raw/taxonomy_18s_genbank_snow_algae.csv"))
asv_seq_data <- read_csv(here("data/asv_seq_data/asv_seq_key_18s.csv"))



taxonomy_tbl %>% distinct(V2, V3, V4, V5, V6, V7, V8, V9) %>% view()


  
# rename cols in taxonomy tbl
cleaned <- taxonomy_tbl %>% 
  select(-V2, -V3, -V4, -V21, -V31, -V41) %>% # only interested in assignments at class level or below (Chlorophyceae vs Trebouxiophyceae)
  dplyr::rename(asv_sequence = asv,
                class = V5,
                order = V6,
                family = V7, 
                genus = V8, 
                species = V9,
                class_conf = V51,
                order_conf = V61,
                family_conf = V71, 
                genus_conf = V81, 
                species_conf = V91) %>% 
  mutate_at(c("class","order","family","genus","species"),
            ~str_replace(., pattern="unclassified", replacement = NA_character_)) %>% 
  filter(!(is.na(class)))


cleaned %>% view()


### testing
tax <- cleaned %>% select(asv_sequence) %>% pull()
key <- asv_seq_data %>% select(asv_sequence) %>% pull()

setdiff(tax, key)
setdiff(key, tax)

# they're 100% different





# replace sequence with asv_id
cleaned %>% 
  left_join(asv_seq_data, by="asv_sequence") %>% view()
  select(asv_id, class:species_conf)

  
  
