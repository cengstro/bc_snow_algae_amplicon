 # wrangle silva 18s assignment table

library(tidyverse)
library(here)

path <- here("intermediate_output/asv_seq_key_18s.csv")
key <- read_csv(path)

path2 <- here("data/raw/18s_assignments_silva.csv")
silva <- read_csv(path2)

clean_silva_18s_assignments <- silva %>% 
  dplyr::rename(asv_sequence = asv) %>% 
  left_join(key, by="asv_sequence") %>% 
  select(-asv_sequence) %>% 
  dplyr::rename(kingdom = V2, 
                phylum = V3, 
                class = V4,
                order = V5,
                family = V6,
                genus = V7,
                kingdom_conf = V21, 
                phylum_conf = V31, 
                class_conf = V41,
                order_conf = V51,
                family_conf = V61,
                genus_conf = V71) %>% 
  select(asv_id, kingdom:genus_conf)

write_path <- here("data/clean/clean_silva_18s_assignments.csv")
write_csv(clean_silva_18s_assignments, write_path)
