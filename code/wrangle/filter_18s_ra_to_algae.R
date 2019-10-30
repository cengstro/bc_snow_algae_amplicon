# this script takes in the clean algae 18s relative abundance table, and returns a filtered version that only contains the Chlorophyta assigned by IDTaxa using SILVA

library(tidyverse)
library(here)

path <- here("data/clean/clean_18s_ra.csv")
ra <- read_csv(path)

path2 <- here("data/clean/clean_silva_18s_assignments.csv")
silva <- read_csv(path2)

filtered_rel_abund <- ra %>% 
  left_join(silva, by="asv_id") %>% 
  filter(phylum %>% str_detect("Chlorophyta"))

# check
filtered_rel_abund %>% 
  distinct(asv_id, phylum)

# remove the taxonomy columns
filtered_rel_abund_final <- filtered_rel_abund %>% 
  select(asv_id, sample_id, n_reads, rel_abund)

# write file
path_out <- here("data/clean/only_algae_18s_rel_abund.csv")
write_csv(filtered_rel_abund_final, path_out)
