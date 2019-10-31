# tidy rbcl relative abundance data

library(tidyverse)
library(here)

raw <- read.csv(here("data/01_raw/ky_rbcL_seqtab.nochim.txt"), sep=" ", stringsAsFactors = F)
key <- read_csv(here("output/rbcl_asv_sequence_key.csv"))

rel_abund_long <- raw %>%
  t() %>% 
  as_tibble(rownames = "asv_sequence") %>% 
  gather(key = "sample_id", val = "n_reads", -asv_sequence) %>% 
  drop_na() %>% 
  filter(n_reads !=0)

# format sample ids (keep protocol for now ".p1" at end on some duplicate sequenced samples)
clean_sample_ids <- rel_abund_long %>%
  mutate(sample_id = sample_id %>% 
           str_extract(".*(?=_S.*_)") %>% # remove underscores
           str_replace_all("-",".") %>%  # replace hyphen
           tolower())

# compute relative abundance
percent_rel_abund <- clean_sample_ids %>% 
  group_by(sample_id) %>% 
  mutate(rel_abund = n_reads/sum(n_reads)) %>% 
  ungroup()

percent_rel_abund %>% distinct(sample_id) %>% view()

# set aside actual sequences, replace with asv_id
added_asv_id <- percent_rel_abund %>% 
  left_join(key, by="asv_sequence") %>% 
  select(asv_id, sample_id, n_reads, rel_abund) %>% 
  arrange(sample_id)

key %>% nrow()

added_asv_id %>% 
  distinct(asv_id) %>% 
  nrow()

write_csv(added_asv_id, here("data/02_tidied/tidied.rbcl_rel_abund.csv"))
