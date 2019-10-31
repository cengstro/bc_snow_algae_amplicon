# Tidy 18s relative abundance data

library(tidyverse)
library(here)

rel_abund <- read.csv(here("data/01_raw/ky_18s_seqtab.nochim_April2019.txt"), sep=" ", stringsAsFactors = F)
key <- read_csv(here("output/asv_seq_key_18s.csv"))

# this is basically the same script as for rbcl... 

# to long format
rel_abund_1 <- rel_abund %>% 
  t() %>% 
  as_tibble(rownames = "asv_sequence") %>% 
  pivot_longer(cols = -asv_sequence, names_to = "sample_protocol_id", values_to = "n_reads") %>% 
  drop_na() %>% 
  filter(n_reads !=0)
head(rel_abund_1)

# clean sample names
rel_abund_2 <- rel_abund_1 %>%
  mutate(sample_protocol_id = sample_protocol_id %>% 
           str_extract(".*(?=_S.*_)") %>% # remove underscores
           str_replace_all("-",".") %>%  # replace hyphen
           tolower())
head(rel_abund_2)

# compute relative abundance
rel_abund_3 <- rel_abund_2 %>% 
  group_by(sample_protocol_id) %>% 
  mutate(rel_abund = n_reads/sum(n_reads)) %>% 
  ungroup()

rel_abund_3 %>% distinct(sample_protocol_id) %>% view()

# set aside actual sequences, replace with asv_id
rel_abund_4 <- rel_abund_3 %>% 
  left_join(key, by="asv_sequence") %>% 
  select(sample_protocol_id, asv_id, n_reads, rel_abund) %>% 
  arrange(sample_protocol_id)
head(rel_abund_4)


write_csv(rel_abund_4, here("data/02_tidied/tidied.18s_rel_abund.csv"))
