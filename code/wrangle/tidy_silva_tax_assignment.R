 # wrangle silva 18s assignment table

library(tidyverse)
library(here)
library(Biostrings)

silva <- read_csv(here("data/01_raw/taxonomy_18s_silva.csv"))

silva_1 <- silva %>% 
  dplyr::rename(asv_sequence = asv,
                kingdom = V2, 
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
  # replace default IDTaxa output "unclassified" with NA
  mutate_if(is.character, funs(replace(., str_detect(., "unclassified"), NA_character_))) %>% ### funs deprecated
  # make new col detecting assignment level
  mutate(id_level = case_when(is.na(kingdom) ~"unknown",
                              is.na(phylum)~"kingdom",
                              is.na(class)~"phylum",
                              is.na(order)~"class",
                              is.na(family)~"order",
                              is.na(genus)~"family",
                              !is.na(genus)~"genus")) %>% 
  # make new col with best assignment
  mutate(assignment = case_when(id_level == "unknown"~ "unknown",
                                id_level == "kingdom"~kingdom,
                                id_level == "phylum"~phylum,
                                id_level == "class"~class,
                                id_level == "order"~order,
                                id_level == "family"~family,
                                id_level == "genus"~genus))
silva_1 %>% view()
silva_1 %>% nrow()

# fix some annotation quirks
silva_2 <- silva_1 %>% 
  mutate(phylum = str_remove(phylum, "_ph"),
         family = str_remove(family, "_fa"),
         class = if_else(str_detect(class, "Incertae"), NA_character_, class))


silva_2 %>% view()


# make a key of the sequences and asv_ids, and replace the actual sequence with a key
key <- silva_2 %>% 
  select(asv_sequence) %>% 
  rownames_to_column("asv_id") %>% 
  mutate(asv_id = str_pad(asv_id, 3, pad="0"))
head(key)

seqs_to_fasta <- key %>% 
  deframe() %>% 
  DNAStringSet()

# replace the actual sequence with the asv_id in the relative abund tbl
silva_3 <- key %>% 
  left_join(silva_2, by="asv_sequence") %>% 
  select(-asv_sequence)


write_csv(key, here("output/asv_seq_key_18s.csv"))
writeXStringSet(seqs_to_fasta, here("output/all_18s.fasta"))
write_csv(silva_3, here("data/02_tidied/tidied_silva_18s_assignments.csv"))
