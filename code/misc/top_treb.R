# what species of trebouxiophyceae in hi elev samples?

library(tidyverse)
library(here)
library(DECIPHER)

# rbcl
rel_abund <- read_csv(here("data/03_filtered/rbcl/filtered_cleaned_rbcl_rel_abund.csv"))
taxonomy <- read_csv(here("data/02_tidied/tidied.rbcl_taxonomy.csv"))
rbcl_key <- read_csv(here("data/asv_seq_data/rbcl_asv_sequence_key.csv"))

# 18S
s18_rel_abund <- read_csv(here("data/03_filtered/18s/filtered_cleaned_18s_rel_abund.csv"))
s18_taxonomy <- read_csv(here("data/02_tidied/tidied_silva_18s_assignments.csv"))
s18_key <- read_csv(here("data/asv_seq_data/asv_seq_key_18s.csv"))

#otus
rbcl_otus <- read_csv(here("output/asv_otu_key.csv"))

# blast the high elevation rbcl trebouxiophyceae
high_samples <- c("wed18.01", "wed18.02", "pan18.01")


treb_asvs <- taxonomy %>% 
  filter(class == "Trebouxiophyceae") %>% 
  pull(asv_id)

my_samples_treb <- rel_abund %>% 
  filter(sample_id %in% high_samples,
         asv_id %in% treb_asvs,
         rel_abund > 0.01) %>% 
  left_join(taxonomy, by="asv_id")

view(my_samples_treb)

top_asvs <- my_samples_treb %>% 
  pull(asv_id)

rbcl_key %>% 
  filter(asv_id %in% top_asvs) %>% 
  deframe() %>% 
  DNAStringSet() %>% 
  writeXStringSet(filepath = here("data/asv_seq_data/etc/treb.fasta"))
# blast the seqs
# raphidonema longiseta and nivale are top two for these



# ------------------
# 18s

s18_taxonomy %>% 
  view()

treb_tax <- s18_taxonomy %>% 
  filter(class %in% c("Trebouxiophyceae", "Chrysophyceae")) %>% 
  pull(asv_id)


s18_rel_abund %>% 
  filter(asv_id %in% treb_tax) %>% 
  left_join(s18_taxonomy, by="asv_id") %>% 
  view()
s18_key


# ----------------------
# check out Unknown E

rbcl_otus %>% 
  filter(otu_name == "Unknown E") %>% 
  left_join(rbcl_key) %>% 
  select(-otu_name) %>% 
  deframe() %>% 
  DNAStringSet() %>% 
  writeXStringSet(filepath = here("data/asv_seq_data/etc/unknown_E.fasta"))



# check how many 18s asvs are algae

s18_taxonomy %>% 
  view()


taxonomy %>% 
  view()
