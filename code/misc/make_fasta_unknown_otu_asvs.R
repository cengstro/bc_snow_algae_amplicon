# make fasta of Unknown OTU

library(tidyverse)
library(DECIPHER)

key <- read_csv(here("data/asv_seq_data/rbcl_asv_sequence_key.csv"))
otus <- read_csv(here("output/asv_otu_key.csv"))

unknown_otu <- otus %>% 
  filter(otu_name == "Unknown E") %>% 
  pull(asv_id)

unknown_seqs_DSS <- key %>% 
  filter(asv_id %in% unknown_otu) %>% 
  deframe() %>% 
  DNAStringSet()

writeXStringSet(unknown_seqs_DSS, here("output/unknown_otu_asvs.fasta"))
