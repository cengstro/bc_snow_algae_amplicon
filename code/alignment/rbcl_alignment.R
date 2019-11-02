# prep rbcL fasta for IQTree

library(tidyverse)
library(here)
library(DECIPHER)


rbcl <- readDNAStringSet(here("data/asv_seq_data/rbcl.fasta"))
asv_ids_main_analysis <- read_csv(here("data/03_filtered/rbcl/filtered_cleaned_rbcl_rel_abund.csv")) %>% distinct(asv_id) %>% pull()

# all alignments ------------
aligns <- rbcl %>% 
  AlignSeqs(verbose = F)

BrowseSeqs(aligns, highlight = T)


# remove gaps alignments ------------------
gappy_seqs <- c("534", "639", "646")

length(aligns)
aligns_no_gaps <- aligns[!(names(aligns) %in% gappy_seqs)]
length(aligns)

BrowseSeqs(aligns_no_gaps, highlight = T)

# just alignments in "main" analysis --------------
length(rbcl)
main_aligns <- rbcl[ names(rbcl) %in% asv_ids_main_analysis ] %>% 
  AlignSeqs(verbose = F)
length(main_aligns)




aligns %>% 
  writeXStringSet(filepath = here("output/tree/rbcl/rbcl_aligns_all.fasta"))

aligns_no_gaps %>% 
  writeXStringSet(filepath = here("output/tree/rbcl/rbcl_aligns_indels_removed.fasta"))

main_aligns %>% 
  writeXStringSet(filepath = here("output/alignments/rbcl_aligns_in_main.fasta"))
