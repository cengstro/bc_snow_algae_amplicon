# make fastas of sequences, aligned and unaligned

library(tidyverse)
library(here)
library(DECIPHER)

s18_asvs <- read_csv(here("data/asv_seq_data/asv_seq_key_18s.csv"))
rbcl_asvs <- read_csv(here("data/asv_seq_data/rbcl_asv_sequence_key.csv"))

algae_18s_taxonomy <- read_csv(here("data/02_tidied/tidied_silva_18s_assignments.csv"))
rbcl_main_rel_abund <- read_csv(here("data/03_filtered/rbcl/filtered_cleaned_rbcl_rel_abund.csv")) 


  
# convert to DNAStringSet
convert_csv_to_dss <- function(csv){
  csv %>% 
    deframe() %>% 
    DNAStringSet()
}

s18_dss <- s18_asvs %>% convert_csv_to_dss()
rbcl_dss <- rbcl_asvs %>% convert_csv_to_dss()
rbcl_main_dss <- 

# also get a fasta of just the algae 18S reads
algae_18s_asv_ids <- algae_18s_taxonomy %>% 
  filter(phylum == "Chlorophyta") %>% 
  pull(asv_id)

s18_algae_dss <- s18_asvs %>% 
  filter(asv_id %in% algae_18s_asv_ids) %>% 
  convert_csv_to_dss()

# ... and a fasta of the rbcl reads, just in the "main" analysis
rbcl_main_asvs <- rbcl_main_rel_abund %>% 
  distinct(asv_id) %>% 
  pull()

nrow(rbcl_asvs)
rbcl_main_dss <- rbcl_asvs %>% 
  filter(asv_id %in% rbcl_main_asvs) %>% 
  convert_csv_to_dss()
length(rbcl_main_dss)

# align (~30 seconds per alignment)
s18_aligns <- s18_dss %>% AlignSeqs(verbose=F)
rbcl_aligns <- rbcl_dss %>% AlignSeqs(verbose=F)
s18_algae_aligns <- s18_algae_dss %>% AlignSeqs(verbose = F)
rbcl_main_aligns <- rbcl_main_dss %>% AlignSeqs(verbose = F)

BrowseSeqs(s18_aligns, highlight = T)
BrowseSeqs(rbcl_aligns, highlight = T)
BrowseSeqs(s18_algae_aligns, highlight = T)
BrowseSeqs(rbcl_main_aligns, highlight = T)

# remove gaps

s18_algae_gappy_asvs <- c("508", "570", "581", "586", "587", "591")
s18_algae_quality_aligns <- s18_algae_aligns[!(names(s18_algae_aligns) %in% s18_algae_gappy_asvs)]


# rbcl_gappy_seqs <- c("534", "639", "646")




# write output

# fastas
writeXStringSet(s18_dss, filepath = here("data/asv_seq_data/all_18s.fasta"))
writeXStringSet(rbcl_dss, filepath = here("data/asv_seq_data/rbcl.fasta"))
writeXStringSet(s18_algae_dss, filepath = here("data/asv_seq_data/algae_18s.fasta"))
writeXStringSet(rbcl_main_dss, here("data/asv_seq_data/rbcl_main.fasta"))

# alignment fastas
writeXStringSet(s18_aligns, filepath = here("data/asv_seq_data/all_18s_aligns.fasta"))
writeXStringSet(rbcl_aligns, filepath = here("data/asv_seq_data/rbcl_aligns.fasta"))
writeXStringSet(s18_algae_aligns, filepath = here("data/asv_seq_data/algae_18s_aligns.fasta"))
writeXStringSet(rbcl_main_aligns, here("data/asv_seq_data/rbcl_main_aligns.fasta"))

# de-gapped alignment fastas
# writeXStringSet(, filepath = here("data/asv_seq_data/.fasta"))
# writeXStringSet(, filepath = here("data/asv_seq_data/.fasta"))
writeXStringSet(s18_algae_quality_aligns, filepath = here("data/asv_seq_data/algae_18s_quality_aligns.fasta"))
