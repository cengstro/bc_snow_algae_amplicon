library(tidyverse)
library(here)
library(metagenomeSeq)

seq.rbcl.2019 <- read.csv(here("data","raw","ky_rbcL_seqtab.nochim.txt"), sep=" ", stringsAsFactors = F)

# format for metagenomeSeq
MR.rbcl <- rbcl_rel_abund %>% 
  select(-rel_abund) %>% 
  spread(sample_id, n_reads, fill = 0) %>% # adds 0s back in
  column_to_rownames("asv_id") %>% 
  newMRexperiment()

rbcl_norm <- MR.rbcl %>% 
  cumNorm(p = cumNormStatFast(MR.rbcl)) %>% 
  MRcounts(norm=T) %>% 
  as_tibble(rownames = "asv_id") %>% 
  gather("sample_id","norm_reads", -asv_id)

rbcl_norm <- rbcl_norm %>% 
  filter(norm_reads!=0) %>% 
  group_by(sample_id) %>% 
  mutate(norm_rel_abund = norm_reads/sum(norm_reads))

rbcl_rel_abund %>% 
  left_join(rbcl_norm, by=c("sample_id","asv_id")) %>% 
  select(asv_id, sample_id, rel_abund, norm_rel_abund) %>% 
  mutate(same = if_else(rel_abund == norm_rel_abund, T, F),
         diff = rel_abund - norm_rel_abund) %>% 
  arrange(-diff)
filter(same==F)