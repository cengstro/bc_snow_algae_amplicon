library(tidyverse)
library(here)
library(metagenomeSeq)

seq.rbcl.2019 <- read.csv(here("data","raw","ky_rbcL_seqtab.nochim.txt"), sep=" ", stringsAsFactors = F) ##### KY saved as txt %^&*

# to long format
unfilt_rbcl_rel_abund <- seq.rbcl.2019 %>%
  t() %>% 
  as_tibble(rownames = "asv") %>% 
  gather(key = "sample_id", val = "n_reads", -asv)

# format sample ids properly
unfilt_rbcl_rel_abund <- unfilt_rbcl_rel_abund %>% 
  mutate(sample_id = str_extract(sample_id, ".*(?=_S.*_)") %>% # remove underscores
           str_replace_all("-","."), # replace hyphen
         protocol = str_extract(sample_id, "(?<=[:alpha:]{3}18\\..{2}\\.p)[:digit:]"), # parse protocol version used
         sample_id = str_replace(sample_id, ".p[:digit:]",""), # remove p1
         protocol = replace_na(protocol, 3)) # replace NA with 3

# compute new variable "rel_abund"





unfilt_rbcl_rel_abund <- unfilt_rbcl_rel_abund %>% 
  group_by(sample_id, protocol) %>% 
  mutate(rel_abund = n_reads/sum(n_reads)) %>% 
  ungroup()
unfilt_rbcl_rel_abund