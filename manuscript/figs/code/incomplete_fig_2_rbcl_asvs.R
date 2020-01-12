# Fig 2 rbcL ASVs

library(tidyverse)
library(here)
library(DECIPHER)
library(ggforce)
library(Rtsne)
library(dbscan)
library(ggrepel)


rel_abund <- read_csv(here::here("data/rel_abund/3_filter/rbcl/rbcl_rel_abund.csv"))
taxonomy <- read_csv(here::here("data/taxonomy/tidied.rbcl_taxonomy.csv"))
aligns <- readDNAStringSet(here::here("data/fastas/alignments/rbcl_aligns_in_main.fasta"))
seq <- read_csv(here::here("data/fastas/asv_seq_data/asv_seq_key_18s.csv"))
                           

# make distance matrix
BrowseSeqs(aligns, highlight = T)

dists <-  aligns %>% 
  DECIPHER::DistanceMatrix(type="matrix", correction = "JC")
