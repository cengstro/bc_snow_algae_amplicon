# tidy rbcl assignment data

library(tidyverse)
library(here)
library(Biostrings)

raw_assignments <- read_csv(here("data/01_raw/rbcl_taxonomy_genbank.csv"))

assignments <- raw_assignments %>% 
  # rename columns
  dplyr::rename(asv_sequence=asv,
                domain=V2, 
                kingdom=V3, 
                phylum=V4,
                class=V5,
                order=V6, 
                family=V7, 
                genus=V8, 
                species=V9, 
                domainConf=V21,
                kingdomConf=V31, 
                phylumConf=V41, 
                classConf = V51, 
                orderConf=V61, 
                familyConf=V71, 
                genusConf=V81, 
                speciesConf=V91) %>% 
  # replace default IDTaxa output "unclassified" with NA
  mutate_if(is.character, funs(replace(., str_detect(., "unclassified"), NA_character_))) %>% ### funs deprecated
  # make new col detecting assignment level
  mutate(id_level = case_when(is.na(domain) ~"root",
                              is.na(kingdom) ~"domain",
                              is.na(phylum)~"kingdom",
                              is.na(class)~"phylum",
                              is.na(order)~"class",
                              is.na(family)~"order",
                              is.na(genus)~"family",
                              is.na(species)~"genus",
                              TRUE ~ "species")) %>% 
  # make new col with best assignment
  mutate(assignment = case_when(id_level =="root"~"root",
                                id_level == "domain"~ domain,
                                id_level == "kingdom"~kingdom,
                                id_level == "phylum"~phylum,
                                id_level == "class"~class,
                                id_level == "order"~order,
                                id_level == "family"~family,
                                id_level == "genus"~genus,
                                id_level == "species"~species))

assignments %>% view()
assignments %>% nrow()



# replace the actual sequence with a sequence identifier
asv_id <- assignments %>% 
  select(asv_sequence) %>% 
  rownames_to_column("asv_id") %>% 
  mutate(asv_id = str_pad(asv_id,3,pad="0"))


# add asv_id to taxonomy tbl
assigns_w_asv_id <- asv_id %>% 
  left_join(assignments, by="asv_sequence") %>% # do it in this order so that asv_id is first column
  select(-asv_sequence)

assigns_w_asv_id %>% view()
assigns_w_asv_id %>% distinct(genus)
assigns_w_asv_id %>% distinct(species)


# Manually change some of the taxonomy that was incorrectly labelled
fixed_annotations <- assigns_w_asv_id %>% 
  mutate(genus = case_when(str_detect(genus,"Stichococcus")~"Stichococcus",
                           str_detect(genus,"Myrmecia")~"Myrmecia",
                           str_detect(genus, "Dicty")~"Dictyochloropsis",
                           str_detect(genus,"Trebouxia")~"Trebouxia",
                           str_detect(family,"Stichococcus")~"Stichococcus", # a mis-alignment of columns in the ref db for this one
                           TRUE~genus),
         species = case_when(str_detect(genus,"Stichococcus")~NA_character_, # not named species, removing
                             TRUE~species),
         id_level = case_when(str_detect(genus,"Stichococcus")~"genus", # and update this
                              TRUE~id_level))


# make new categorical to tell ggplot what taxonomy to plot
# include both species and genus level assignments
# lump low abundance Trebouxiophyceae
misc_treb <- c("Myrmecia","Trebouxia","Coccomyxa","Dictyochloropsis","Diplosphaera")
plot_taxonomy <- fixed_annotations %>% 
  mutate(plot_taxa = case_when(genus %in% misc_treb ~"other Trebouxiophyceae",
                                  genus == "Stichococcus" ~ "Stichococcus",
                                  id_level == "species" ~ assignment,
                                  id_level == "genus" ~ assignment))

# species list
plot_taxonomy %>% distinct(genus)
plot_taxonomy %>% distinct(species)
plot_taxonomy %>% distinct(plot_taxa)



write_csv(plot_taxonomy, here("data/02_tidied/tidied.rbcl_taxonomy.csv"))
write_csv(asv_id, here("data/asv_seq_data/rbcl_asv_sequence_key.csv"))

# make this into a fasta as well
asv_id %>% 
  deframe() %>% 
  DNAStringSet() %>% 
  writeXStringSet(here("data/asv_seq_data/rbcl.fasta"))
