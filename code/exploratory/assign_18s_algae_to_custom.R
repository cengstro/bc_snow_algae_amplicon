# assign 18s algae to custom database

library(tidyverse)
library(DECIPHER)
library(here)

algae_18s <- readDNAStringSet(here("data/asv_seq_data/algae_18s.fasta")) 
reference <- readDNAStringSet(here("reference_database/snow_algae_18s_taxonomy.fasta"))
curated_ref <- readDNAStringSet(here("reference_database/select_snow_algae_refs.fasta"))



# train classifier and assign taxonomy
trained_reference <- curated_ref %>% 
  LearnTaxa(names(curated_ref))

assignments <- IdTaxa(algae_18s, trained_reference, threshold = 50)


# convert to tbl
taxonomy_tbl <- tibble(list = assignments) %>% 
  add_column(asv_id = names(assignments)) %>% 
  hoist(list,
        taxon = "taxon", 
        conf = "confidence") %>% 
  hoist(taxon,
        root = 1,
        domain = 2,
        kingdom = 3,
        phylum = 4,
        class=5,
        order=6,
        family=7,
        genus=8,
        species=9) %>% 
  hoist(conf,
        root_conf = 1,
        domain_conf = 2,
        kingdom_conf = 3,
        phylum_conf = 4,
        class_conf=5,
        order_conf=6,
        family_conf=7,
        genus_conf=8,
        species_conf=9)

taxonomy_select <- taxonomy_tbl %>% 
  select(asv_id, phylum, class, order, family, genus, species, phylum_conf, class_conf, order_conf, family_conf, genus_conf, species_conf)

label_na <- function(x){ 
  if_else(str_detect(x,"unclass"), NA_character_, x)
  }
taxonomy_na_labeled <- taxonomy_select %>% 
  mutate_at(2:7, label_na)

best_assignment <- taxonomy_na_labeled %>% 
  # make new col detecting assignment level
  mutate(id_level = case_when(is.na(class)~"phylum",
                              is.na(order)~"class",
                              is.na(family)~"order",
                              is.na(genus)~"family",
                              is.na(species)~"genus",
                              TRUE~"species")) %>% 
  # make new col with best assignment
  mutate(best_assignment = case_when(id_level == "class"~class,
                                     id_level == "order"~order,
                                     id_level == "family"~family,
                                     id_level == "genus"~genus,
                                     id_level=="species"~species))
best_assignment %>% view

# output
write_csv(best_assignment, path = here("output/algae_18s_assignments_w_select_snow_algae.csv"))
