# fig 4 relative abundance plots: rbcl, 18s, cellct

library(tidyverse)
library(here)
library(GUniFrac)
library(vegan)
library(ggrepel)



rbcl_rel_abund <- read_csv(here::here("data/rel_abund/3_filter/rbcl/rbcl_rel_abund.csv")) # not looking at the depth samples, controls..
rbcl_tax <- read_csv(here::here("data/taxonomy/tidied.rbcl_taxonomy.csv"))
rbcl_asv_seqs <- read_csv(here::here("data/fastas/asv_seq_data/rbcl_asv_sequence_key.csv"))
rbcl_otus <- read_csv(here::here("data/otus/otu_rbcl_asvs.csv"))
s18_rel_abund <- read_csv(here::here("data/rel_abund/3_filter/18s_algae/18s_algae_rel_abund.csv"))
s18_algae_taxonomy <- read_csv(here::here("data/taxonomy/algae_18s_assignments_snow_algae_genbank.csv"))
# s18_algae_select_taxonomy <- read_csv(here("output/algae_18s_assignments_w_select_snow_algae.csv")) #### this didnt work very well, need to make consensus for each species
field <- read_csv(here::here("data/field/tidy_field.csv"))
cellct <- read_csv(here::here("data/cellct/tidied_cellct.csv"))
rbcl_tree <- read.tree(here::here("data/tree/rbcl/all_asvs/rbcl_aligns_all.fasta.treefile")) ### this tree should only contain the asvs we found in the "main" sites, not looking at controls, etc
s18_tree <- read.tree(here::here("data/tree/algae_18s/algae_18s_seqs.fasta.treefile"))


# combine rbcl, 18s, and cellct into single tbl ----------

# rbcl, group by otu
rbcl_ra <- rbcl_rel_abund %>%
  left_join(rbcl_otus, by="asv_id") %>% 
  group_by(sample_id, otu_name) %>% 
  summarise(rel_abund = sum(rel_abund)) %>%   
  ungroup() %>% 
  mutate(marker="rbcL") %>% # add variable to facilitate joining tbls
  dplyr::rename(species = otu_name) %>% # rename variable to facilitate joining lates
  select(sample_id, marker, species, rel_abund) %>% 
  mutate(species = species %>% str_remove("muramotoi ")) # should do this upsteam

# 18s rel abund by genus
s18_ra <- s18_rel_abund %>% 
  left_join(s18_algae_taxonomy, by="asv_id") %>%  # probably could do more work to make a better 18s database... I never tried using consensus seqs
  group_by(sample_id, best_assignment) %>% 
  summarise(rel_abund = sum(rel_abund)) %>% 
  ungroup() %>% 
  # this to lump some of the Other categories (and rename "species" for joining later)
  mutate(species = case_when(best_assignment=="Sanguina_nivaloides"~"Sanguina nivaloides",
                             best_assignment %>% str_detect("Chloromonas")~"Chloromonas",
                             # best_assignment == "Chlorogonium"~"Chlorogonium",
                             best_assignment=="Koliellaceae"~"Trebouxiophyceae",
                             TRUE~"Other")) %>% 
  mutate(marker = "18S") %>%
  select(sample_id, marker, species, rel_abund)


# cell count data ready to join
cellct_ra <- cellct %>% 
  filter(sample_id %in% rbcl_rel_abund$sample_id) %>% # specify samples found in the other 2 tbls
  mutate(marker = "Cell morphology") %>% 
  dplyr::rename(species=morpho_sp, rel_abund=percent_of_sample) %>% 
  select(sample_id, marker, species, rel_abund)

# combine the tbls into one
combined <- bind_rows(cellct_ra, s18_ra, rbcl_ra) %>% 
  left_join(field, by="sample_id")



# plot ---------

# specify colors (after much trial and error)
species_colors <- c("Unknown E"="grey60", "Green cell"="#4aaf53", "Other"="grey60",
                    "Trebouxiophyceae G"="#975c5c", "Trebouxiophyceae"="#975c5c",
                    "Other Chloromonas"="#4aaf53", "Chloromonas"="#4aaf53", 
                    "Chloromonas cf. nivalis"="#4aaf86", "Chloromonas cf. brevispina"="#80c786",
                    "Chloromonas F"="#fdfd96","Chloromonas D"="#94cf92", "Chloromonas C"="#87b1d4","Chloromonas krienitzii"="#ffa54c",
                    "Sanguina B"="#ff534a", "Sanguina nivaloides"="#ff534a",
                    "Chlainomonas A"="#ac71b5","Chlainomonas rubra"="#ac71b5")


stacked_bar <- ggplot(combined, aes(x=alias %>% fct_reorder(elev_m), y=rel_abund)) +
  geom_bar(aes(fill=species %>% fct_relevel(names(species_colors))), position="stack", stat="identity") +
  # coord_flip() +
  facet_grid(rows = vars(marker %>% fct_relevel(c("rbcL","18S", "Cell morphology")))) +
  theme_classic() +
  labs(x="Sample ID", y="Relative abundance", fill="Taxa")+
  scale_fill_manual(values = species_colors) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))                 


stacked_bar                 


# save ----

# ggsave(filename=here::here("manuscript/figs/intermediate/fig_4_barplot_r_out.pdf"), plot=stacked_bar, width=180, height=180, units = "mm")

