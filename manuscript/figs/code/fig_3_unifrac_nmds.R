# make UniFrac NMDS

library(tidyverse)
library(here)
library(GUniFrac)
library(vegan)
library(ggrepel)
library(picante)
library(gridExtra)


rel_abund <- read_csv(here::here("data/rel_abund/3_filter/rbcl/rbcl_rel_abund.csv")) # not looking at the depth samples, controls, etc
tax <- read_csv(here::here("data/taxonomy/tidied.rbcl_taxonomy.csv"))
field <- read_csv(here::here("data/field/tidy_field.csv"))
asv_seqs <- read_csv(here::here("data/fastas/asv_seq_data/rbcl_asv_sequence_key.csv"))
otus <- read_csv(here::here("data/otus/otu_rbcl_asvs.csv"))
rbcl.tree <- read.tree(here::here("data/tree/rbcl/asv613.fasta.treefile")) ### this tree should only contain the asvs we found in the "main" sites, not looking at controls, etc


ra_wide <- rel_abund %>% 
  select(sample_id, asv_id, rel_abund) %>% 
  arrange(asv_id) %>% 
  pivot_wider(names_from = asv_id, values_from = rel_abund, values_fill = list(rel_abund = 0)) %>% 
  column_to_rownames("sample_id")

# root tree with Trebouxiophyceae
root <- tax %>% 
  filter(class=="Trebouxiophyceae") %>% 
  head(1) %>% 
  pull(asv_id)
rbcl.tree <- rbcl.tree %>% root(outgroup = root, resolve.root=T)


# check for differences between asvs in tree and rel abund table (all names must appear in tree)
# asvs_rel_abund <- colnames(ra_wide)
# asvs_tree <- rbcl.tree$tip.label %>% sort()
# 
# length(asvs_rel_abund)
# length(asvs_tree)
# 
# setdiff(asvs_rel_abund, asvs_tree) # output elts unique to asvs_rel_abund

# run weighted UniFrac
set.seed(123)
unifracs <- ra_wide %>% 
  GUniFrac::GUniFrac(rbcl.tree, alpha=1) # ignore error

unifrac <- unifracs$unifracs[, , "d_1"]


# run NMDS ordination on results 
set.seed(123)
nmds <- metaMDS(unifrac, autotransform = F)


# plot
unifrac_plot_data <- nmds %>% 
  scores() %>% 
  as_tibble(rownames = "sample_id") %>% 
  left_join(field, by="sample_id")

# plot unifrac, colored by elevation
uf_plot <- ggplot(unifrac_plot_data, aes(x=NMDS2, y=NMDS1)) +
  geom_point(aes(color=elev_m), size=2.5, alpha=0.75) +
  scale_color_gradientn(colours = terrain.colors(8)[1:7]) +
  labs(color="Elevation (m)") +
  geom_text_repel(aes(label=alias), size=2, segment.size = 0.3) +
  theme_classic(base_size=8)
  # theme(axis.text=element_text(size=8),
  #       legend.text = element_text(size=8))

uf_plot

ggsave(filename = here::here("manuscript/figs/intermediate/fig_3.png"), plot=uf_plot, width = 85, height = 70, dpi=300, units="mm")
