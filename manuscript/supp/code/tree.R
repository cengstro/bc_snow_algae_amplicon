# Make rbcl ASV tree
# 2020-01-10

library(tidyverse)
library(here)
library(ape)
library(ggtree)


# ML phylogenetic tree (IQTree) of rbcL ASVs that appear in filtered relative abundance dataset (613 tips)
tree <- read.tree(file=here::here("output/tree/rbcl/asv613.fasta.treefile")) 
# Taxonomy tbl of each ASV (652; includes ASVs filtered out in samples not used in final analysis)
taxonomy <- read_csv(here::here("data/02_tidied/tidied.rbcl_taxonomy.csv"))
# OTU of each ASV
otus <- read_csv(here::here("output/asv_otu_key.csv"))
rel_abund <- read_csv("data/rel_abund/3_filter/rbcl/rbcl_rel_abund.csv")


# Misc edits of tree annotation data

# rename one of the OTUs (should take care of this upstream)
otus <- otus %>% 
  mutate(otu_name= otu_name %>% str_remove("muramotoi "))

taxonomy <- taxonomy %>% 
  mutate(assignment = assignment %>% str_remove("root"))




# Plot tree ---------

# re-root tree
root <- taxonomy %>% 
  filter(class=="Trebouxiophyceae") %>% 
  head(1) %>% 
  pull(asv_id)

rbcl.tree <- rbcl.tree %>% root(outgroup = root, resolve.root=T)

rbcl_ggtree <- ggtree(rbcl.tree) # ignore warning message 

# format bayesian/bootstrap confidence for each node
boots <- rbcl_ggtree$data %>% 
  filter(isTip == FALSE) %>% 
  separate(label, c("boot", "bayes"), sep = "/") %>% #split label into boots and bayes vals
  filter(boot > 80) %>%
  mutate(boot = round(as.numeric(boot), digits = 0),
         insignificant_edge = if_else(boot > 80, "black", "red"))# round boot to nearest integer

# get rel abund for each asv
rel_abund_asv <- rel_abund %>% 
  group_by(asv_id) %>% 
  summarise(rel_abund=sum(rel_abund))

# format taxonomy, otu for each node
assignment <- taxonomy %>% 
  select(asv_id, assignment)
annotation <- rbcl_ggtree$data %>% 
  left_join(assignment, by=c("label"="asv_id")) %>% 
  left_join(otus, by=c("label"="asv_id")) %>% 
  left_join(rel_abund_asv, by=c("label"="asv_id"))

# specify colors
rbcl_otu_colors <- c("Chlainomonas A"="#ac71b5",
                     "Sanguina B"="#ff534a", 
                      "Chloromonas krienitzii"="#ffa54c","Chloromonas muramotoi C"="#87b1d4", "Chloromonas D"="#94cf92", "Chloromonas F"="#fdfd96", "Other Chloromonas"="#4aaf53", 
                     "Trebouxiophyceae G"="#975c5c",
                     "Unknown E"="grey60")


# plot tree
otu_tree <- rbcl_ggtree +
  geom_text2(data=boots, aes(label=paste0(boot,";",bayes)),  vjust= 0.01, hjust = 1, size=2) +
  geom_tippoint(data = annotation, aes(color= otu_name, size=rel_abund), alpha=1) + 
  geom_tiplab(data = annotation, aes(label = assignment), size=1) +
  theme(legend.position = c(0.2,0.7), 
        legend.title = element_blank(), 
        legend.key = element_blank()) +
  guides(alpha="none") +
  scale_color_manual(values= rbcl_otu_colors) +
  guides(size="none",colour = guide_legend(override.aes = list(size=5)))

otu_tree



ggsave(filename=here::here("manuscript/supp/intermediate/fig_S3_tree.pdf"), width = 200, height = 425, units="mm", dpi=300)
