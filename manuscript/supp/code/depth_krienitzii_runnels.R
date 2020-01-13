# C krienitzii depth vs surface sample figure

library(tidyverse)
library(here)
library(gridExtra)



rbcl_ra_depth <- read_csv(here::here("data/rel_abund/3_filter/rbcl/depth_rbcl_rel_abund.csv")) # only the "depth samples", see code/wrangle/filter_rbcl_rel_abund.R for selection criteria
rbcl_ra_main <- read_csv(here:::here("data/rel_abund/3_filter/rbcl/rbcl_rel_abund.csv"))
s18_all_rel_abund <- read_csv(here::here("data/rel_abund/3_filter/18s/depth_18s_rel_abund.csv"))
# rbcl_tax <- read_csv(here::here("data/taxonomy/tidied.rbcl_taxonomy.csv"))
s18_tax <- read_csv(here::here("data/taxonomy/tidied_silva_18s_assignments.csv"))
rbcl_otus <- read_csv(here::here("data/otus/otu_rbcl_asvs.csv"))
field <- read_csv(here::here("data/field/tidy_field.csv"))


# combine tbls, choose samples of interest
rbcl <- rbcl_ra_depth %>%
  # since there is overlap between this and the main one, select to avoid conflict
  filter(!(sample_id %in% c("hol18.21","sey18.66"))) %>% 
  bind_rows(rbcl_ra_main) %>%
  left_join(field, by="sample_id") %>%  # annoying redundant
  left_join(rbcl_otus, by="asv_id") %>% 
  filter(alias %in% c("S9.2", "S9.1", "S6.7", "S2.2")) %>% # choose just six samples
  # excluded H4.3, H4.4 because dominated by Chlainomonas, I want to focus on Chloro krienitzii samples
  mutate(depth = case_when(alias == "S9.2"~"surface", # add new category, whether its surface or subsurface
                           alias == "S9.1"~"depth",
                           alias == "S6.7"~"surface",
                           alias == "S2.2"~"depth"))

# rbcl %>% distinct(alias, depth, depth_cm)

# select variables of interest, group by otus
rbcl_1 <- rbcl %>% 
  group_by(alias, otu_name, depth) %>% 
  summarise(rel_abund= sum(rel_abund)) %>% 
  ungroup() %>% 
  select(alias, depth, otu_name, rel_abund) %>% 
  mutate(otu_name = otu_name %>% str_remove("muramotoi ")) %>% # should have dont this upstream
  mutate(alias = alias %>% str_replace("S9.1","S9.2")) # this is below S9.2, renaming it for plotting simplicity

# define plot colors
species_colors <- c("Unknown E"="grey60", "Green cell"="#4aaf53", "Other"="grey60",
                    "Trebouxiophyceae G"="#975c5c", "Trebouxiophyceae"="#975c5c",
                    "Other Chloromonas"="#4aaf53", "Chloromonas"="#4aaf53", 
                    "Chloromonas cf. nivalis"="#4aaf86", "Chloromonas cf. brevispina"="#80c786",
                    "Chloromonas F"="#fdfd96","Chloromonas D"="#94cf92", "Chloromonas C"="#87b1d4","Chloromonas krienitzii"="#ffa54c",
                    "Sanguina B"="#ff534a", "Sanguina nivaloides"="#ff534a",
                    "Chlainomonas A"="#ac71b5","Chlainomonas rubra"="#ac71b5")


# make components, export to inkscape

s6.7 <- rbcl_1 %>% 
  filter(alias=="S6.7")
s2.2 <- rbcl_1 %>% 
  filter(alias=="S2.2")
s9 <- rbcl_1 %>% 
  filter(alias %in% c("S9.2"))

theme_set(theme_classic())

legend_source_plot <- ggplot(s6.7, aes(x=alias, y=rel_abund, fill = otu_name %>% fct_relevel(names(species_colors))))+
  geom_bar(stat="identity") +
  labs(fill="OTU") +
  scale_fill_manual(values = species_colors) 

legend <- legend_source_plot %>% 
  cowplot::get_legend() %>% 
  ggpubr::as_ggplot()

s6.7_plot <- legend_source_plot +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

s2.2_plot <- ggplot(s2.2, aes(x=alias, y=rel_abund, fill = otu_name %>% fct_relevel(names(species_colors))))+
  geom_bar(stat="identity") +
  labs(x="Sample ID", y="Relative abundance", fill="OTU") +
  scale_fill_manual(values = species_colors) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

s9_plot <- ggplot(s9, aes(x=alias, y=rel_abund, fill = otu_name %>% fct_relevel(names(species_colors))))+
  geom_bar(stat="identity") +
  labs(x="Sample ID", y="Relative abundance", fill="OTU") +
  scale_fill_manual(values = species_colors) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  facet_grid(rows = vars(depth))



# save comonents

ggsave(here::here("manuscript/figs/intermediate/f5/legend.pdf"), plot=legend, width=2, height=3)
ggsave(here::here("manuscript/figs/intermediate/f5/s6.7.pdf"), plot=s6.7_plot, width=2, height=3)
ggsave(here::here("manuscript/figs/intermediate/f5/s2.2.pdf"), plot=s2.2_plot, width=2, height=3)
ggsave(here::here("manuscript/figs/intermediate/f5/s9.pdf"), plot=s9_plot, width=2, height=5)

