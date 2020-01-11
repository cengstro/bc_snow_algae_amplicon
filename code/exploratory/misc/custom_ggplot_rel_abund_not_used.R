# a few custom ggplot scripts set aside for later

otu_rel_abund <- rel_abund %>% 
  left_join(otus, by="asv_id") %>% 
  group_by(sample_id, otu_name) %>% 
  summarise(rel_abund = sum(rel_abund)) %>%   
  left_join(field, by="sample_id") %>%
  ungroup()

otu_rel_abund_round_to_1_percent <- otu_rel_abund %>% 
  mutate(rel_abund = if_else(rel_abund <0.01, 0.01, rel_abund)) # artificially boost rel abund <1% to 1% so its visible on plot

# check rel abund sums ~= 1 (will be slightly higher, due to rounding)
otu_rel_abund_round_to_1_percent %>%
  group_by(alias) %>%
  summarise(sum(rel_abund))

cust_theme <- theme(legend.position = "none",
                    panel.grid.major.x = element_blank(), # remove vertical lines in background
                    panel.grid.minor.x = element_blank(),
                    axis.line=element_blank(), # remove x axis
                    axis.title.x=element_blank(), 
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    panel.spacing = unit(-0.01, "lines"), # remove space between facet panels
                    strip.text.x = element_text(angle = 90, hjust=1.0, vjust = 0.8), # rotate facet labels 90
                    strip.background = element_blank()) # remove grey boxes around facet labs

otus %>% distinct(otu_name)


otu_ord <- c("Chlainomonas A","Sanguina B", "Other Chloromonas","Chloromonas krienitzii","Chloromonas muramotoi C","Chloromonas D", "Unknown E","Chloromonas F", "Trebouxiophyceae G")

barplot <- ggplot(otu_rel_abund_round_to_1_percent, aes(x=alias %>% fct_reorder(elev_m), y=rel_abund)) +
  geom_bar(aes(fill=otu_name %>% fct_relevel(otu_ord)), stat="identity") + # classic stacked relative abundance plot
  coord_flip() +
  facet_grid(~otu_name %>% fct_relevel(otu_ord), switch = "x") +
  labs( x="Sample ID", tag="B") +
  cust_theme
barplot
