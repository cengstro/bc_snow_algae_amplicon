library(tidyverse)

##### none of this is working

otus %>% distinct(otu_name)


otu_ord <- c("Chlainomonas A","Sanguina B", "Other Chloromonas","Chloromonas krienitzii","Chloromonas muramotoi C","Chloromonas D", "Algae E","Chloromonas F", "Trebouxiophyceae G")

barplot <- 


plot_rel_abund_bar <- function(rel_abund_tbl, species, species_order, sample_order){
  # input: long form tibble with
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
  
  ggplot(rel_abund_tbl, aes(x=alias %>% fct_reorder(elev_m), y=rel_abund)) +
    geom_bar(aes(fill=otu_name %>% fct_relevel(otu_ord)), stat="identity") + # classic stacked relative abundance plot
    coord_flip() +
    facet_grid(~otu_name %>% fct_relevel(otu_ord), switch = "x") +
    labs( x="Sample ID", tag="B") +
    cust_theme
}

plot_rel_abund_stacked_bar <- function(){
  
}