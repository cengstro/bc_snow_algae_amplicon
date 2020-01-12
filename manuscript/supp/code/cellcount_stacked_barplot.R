# cell count pie charts

library(tidyverse)
library(here)


cellct <- read_csv(here::here("data/cellct/tidied_cellct.csv"))
field <- read_csv(here::here("data/field/tidy_field.csv"))

cellct <- cellct %>% 
  left_join(field, by="sample_id")


colors <- c("Chlainomonas rubra"="#ac71b5", #purple
            "Sanguina nivaloides"="#ff534a", #red
            # Chloromonas: pastel greens / blues / yellows
            "Chloromonas krienitzii"="#ffa54c",
            "Chloromonas cf. brevispina"="#80c786", "Chloromonas cf. nivalis"="#4aaf86", 
            #                 lt green                            teal
            "Other Chloromonas"="#4aaf53", "Chloromonas"="#4aaf53", 
            # greyish light green
            "Green cell"="#4aaf53", "Other"="grey60") #greys
# oops, ordered them backwards, too lazy to rewrite
colors <- colors %>% rev()


cellct_plot <- ggplot(cellct, aes(x=alias %>% fct_reorder(elev_m), 
                     y=percent_of_sample)) +
    geom_bar(aes(fill=morpho_sp %>% fct_relevel(names(colors))), position="stack", stat="identity") +
    theme_classic() +
    labs(x="Sample ID (in order of increasing elevation)", y="Relative abundance", fill="Cell morphology")+
    scale_fill_manual(values = colors) +
    theme(text = element_text(size=8),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))  
cellct_plot

# ggsave(filename = here::here("manuscript/supp/final/fig_S4_cellct.tiff"), width = 300, height = 100, units="mm", dpi=300)
