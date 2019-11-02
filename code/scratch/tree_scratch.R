# color edge red if boot < 80
# ggtree_conf <- rbcl_ggtree$data %>%
#   separate(label, c("boot", "bayes"), sep = "/") %>% #split label into boots and bayes vals
#   mutate(insignificant_edge = case_when((boot < 80 & isTip == FALSE)~"dotted",
#                                         TRUE~"solid"),
#          asv_id = case_when(isTip == TRUE~boot,
#                             TRUE~NA_character_)) %>% # round boot to nearest integer
#   left_join(otus, by="asv_id")
# 
# 
# 
# rbcl_ggtree %<+% ggtree_conf +
#   aes(linetype = I(insignificant_edge)) +
#   geom_tippoint(aes(color=otu_name),alpha=0.7, size=3) + #
#   theme(legend.position = c(0.2,0.7),
#         legend.title = element_blank(),
#         legend.key = element_blank())