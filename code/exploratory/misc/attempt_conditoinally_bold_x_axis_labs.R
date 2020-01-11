# toy -------------
data <- tibble(labs = c("Oranges", "Apples", "Cucumbers"), counts = c(5, 10, 12), bold = c(T, F, F))

# breaks <- levels(data$labs)
# str(breaks)
# Reference breaks by name

ggplot(data = data) + 
  geom_bar(aes(x = labs, y = counts), stat="identity") + 
  # scale_x_discrete(label = labels, breaks = breaks) +
  theme(=
          



# my case -------------

samples_w_seq <- rel_abund %>% 
  distinct(sample_id) %>% 
  pull()

p <- cellct %>% 
  left_join(field, by="sample_id") %>% 
  mutate(have_seq = if_else(sample_id %in% samples_w_seq, TRUE, FALSE))

morpho_order <- c("Green cell",
                  "Sanguina 'citrine'",
                  "Sanguina nivaloides",
                  "Sanguina nivaloides 'turret'",
                  "Green cell",
                  "Chlainomonas",
                  "Chloromonas krienitzii",
                  "Chloromonas cf. brevispina",
                  "Chloromonas cf. nivalis",
                  "Chloromonas 'spiny'",
                  "Chloromonas 'oval'",
                  "Chloromonas 'zuke'"
)

to_bold <- p %>% 
  distinct(sample_id, have_seq, elev_m) %>% 
  arrange(elev_m) %>% 
  pull(have_seq)

my_theme <- theme(axis.text.x = element_text(hjust=1,angle=45),
                  axis.text.y = element_text(face=if_else(to_bold == T, "bold", "plain"),
                                             color = if_else(to_bold == T, "black", "grey")))

ggplot(p, aes(y=sample_id %>% fct_reorder(elev_m),
              x=morpho_sp)) +
  geom_tile(aes(alpha = percent_of_sample, fill = morpho_sp)) + # color = "white" %>% fct_reorder(morpho_order)
  labs(title="Cell count relative abundance", x="Cell morphology", y= "Sample ID", fill = "Cell morphology", alpha="Relative abundance") +
  guides(fill=F) +
  scale_alpha(range = c(0.1, 1)) +
  my_theme
