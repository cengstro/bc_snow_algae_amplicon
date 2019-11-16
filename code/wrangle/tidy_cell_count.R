# tidy cell count data

library(tidyverse)
library(here)

raw_cellct <- read_csv(here::here("data/01_raw/cellct_2018.csv"))
glimpse(raw_cellct)



thresh <- 50 # set this much lower than 100, to keep some counts with high fungi relative abundance

cellct <- raw_cellct %>%
  # drop non-algae categories
  select(-three_arm:-biofilm) %>% 
  # make long format
  pivot_longer(cols = -sample_id, names_to="morpho_sp", values_to="count") %>%
  # remove 0's
  filter(count != 0) %>% 
  drop_na() %>% 
  mutate(morpho_sp = case_when( str_detect(morpho_sp, "green")~"Green cell", # lump all green categories
                                morpho_sp=="orb"~"Sanguina nivaloides", 
                                morpho_sp=="ruby"~"Sanguina nivaloides", # assuming Sanguina grows turrets
                                morpho_sp=="balloon"~"Chlainomonas rubra",
                                morpho_sp=="tangerine"~"Chloromonas krienitzii",
                                morpho_sp=="hedgehog"~"Chloromonas cf. brevispina",
                                morpho_sp=="lemon"~"Chloromonas cf. nivalis",
                                morpho_sp=="spiny"~"Other",
                                morpho_sp=="oval"~"Other",
                                morpho_sp=="zuke"~"Other",
                                morpho_sp=="halo"~"Other",
                                morpho_sp=="unknown"~"Other",
                                morpho_sp=="citrine"~"Sanguina nivaloides",
                                TRUE ~ morpho_sp)) %>% 
  # drop samples with count < thresh
  group_by(sample_id) %>% 
  mutate(count_total = sum(count)) %>% 
  filter(count_total > thresh) %>% 
  # calculate percentage
  mutate(percent_of_sample = count / count_total) %>% 
  ungroup() %>% 
  # lump multiple rows of "Other" to same row
  group_by(sample_id, morpho_sp) %>%
  summarise(percent_of_sample=sum(percent_of_sample))

head(cellct)


# add in NA for wed2, sey7, and bre10
# nas <- tibble(sample_id = c("wed18.02","sey18.07","bre18.10"),
#               morpho_sp = NA_character_, 
#               count=NA_character_, 
#               count_total=NA_character_)
# 
# cellct_w_NAs <- cellct %>% 
#   bind_rows(nas)

write_csv(cellct, here::here("data/02_tidied/tidied.cell_count.csv"))
