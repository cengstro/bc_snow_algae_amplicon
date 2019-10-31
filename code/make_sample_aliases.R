# Make sample aliases

library(tidyverse)

field <- read_csv(here("data/02_tidied/field_clean.csv"))

# each mountain gets a letter, each date gets a number, each 

# assign unique mountain_letter code to each field site
field_1 <- field %>% 
  mutate(mountain_letter = case_when(str_detect(sample_id, "sax") ~ "X",
                                     str_detect(sample_id, "sey") ~ "S",
                                     str_detect(sample_id, "sky") ~ "K",
                                     str_detect(sample_id, "stm") ~ "M",
                                     TRUE ~ str_sub(sample_id, 0,1) %>% toupper()))


# which visit to the mountain (only 1 visit for most sites)
visit_number <- field_1 %>%
  distinct(mountain_letter, date) %>% 
  arrange(mountain_letter, date) %>% 
  group_split(mountain_letter) %>% 
  map_df(rownames_to_column, "visit_number") %>% 
  mutate(alias = paste0(mountain_letter, visit_number))
  
field_2 <- field_1 %>% 
  left_join(visit_number, by=c("mountain_letter", "date"))
  
# for each moutain visit, unique sample ID
alias <- field_2 %>% 
  arrange(-elev_m) %>% # from high to low since thats usually the order I collected in
  group_split(alias) %>% 
  map_df(rownames_to_column, "uid") %>% 
  mutate(alias = paste0(alias, ".", uid)) %>% 
  select(sample_id, alias)
  
  

write_csv(alias, here("output/sample_aliases.csv"))
