# tidy field data, adds sample aliases

library(tidyverse)
library(here)



field <- read_csv(here("data/01_raw/field_2018.csv"))

head(field)


# clean up a few small details
field_1 <- field %>%
  mutate(
    # lump some of the color snow categories
    color_snow = case_when(color_snow %in% c("white","yellow") ~ "other",
                           color_snow %in% c("purple","red") ~ "pink", 
                           is.na(color_snow) ~ "other",
                           TRUE ~ color_snow),
    # change NA habitat to other
    habitat = if_else(is.na(habitat), "other", habitat)
  )



# make sample aliases ----------------------
# each mountain gets a letter, each date gets a number, each sample gets unique ID 
# in the format B1.05 where B is mountain, 1 is the first visit, and 5 is the 5th sample collected that day

# assign unique mountain_letter code to each field site
field_2 <- field_1 %>% 
  mutate(mountain_letter = case_when(str_detect(sample_id, "sax") ~ "X",
                                     str_detect(sample_id, "sey") ~ "S",
                                     str_detect(sample_id, "sky") ~ "K",
                                     str_detect(sample_id, "stm") ~ "M",
                                     TRUE ~ str_sub(sample_id, 0,1) %>% toupper()))
field_2 %>% select(mountain_letter, sample_id)

# which visit to the mountain (only 1 visit for most sites)
visit_number <- field_2 %>%
  distinct(mountain_letter, date) %>% 
  arrange(mountain_letter, date) %>% 
  group_split(mountain_letter) %>% 
  map_df(rownames_to_column, "visit_number") %>% 
  mutate(alias_1 = paste0(mountain_letter, visit_number))

field_3 <- field_2 %>% 
  left_join(visit_number, by=c("mountain_letter", "date"))
field_3 %>% distinct(sample_id, date, mountain_letter, visit_number, alias_1) %>% arrange(date) %>% view()

# for each moutain visit, unique sample ID
field_4 <- field_3 %>% 
  arrange(-elev_m) %>% # from high to low since thats usually the order I collected in
  group_split(alias_1) %>% 
  map_df(rownames_to_column, "uid") %>% 
  mutate(alias = paste0(alias_1, ".", uid))
field_4 %>% distinct(sample_id, alias, date) %>% arrange(date) %>% view()

# remove some of the unnessecary columns
field_5 <- field_4 %>% 
  select(sample_id, alias, date, lat, lon, elev_m, color_snow, habitat, substrate, depth_cm, dom_morph)

write_csv(field_5, here("data/02_tidied/tidied_field.csv"))