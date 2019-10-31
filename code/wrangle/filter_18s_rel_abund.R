# filter 18s dataset to just algae, and to just the samples in our rbcL dataset
# set aside other rel abund datasets

library(tidyverse)
library(here)


# input -----------------
rel_abund_in <- read_csv(here("data/02_tidied/tidied.18s_rel_abund.csv"))
head(rel_abund_in)

# protocols ------------------------
# parsed protocol vers
rel_abund_1 <- rel_abund_in %>%
  mutate(protocol = sample_protocol_id %>% 
           str_extract("(?<=\\.p)[:digit:]$"),
         sample_id = sample_protocol_id %>% 
           str_remove("\\.p[:digit:]$"),
         # if NA is protocol v3
         protocol = if_else(is.na(protocol), "3",protocol)) %>% 
  select(sample_id, protocol, asv_id, n_reads, rel_abund)
head(rel_abund_1)

# make a seperate tbl for protocol comparison
samples_w_mult_protocols <- rel_abund_1 %>% 
  distinct(sample_id, protocol) %>% 
  dplyr::count(sample_id) %>% 
  filter(n>1) %>% # choose if multiple protocols associated with a sample
  pull(sample_id)
samples_w_mult_protocols

protocol_comparison_rel_abund <- rel_abund_1 %>% 
  filter(sample_id %in% samples_w_mult_protocols)
protocol_comparison_rel_abund %>% distinct(sample_id)

# only keep the most recent protocol in my samples
protocol_comparison_rel_abund %>% 
  distinct(sample_id, protocol) %>% 
  arrange(sample_id, protocol)

rel_abund_2 <- rel_abund_1 %>%
  # filter out the lowest protocol vers used in the duplicate samples
  filter(!(sample_id == "gar18.01" & protocol == "2"),
         !(sample_id == "sax18.01" & protocol == "1"),
         !(sample_id == "sey18.25" & protocol == "1"),
         !(sample_id == "sey18.74" & protocol == "1"),
         !(sample_id == "sky18.12" & protocol == "1") )

# now can remove protocol from the tbl
rel_abund_2.5 <- rel_abund_2 %>% 
  select(-protocol)

# water negative controls ---------------

water_ctrls <- rel_abund_2.5 %>% 
  filter(sample_id %>% str_detect("water"))
rel_abund_3 <- rel_abund_2.5 %>% 
  filter(!(sample_id %>% str_detect("water")))

# white snow samples ---------------
white_snow_samples <- rel_abund_3 %>% 
  filter(sample_id %in% c("bre18.17", "bre18.22.bb"))
rel_abund_4 <- rel_abund_3 %>% 
  filter( !(sample_id %in% c("bre18.17", "bre18.22.bb")))

# scraped vs unscraped comparison -------------
scrape_comparison <- rel_abund_4 %>% 
  filter(sample_id %>% str_detect("sky18.24"))
rel_abund_5 <- rel_abund_4 %>% 
  filter(!(sample_id == "sky18.24sc"))

# set aside depth samples ------------------
depth_sample_rel_abund <- rel_abund_5 %>% 
  filter(sample_id %in% c("hol18.42", "hol18.42b", "sey18.66", "sey18.66b", "hol18.21", "hol18.39d"))
rel_abund_6 <- rel_abund_5 %>% 
  filter(!(sample_id %in% c("hol18.42", "sey18.66b","hol18.39d")))

# remove samples with low read count ---------------
rel_abund_6 %>% 
  group_by(sample_id, protocol) %>% 
  summarise(sum=sum(n_reads)) %>% 
  arrange(sum)
# remove failed sample lib18.01
rel_abund_7 <- rel_abund_6 %>% 
  filter(sample_id != "lib18.01" )

# remove outlier sey18.22a, likely cross contam ad hoc -----------
rel_abund_8 <- rel_abund_7 %>%
  filter(sample_id != "sey18.22a")

rel_abund_8 %>% nrow()
rel_abund_8 %>% distinct(sample_id) %>% nrow()
rel_abund_8 %>% distinct(asv_id) %>% nrow()

# Output -----------------
dir.create(here("data/03_filtered/18s"))

write_csv(protocol_comparison_rel_abund, here("data/03_filtered/18s/protocol_comparison_18s_rel_abund.csv"))
write_csv(water_ctrls, here("data/03_filtered/18s/water_controls_18s_rel_abund.csv"))
write_csv(white_snow_samples, here("data/03_filtered/18s/white_snow_18s_rel_abund.csv"))          
write_csv(scrape_comparison, here("data/03_filtered/18s/scrape_comparison_18s_rel_abund.csv"))
write_csv(depth_sample_rel_abund, here("data/03_filtered/18s/depth_18s_rel_abund.csv"))
write_csv(rel_abund_8, here("data/03_filtered/18s/filtered_cleaned_18s_rel_abund.csv"))
