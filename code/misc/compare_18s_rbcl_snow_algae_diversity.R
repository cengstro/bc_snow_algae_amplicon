# n differences between rbcl species on genbank

# align snow algae rbcl, align, trim to region between our primers, calculate distance matrix (in num bp diff) for each
library(tidyverse)
library(DECIPHER)
library(broom)
library(here)
library(magrittr)



gb <- readDNAStringSet(here("reference_database/genbank_snow_algae_fastas/snow_algae_rbcl.fasta")) # max 2000 bp len
s18 <- readDNAStringSet(here("reference_database/genbank_snow_algae_fastas/chloromonas18s.fasta"))
my_asvs <- readDNAStringSet(here("data/asv_seq_data/rbcl_main.fasta"))
my_18s_asvs <- readDNAStringSet(here("data/asv_seq_data/algae_18s_seqs.fasta"))






length(gb)
length(s18)

# shorten names
short_names <- names(gb) %>%
  str_split(" ") %>% 
  purrr::map(extract, 1:3) %>% # select first name elements
  purrr::map(paste, collapse = "_")  %>% # seperate by underscores
  unlist()

# remove N gaps
gb <- gb %>%
  str_replace_all("N", '') %>%
  DNAStringSet()

# assign short names
names(gb) <- short_names # add names AFTER removng gaps (converting XStringSet to vector will remove the names)

# align asvs
aligns <- gb %>% 
  append(my_asvs[1:50]) %>% # add my seqs to see where to trim
  AlignSeqs()
aligns %>% BrowseSeqs(highlight = T) # check

# trim to region between primers
names <- names(aligns)
aligns <- aligns %>% 
  str_sub(484, 896) %>% 
  DNAStringSet()
names(aligns) <- names
aligns %>% BrowseSeqs(highlight = T) # check

# remove low coverage genbank refs, and remove my primers
aligns <- aligns[1:80]
aligns <- aligns[-str_which(names(aligns), "AB434274.1|AB434275.1")]

# filter aligns to one of each species
unique_sp_indices <- names(aligns) %>% 
  str_split("_") %>% 
  purrr::map(magrittr::extract, 3) %>%
  unlist() %>% 
  enframe() %>% 
  group_by(value) %>% 
  sample_n(1) %>% #grab one of each species
  ungroup() %>% 
  select(name) %>% 
  deframe() # grab these indices

# make distance matrix
dists <- aligns[unique_sp_indices] %>%
  DistanceMatrix(type="dist") %>% 
  tidy(diag=F, upper=F) %>% 
  mutate(distance = distance * width(aligns)[1]) # multiply each by 413 to get bp diffs


# visualise as heatmap
plot_genus <- "Chloromonas|Chlainomonas"

chloro_chlain_dists <- dists %>%
  filter(str_detect(item1,plot_genus),
         str_detect(item2, plot_genus))

ggplot(chloro_chlain_dists, aes(x=item1, y=item2)) +
  geom_tile(aes(fill = distance)) +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))

# view as histogram
ggplot(chloro_chlain_dists, aes(x=distance)) +
  geom_histogram() +
  xlab("Number of base pair differences") +
  ylab("Frequency")

summarise(chloro_chlain_dists, min =min(distance), max = max(distance))

# possible redundant species names... only include Matsuzaki names that seperate out in multi-marker study

plot_species <- "muramotoi|miwae|pichinchae|fuku|tugh|tenuis|hoham|krienitzii|chenan|remias|rubra"

matsuzaki_dists <- dists %>%
  filter(str_detect(item1,plot_species),
         str_detect(item2, plot_species))

ggplot(matsuzaki_dists, aes(x=item1, y=item2)) +
  geom_tile(aes(fill = distance)) +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))

# view as histogram
bp_diffs_rbcl <- ggplot(matsuzaki_dists, aes(x=distance)) +
  geom_histogram() +
  xlab("Number of base pair differences") +
  ylab("Number of snow algae species")
bp_diffs_rbcl

# only 3 less than 20, 9 between (20, 30], most (30,40]

# but changing d in swarm dosent mean that it will seperate exactly thus... but gives us an idea of where to start











# Compare with 18s----

# shorten names
short_names_s18 <- names(s18) %>%
  str_split(" ") %>% 
  purrr::map(magrittr::extract, 1:3) %>% # select first name elements
  purrr::map(paste, collapse = "_")  %>% # seperate by underscores
  unlist()

# remove N gaps
s18 <- s18 %>%
  str_replace_all("N", '') %>%
  DNAStringSet()

# assign short names
names(s18) <- short_names_s18 # add names AFTER removng gaps (converting XStringSet to vector will remove the names)

# remove gaps (whoops)
degapped_18s_asvs <- my_18s_asvs %>% 
  RemoveGaps()

# align asvs
aligns_s18 <- s18 %>% 
  append(degapped_18s_asvs) %>% # add my seqs to see where to trim
  AlignSeqs()
aligns_s18 %>% BrowseSeqs(highlight = T) # check

# trim to region between primers
names <- names(aligns_s18)
aligns_s18 <- aligns_s18 %>% 
  str_sub(1378, 1930) %>% 
  DNAStringSet()
names(aligns_s18) <- names
aligns_s18 %>% BrowseSeqs(highlight = T) # check


LC438439.1


# remove low coverage genbank refs, and remove my primers
aligns <- aligns[1:80]
aligns <- aligns[-str_which(names(aligns), "AB434274.1|AB434275.1")]

# filter aligns to one of each species
unique_sp_indices <- names(aligns) %>% 
  str_split("_") %>% 
  map(magrittr::extract, 3) %>%
  unlist() %>% 
  enframe() %>% 
  group_by(value) %>% 
  sample_n(1) %>% #grab one of each species
  ungroup() %>% 
  select(name) %>% 
  deframe() # grab these indices

# make distance matrix
dists <- aligns[unique_sp_indices] %>%
  DistanceMatrix(type="dist") %>% 
  tidy(diag=F, upper=F) %>% 
  mutate(distance = distance * width(aligns)[1]) # multiply each by 413 to get bp diffs


# visualise as heatmap
plot_genus <- "Chloromonas|Chlainomonas"

chloro_chlain_dists <- dists %>%
  filter(str_detect(item1,plot_genus),
         str_detect(item2, plot_genus))

ggplot(chloro_chlain_dists, aes(x=item1, y=item2)) +
  geom_tile(aes(fill = distance)) +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))

# view as histogram
ggplot(chloro_chlain_dists, aes(x=distance)) +
  geom_histogram() +
  xlab("Number of base pair differences") +
  ylab("Frequency")

summarise(chloro_chlain_dists, min =min(distance), max = max(distance))

# possible redundant species names... only include Matsuzaki names that seperate out in multi-marker study

plot_species <- "muramotoi|miwae|pichinchae|fuku|tugh|tenuis|hoham|krienitzii|chenan|remias|rubra"

matsuzaki_dists <- dists %>%
  filter(str_detect(item1,plot_species),
         str_detect(item2, plot_species))

ggplot(matsuzaki_dists, aes(x=item1, y=item2)) +
  geom_tile(aes(fill = distance)) +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 90))

# view as histogram
ggplot(matsuzaki_dists, aes(x=distance)) +
  geom_histogram() +
  xlab("Number of base pair differences") +
  ylab("Frequency")

distinct(matsuzaki_dists, item1)
