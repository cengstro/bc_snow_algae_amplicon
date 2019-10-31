# Wrangle rbcL and 18S ASV tables and taxonomy

library(tidyverse)


# data in ------------
seq.rbcl.2019 <- read.csv("dat/ky_rbcL_seqtab.nochim.txt", sep=" ", stringsAsFactors = F)
idtax <- read_csv("dat/assignments_9-18.csv") # thresh = 50

asv.tbl.18s <- read.csv("dat/ky_18s_seqtab.nochim_April2019.txt", sep=" ", stringsAsFactors = F)
s18_taxa_silva <- read_csv("dat/18s_assignments_silva.csv") # thresh 40

field <- read_csv("dat/field_2018.csv")
cellct <- read_csv("dat/cellct_2018.csv")


# rbcL relative abund --------------------

unfilt_rbcl_rel_abund <- seq.rbcl.2019 %>%
  t() %>% 
  as_tibble(rownames = "asv") %>% 
  gather(key = "sample_id", val = "n_reads", -asv)

# format sample ids properly
unfilt_rbcl_rel_abund <- unfilt_rbcl_rel_abund %>% 
  mutate(sample_id = str_extract(sample_id, ".*(?=_S.*_)") %>% # remove underscores
           str_replace_all("-","."), # replace hyphen
         protocol = str_extract(sample_id, "(?<=[:alpha:]{3}18\\..{2}\\.p)[:digit:]"), # parse protocol version used
         sample_id = str_replace(sample_id, ".p[:digit:]",""), # remove p1
         protocol = replace_na(protocol, 3)) # replace NA with 3

# compute new variable "rel_abund" 
############### consider normalizing with metagenomeSeq ( i think this weights for 0s somehow...)

unfilt_rbcl_rel_abund <- unfilt_rbcl_rel_abund %>% 
  group_by(sample_id, protocol) %>% 
  mutate(rel_abund = n_reads/sum(n_reads)) %>% 
  ungroup()

# Filter out controls, and samples with low reads ---------------------------

# set aside and remove protocol controls
protocol.ids <- unfilt_rbcl_rel_abund %>% 
  select(sample_id, protocol) %>% 
  distinct() %>% 
  dplyr::count(sample_id) %>% 
  filter(n>1) %>% # choose if multiple protocols associated with a sample
  pull(sample_id)
protocol_comparison <- unfilt_rbcl_rel_abund %>% 
  filter(sample_id %in% protocol.ids)

# manually look for "protocol" samples to remove
protocol_comparison %>% 
  select(sample_id, protocol) %>% 
  distinct() %>% 
  group_by(sample_id) %>% 
  mutate(min=as.numeric(protocol) %>% min() %>% as.character(), 
         is_min = if_else(protocol == min, TRUE, FALSE)) %>% 
  filter(is_min == TRUE) %>% 
  select(1,2)
# ... and remove them from the main tbl manually
rbcl_rel_abund <- unfilt_rbcl_rel_abund %>%
  filter(!(sample_id == "gar18.01" & protocol == "2"),
         !(sample_id == "sax18.01" & protocol == "1"),
         !(sample_id == "sey18.25" & protocol == "1"),
         !(sample_id == "sey18.74" & protocol == "1"),
         !(sample_id == "sky18.12" & protocol == "1") )


# water negative controls
water_ctrls <- rbcl_rel_abund %>% 
  filter(sample_id %>% str_detect("water"))
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(!(sample_id %>% str_detect("water")))

# white snow samples
white_snow_samples <- rbcl_rel_abund %>% 
  filter(sample_id %in% c("bre18.17", "bre18.22.bb"))
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter( !(sample_id %in% c("bre18.17", "bre18.22.bb")))

# scraped vs unscraped comparison
scrape_comparison <- rbcl_rel_abund %>% 
  filter(sample_id %>% str_detect("sky18.24"))
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(!(sample_id == "sky18.24sc"))

# set aside depth samples
depth_sample_rel_abund <- rbcl_rel_abund %>% 
  filter(sample_id %in% c("hol18.42", "hol18.42b", "sey18.66", "sey18.66b", "hol18.21", "hol18.39d"))
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(!(sample_id %in% c("hol18.42", "sey18.66b","hol18.39d")))

# remove samples with low read count
rbcl_rel_abund %>% 
  group_by(sample_id, protocol) %>% 
  summarise(sum=sum(n_reads)) %>% 
  arrange(sum)
# remove failed sample lib18.01
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(sample_id != "lib18.01" )

# remove outlier sey18.22a, likely cross contam
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(sample_id != "sey18.22a")



# now remove rows that contain 0s
rbcl_rel_abund <- rbcl_rel_abund %>% 
  filter(n_reads!=0)

rbcl_rel_abund <- rbcl_rel_abund %>% 
  select(-protocol)

rbcl_rel_abund


# rbcl IDTaxa assignment -----------

idtax <- idtax %>% 
  dplyr::rename(domain=V2, kingdom=V3, phylum=V4,class=V5 ,order=V6, family=V7, genus=V8, species=V9) %>% 
  dplyr::rename(domainConf=V21,kingdomConf=V31, phylumConf=V41, classConf = V51, orderConf=V61, familyConf=V71, genusConf=V81, speciesConf=V91) %>% 
  mutate_if(is.character, funs(replace(., str_detect(., "unclassified"), NA_character_))) %>% 
  mutate(id_level = case_when(is.na(domain) ~"root",
                              is.na(kingdom) ~"domain",
                              is.na(phylum)~"kingdom",
                              is.na(class)~"phylum",
                              is.na(order)~"class",
                              is.na(family)~"order",
                              is.na(genus)~"family",
                              is.na(species)~"genus",
                              TRUE ~ "species")) %>% 
  mutate(assignment = case_when(id_level =="root"~"root",
                                id_level == "domain"~ domain,
                                id_level == "kingdom"~kingdom,
                                id_level == "phylum"~phylum,
                                id_level == "class"~class,
                                id_level == "order"~order,
                                id_level == "family"~family,
                                id_level == "genus"~genus,
                                id_level == "species"~species))
idtax %>% nrow() # 652 ASVs
# remove ASVs not found in snow samples
idtax <- idtax %>% 
  filter(asv %in% rbcl_rel_abund$asv)
idtax %>% nrow()

# make rbcl asv id key
rbcl_asv_id_key <- idtax %>% 
  select(asv) %>% 
  rownames_to_column("asv_id") %>% 
  mutate(asv_id = str_pad(asv_id,3,pad="0")) %>% 
  arrange(asv_id)

# add in asv_id to asv tbl
rbcl_rel_abund <- rbcl_asv_id_key %>% 
  right_join(rbcl_rel_abund, by="asv") %>% 
  arrange(asv_id) %>% 
  select(-asv)

# add asv_id to taxonomy tbl
idtax <- rbcl_asv_id_key %>% 
  right_join(idtax, by="asv") %>% 
  select(-asv) %>% 
  arrange(asv_id)

idtax %>% distinct(genus)

idtax <- idtax %>% 
  mutate(genus = case_when(str_detect(genus,"Stichococcus")~"Stichococcus",
                           str_detect(genus,"Myrmecia")~"Myrmecia",
                           str_detect(genus, "Dicty")~"Dictyochloropsis",
                           str_detect(genus,"Trebouxia")~"Trebouxia",
                           str_detect(family,"Stichococcus")~"Stichococcus", # a mis-alignment of columns in the ref db for this one
                           TRUE~genus),
         species = case_when(str_detect(genus,"Stichococcus")~NA_character_, # not named species, removing
                           TRUE~species),
         id_level = case_when(str_detect(genus,"Stichococcus")~"genus", # and update this
                              TRUE~id_level))
idtax %>% distinct(species)

# 18S relative abundances -----------
unfilt_18s_ra_2019 <- asv.tbl.18s %>%
  t() %>% 
  as_tibble(rownames = "asv")

# change column names
colnames(unfilt_18s_ra_2019)[-1] <- colnames(unfilt_18s_ra_2019)[-1] %>% 
  str_remove("_S.*_") %>% # remove _S*_ ending
  str_replace_all("-",".") #replace hyphen with period

# remove duplocate protocols, remove p from name
colnames(unfilt_18s_ra_2019) # look for dups
unfilt_18s_ra_2019 <- unfilt_18s_ra_2019 %>% 
  select(-gar18.01.p2, -sax18.01.p1, -sey18.25.p1, -sey18.74.p1, -sky18.12.p1)

# now remove p from sample Id
colnames(unfilt_18s_ra_2019)[-1] <- colnames(unfilt_18s_ra_2019)[-1] %>% 
  str_remove("\\.p[:digit:]")

# tidy long form, remove remaining samples not in rbcl data set (water, etc)
s18_rel_abund <- unfilt_18s_ra_2019 %>% 
  gather(key = "sample_id", val = "n_reads", -asv) %>% 
  filter(sample_id %in% rbcl_rel_abund$sample_id)

# add percent asv
s18_rel_abund <- s18_rel_abund %>% 
  group_by(sample_id) %>% 
  mutate(rel_abund = n_reads/sum(n_reads)) %>% 
  ungroup()

# n reads per sample
s18_rel_abund %>% 
  group_by(sample_id) %>% 
  summarise(sum_n = sum(n_reads)) %>% 
  arrange(sum_n) # max 76k, min 21k reads per sample

s18_rel_abund <- s18_rel_abund %>% 
  filter(sample_id != "gar18.04") %>% # remove gar4, no reads (?)
  filter(rel_abund != 0) # remove 0s

# 18S taxonomy assignments ---------
s18_taxa_silva
s18_taxa_silva <- s18_taxa_silva %>% 
  dplyr::rename(domain=V2,phylum=V3, class=V4 ,order=V5, family=V6, genus=V7, domainConf=V21,phylumConf=V31, classConf=V41, orderConf = V51, familyConf=V61 ,genusConf=V71) %>% 
  mutate_if(is.character, funs(replace(., str_detect(., "unclassified"), NA_character_))) %>% 
  mutate(id_level = case_when(is.na(kingdom) ~"domain",
                              is.na(class)~"phylum",
                              is.na(order)~"class",
                              is.na(family)~"order",
                              is.na(genus)~"family",
                              is.na(species)~"genus",
                              TRUE ~ "species")) %>% 
  mutate(assignment = case_when(id_level == "domain"~ domain,
                                id_level == "phylum"~phylum,
                                id_level == "class"~class,
                                id_level == "order"~order,
                                id_level == "family"~family,
                                id_level == "genus"~genus,
                                id_level == "species"~species))
s18_taxa_silva
##### some weird additions to the 18s taxonomy tbl from SILVA, _fa, _ge, only in some cases...

# make rbcl asv id key
s18_asv_id_key <- s18_taxa_silva %>% 
  select(asv) %>% 
  rownames_to_column("asv_id") %>% 
  mutate(asv_id = str_pad(asv_id,3,pad="0")) %>% 
  arrange(asv_id)

# add in asv_id to asv tbl
s18_rel_abund <- s18_asv_id_key %>% 
  right_join(s18_rel_abund, by="asv") %>% # right_join just a shortcut so that asv_id is first column
  arrange(asv_id) %>% 
  select(-asv)

# add asv_id to taxonomy tbl
s18_taxa_silva <- s18_asv_id_key %>% 
  right_join(s18_taxa_silva, by="asv") %>% 
  select(-asv) %>% 
  arrange(asv_id)


# wrangle field data ------------


## should do this in the data sheet directly??
field <- field %>%
  mutate(mountain = str_sub(sample_id, 1,3),
         color_snow = case_when(color_snow %in% c("white","yellow") ~ "other",
                                color_snow %in% c("purple","red") ~ "pink", 
                                is.na(color_snow) ~ "other",
                                TRUE ~ color_snow),
         habitat = if_else(is.na(habitat), "other", habitat),
         bloom_type = case_when(appearance == "runnel" & !str_detect(sample_id, "sky")~ "runnel",
                                color_snow == "rusty" ~ "rusty",
                                habitat == "alpine" & color_snow %in% c("pink","orange") ~ "pink_alpine",
                                habitat == "alpine" & color_snow == "green" ~ "green_alpine",
                                habitat %in% c("forest", "clearing") & color_snow == "green" ~ "green_forest",
                                habitat %in% c("forest", "clearing") & color_snow %in% c("pink", "orange") ~ "pink_forest",
                                TRUE ~ "other"))

# further facet y axis by chlain and non chlain, and by sax1 into own cat,
field <- field %>%
  mutate(bloom_type = if_else(bloom_type != "pink_alpine", bloom_type, case_when(sample_id == "sax18.01"~"pink_alpine_redwall",
                                                                                 dom_morph == "balloon"~"pink_alpine_Chlainomonas",
                                                                                 TRUE~"pink_alpine")))
# change "other" cat to fit with the others
field <- field %>% 
  mutate(bloom_type = case_when(sample_id == "pan18.ro"~ "pink_alpine_Chlainomonas",
                         bloom_type == "other" ~"pink_alpine",
                         TRUE~bloom_type))
# sey18.74 was mislabelled
field <- field %>% 
  mutate(bloom_type = case_when(sample_id == "sey18.74"~ "pink_forest",
                                TRUE~bloom_type))
# bre18.19 is alpine green
field <- field %>% 
  mutate(bloom_type = if_else(sample_id == "bre18.19","green_alpine",bloom_type))

# sey18.63o is orange, should be lumped in the "other" category with ones at bottom of heatmap
field <- field %>% 
  mutate(bloom_type = if_else(sample_id == "sey18.63o","orange_forest",bloom_type))

# for suspense, I'm going to lump the redwall with the pink_alpine in fig3
field <- field %>% 
  mutate(bloom_type = if_else(sample_id == "sax18.01","pink_alpine",bloom_type))





# cell count data -------


