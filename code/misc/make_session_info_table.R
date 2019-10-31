# print session info

library(sessioninfo)
library(here)
library(tidyverse)
library(kableExtra)

my_session_info <- sessioninfo::session_info()
  
my_session_info$platform %>% 
  unlist() %>% 
  enframe() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F, position = "left")



software <- my_session_info$packages %>% 
  as_tibble() %>% 
  select(package, loadedversion) %>% 
  dplyr::rename(version = loadedversion)

cedar_info <- tibble(package = c(""),
                     version = c(""))

software <- software %>% 
  bind_rows(cedar_info)

software %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F, position = "left")

