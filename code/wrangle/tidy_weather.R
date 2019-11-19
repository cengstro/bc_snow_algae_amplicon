# check out historical weather data from S2S region

library(tidyverse)
library(here)
library(lubridate)
library(hms)
# library(conflicted)  # this makes everything run really slow

conflicted::conflict_prefer("here","here")
conflicted::conflict_prefer("filter","dplyr")

dir <- here("data/01_raw/climate/")
grouse_path <- paste0(dir,"grouse_1127m.ascii")
strach_lo_path <- paste0(dir,"strachan_1220.ascii")
strach_hi_path <- paste0(dir,"strachan_1420m.ascii")
whistler_path <- paste0(dir,"whistler_1835.ascii")

my_paths <- list(grouse_path, strach_lo_path, strach_hi_path, whistler_path)

read_wx <- function(path){
  read_csv(path, 
           col_types = cols(
             time=col_datetime()),
           skip=1)
}

# read in the data
wx_list <- map(my_paths, read_wx)

# remove characters from numeric columns (necessary before binding rows)
chr_to_dbl <- function(tbl){mutate_if(tbl, is.character, as.numeric)}
wx_clean <- map(wx_list, chr_to_dbl)

# add in primary key for station metadata, make into a tbl
names(wx_clean) <- c("grouse","strachan_low","strachan_high","whistler")
wx_tbl <- bind_rows(wx_clean, .id = "station")

# rename a confusing variable
wx_date <- wx_tbl %>%
  dplyr::rename(datetime = time)

# todo: make the stachen MINIMUM AIR TEMPERATURE the same as MIN_TEMP

wx_date

write_csv(wx_date, here("data/02_tidied/tidy_wx.csv"))
