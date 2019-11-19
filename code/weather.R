library(tidyverse)
library(here)
library(lubridate)
library(hms)
# library(conflicted)  # this makes everything run really slow

conflicted::conflict_prefer("here","here")
conflicted::conflict_prefer("filter","dplyr")


wx <- read_csv(here("data/02_tidied/tidy_wx.csv"))
field_2018 <- read_csv(here("data/02_tidied/tidied_field.csv"))

wx

early_summer_18 <- wx_date %>% 
  dplyr::filter(month(datetime) %>% between(month(05), month(07)),
                year(datetime) == 2018,
                station %in% c("grouse","whistler"))

view(early_summer_18)

ggplot(early_summer_18, aes(x=date(datetime), y=MIN_TEMP)) +
  geom_line() +
  facet_grid(vars(station %>% fct_relevel("whistler","grouse")), scales = "free_x")

view(field_2018)
