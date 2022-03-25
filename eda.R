library(dplyr)
library(tidyr)
library(assertr)

data <- read.csv("DATAHOME/click_through_regression/training_data.csv")

data %>%  
 verify(has_all_names("ordered", "device_mobile", "device_computer", "device_tablet", "returning_user")) %>%
 verify(sum(ordered, nr.rm=TRUE) > 1000) %>%
 group_by(ordered, loc_uk) %>%
 tally() %>%
 spread(loc_uk, n)
