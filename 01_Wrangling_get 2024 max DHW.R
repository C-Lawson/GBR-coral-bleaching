# 13 May 2024 Chris Lawson

# Extract only 2024 max DHW data

library(tidyverse)

load("gbr_shape_dhw.RDS")

dat <- as.data.frame(gbr_shape_dhw)

dat %>%
  select(gbr_name,
         id,
         geometry,
         dhw_max_2024) -> dat2
