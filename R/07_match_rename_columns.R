
# Libraries ---------------------------------------------------------------



library(readxl)
library(tidyverse)
library(glue)

# Read in -----------------------------------------------------------------

# this is old needs updating
nhd_dat <- read_csv("data_clean/lsh_shasta_nhdv2_streamcat_vars.csv")

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)

# model input
input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

input_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)



# Gather vars? ------------------------------------------------------------


# need 1950-2015 for months and water year
