# Stitch the SciBase StreamCat data together to match Flow Model Input Sample

# pull only variables we need and combine/merge

library(purrr)
library(tidyverse)
library(janitor)
library(glue)
library(here)
library(fs)

source("R/functions/f_functions.R")

# Read In Files -----------------------------------------------------------

dat_dir <- here("data_clean/scibase_flow")
dir_exists(dat_dir)

# get file list
clean_list <- get_zip_list(glue("{dat_dir}"), "*csv", recurse = FALSE)
head(clean_list)

# Get Variables of Interest -----------------------------------------------

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)

# Get Files Of Interest ---------------------------------------------------


ppt_files <- filter(clean_list, grepl("(?=.*ppt)(?=.*cat)", x = filename, perl=TRUE, ignore.case = TRUE))

tav_files <- filter(clean_list, grepl("(?=.*tav)(?=.*cat)", x = filename, perl=TRUE, ignore.case = TRUE))

run_files <- filter(clean_list, grepl("(?=.*run)(?=.*cat)", x = filename, perl=TRUE, ignore.case = TRUE))

# combine files
dat_files <- bind_rows(ppt_files, tav_files, run_files)

# Combine -----------------------------------------------------------------

# use purrr to read all in and combine?
cat_dat <- map(dat_files$path, ~read_csv(.x, show_col_types = FALSE))
cat_dat <- map(cat_dat, ~select(.x, (COMID:filename))) %>%
  map(~select(.x, -c(filename)))

map(cat_dat, ~names(.x))
map(cat_dat, ~dim(.x))
map(cat_dat, ~class(.x))
map(cat_dat, ~pluck(.x, "COMID")) %>% map(., ~class(.x))

# bind together (THIS BREAKS IF USING VROOM TO READ IN!!..weird)
cat_df <- cat_dat %>% reduce(left_join, by="COMID") # by="COMID"

dall <- names(cat_df) %>% as_tibble()

# write out
write_csv(dall, file="data_clean/lsh_scibase_var_names.csv")

# Read in Example Input for Models ----------------------------------------

input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

dsel <- names(input_sample) %>% as_tibble() %>% rename("input"=value)
View(dsel)



