# pull the additional metrics

# Libraries ---------------------------------------------------------------

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

# get xwalk from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)

# get clim data
clim_final <- read_rds("data_clean/06_seas_prism_metrics_for_mod.rds")

# Model Input -------------------------------------------------------------

# model input
input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

input_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)

# Get Files Of Interest ---------------------------------------------------

files_needed <- xwalk %>% select(source_file) %>%
  filter(!is.na(source_file)) %>% distinct() %>%
  mutate(filepath = glue("{dat_dir}/{source_file}"))

# use purrr to read all in and combine
dat <- map(files_needed$filepath,
           ~read_csv(.x, show_col_types = FALSE)) %>%
  reduce(left_join, by="COMID") %>%
  select(-c(starts_with("filename"), contains("NODATA"), source)) %>%
  clean_names()

# now select?
dat_sel <- dat %>%
  select(any_of(names(input_sample)))

# things don't match up...


# AAAAAAAAGGGGHHH ---------------------------------------------------------


