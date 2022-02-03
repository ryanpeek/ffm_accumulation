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
write_csv(input_names, file = "data_clean/07_model_input_metric_names_only.csv")
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

#write_csv(dat, file = "data_clean/07_non_prism_metrics_for_mod.csv")

# make just names
dat_names <- names(dat) %>% as_tibble() %>% rename("input"=value)

write_csv(dat_names, file = "data_clean/07_non_prism_metric_names_only.csv")

# Need to Calc Elev Vars --------------------------------------------------

# model input needs this

#drain_sqkm
#slope_pct_30m
#elev_mean_m_basin_30m
#elev_min_m_basin_30m
#elev_max_m_basin_30m

# but data_input has this:
# cat_basin_area
# cat_basin_slope
# cat_elev_mean
# cat_elev_min
# cat_elev_max
# cat_stream_length


dat_rev <- dat %>%
  mutate(elv_rng = cat_elev_max-cat_elev_min) %>%
  rename(
    drain_sqkm = cat_basin_area,
    slope_pct_30m = cat_basin_slope,
    elev_mean_m_basin_30m = cat_elev_mean,
    elev_min_m_basin_30m = cat_elev_min,
    elev_max_m_basin_30m = cat_elev_max)


# Join ALL ----------------------------------------------------------------

dat_final <- left_join(clim_final, dat_rev, by="comid")

# Select Vars of Interest -------------------------------------------------

# now select?
dat_sel <- dat_final %>%
  select(any_of(names(input_sample)))

# things don't match up...need to fix or no?


# Write Out ---------------------------------------------------------------

write_csv(dat_final, file = "data_clean/07_final_catch_metrics_for_accum.csv")

