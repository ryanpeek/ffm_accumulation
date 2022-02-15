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
xwalk_join <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 2) %>%
  clean_names()

# get clim data
clim_final <- read_rds("data_clean/06_seas_prism_metrics_for_mod.rds")

# Model Input -------------------------------------------------------------
#
# # model input
# input_sample_clean <- read_csv("data_raw/sample.csv") %>%
#   clean_names()
#
# input_sample <- read_csv("data_raw/sample.csv")
#
# input_sample_clean_names <- names(input_sample_clean) %>% as_tibble() %>% rename("input"=value)
# #write_csv(input_sample_clean_names, file = "data_clean/07_model_input_metric_names_only.csv")
#
# input_sample_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)
#
# # bind names and use to xwalk back to original model input/output names
# input_mod_xwalk <- cbind(input_sample_names, input_sample_clean_names) %>%
#   rename(mod_names=1, mod_names_clean = 2)
#
# # Join Xwalks -------------------------------------------------------------
#
# xwalk_join <- left_join(xwalk,input_mod_xwalk, by=c("model_input_clean"="mod_names_clean")) %>%
#   select(model_input_clean, mod_input_raw=mod_names, dat_output:source_file)
#
# #write_csv(xwalk_join, "data_clean/07_final_xwalk_variables.csv")
#
# rm(xwalk, xwalk_rev, input_mod_xwalk, input_sample, input_sample_clean, input_sample_clean_names, input_sample_names)

# Get Files Of Interest ---------------------------------------------------

# first pull the non prism data:
files_needed <- xwalk_join %>% select(source_file) %>%
  filter(!is.na(source_file)) %>% distinct() %>%
  mutate(filepath = glue("{dat_dir}/{source_file}")) %>%
  filter(!source_file=="NA") %>%
  # filter out the YYYY files for RUN, TAV, PPT
  filter(!grepl("YYYY", source_file))

# use purrr to read all in and combine
dat <- map(files_needed$filepath,
           ~read_csv(.x, show_col_types = FALSE)) %>%
  reduce(left_join, by="COMID") %>%
  select(-c(starts_with("filename"), contains("NODATA"), source)) %>%
  clean_names()

# Need to Calc Elev Vars --------------------------------------------------

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

# make slope/elev as above
dat <- dat %>%
  mutate(elv_rng = cat_elev_max-cat_elev_min, .after="cat_elev_max")

# Join ALL ----------------------------------------------------------------

# now add prism data for full dataset
dat_final <- left_join(clim_final, dat, by="comid")
names(dat_final) # still lots of additional vars

rm(clim_final, dat)

# Select Vars of Interest -------------------------------------------------

# now select?
dat_sel <- dat_final %>%
  select(any_of(xwalk_join$dat_output))

# duplicates?
xwalk_join %>% filter(duplicated(xwalk_join$dat_output))
# cat_wtdep # using one for area weighted avg and one as is (value for catch)

# Write Out ---------------------------------------------------------------

write_csv(dat_sel, file = "data_clean/07_final_catchment_data_for_accum.csv")

