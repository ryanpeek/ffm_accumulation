# filter to variables of interest from download_scibase

library(readr)
library(glue)
library(dplyr)
library(here)
library(janitor)
library(purrr)
library(fs)


# Get XWalk Data ----------------------------------------------------------

# get list of var names:
varnames <- read_csv(here("data_raw/input_var_name_xwalk_sample_input_var_name.csv")) %>%
  clean_names()


# Get NHD Data ------------------------------------------------------------

dat_dir <- here("data_clean/scibase_flow/")
dir_exists(dat_dir)

# get col names from data
files_to_get <- list.files(path = dat_dir, pattern = "(.*).csv$")

# read in everything
df_all <- map(files_to_get, ~read_csv(glue("{dat_dir}/{.x}")))

# bind together (this takes awhile)
alldat <- df_all %>% reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source"), starts_with("filename"))) %>%
  clean_names() %>%
  rename(sid=id) %>%
  # drop dup cols:
  select(-c(starts_with("x"), starts_with("id")))

dall <- names(alldat) %>% as_tibble() %>% rename("all"=value) %>%
  filter(!grepl("^filename", all))

View(dall)

write_csv(dall, file="data_clean/nhd_v2_streamcat_var_names.csv")




