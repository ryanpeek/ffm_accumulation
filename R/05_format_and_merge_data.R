# Stitch the SciBase StreamCat data together to match Flow Model Input Sample

# pull only variables we need and combine/merge

library(purrr)
library(tidyverse)
library(vroom)
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
cat_dat <- map(dat_files$path, ~vroom(.x, show_col_types = FALSE))

# bind together
alldat <- tst %>% reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source"))) %>%
  clean_names() %>%
  rename(sid=id) %>%
  # drop dup cols:
  select(-c(starts_with("x"), starts_with("id")))

dall <- names(alldat) %>% as_tibble() %>% rename("all"=value)

# write out
write_csv(alldat_combine, file = glue("{outdir}/scibase_data_merged_by_comids.csv"))

write_csv(dall, file="data_clean/lsh_scibase_var_names.csv")

# Read in Example Input for Models ----------------------------------------

input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

dsel <- names(input_sample) %>% as_tibble() %>% rename("input"=value)
View(dsel)

write_csv(dsel, file="data_clean/lsh_sample_input_var_names.csv")

# Compare column names ----------------------------------------------------

filter(dsel, input %in% dall$all) # only two??
# all names are slightly_diff...of course

# Write it Out ------------------------------------------------------------

write_csv(alldat, file="data_clean/lsh_scibase_vars.csv")
write_rds(alldat, file="data_clean/lsh_scibase_vars.rds")



