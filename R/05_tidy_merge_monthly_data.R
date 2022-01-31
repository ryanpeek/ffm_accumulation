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

# write out just names
write_csv(dall, file="data_clean/lsh_scibase_var_names.csv")

# Write Out Raw Data ----------------------------------------------------------

write_rds(cat_df, file = "data_clean/05_lsh_scibase_mon_raw_ppt_tav_run.rds")

cat_df <- read_rds("data_clean/05_lsh_scibase_mon_raw_ppt_tav_run.rds")


# Format ------------------------------------------------------------------

# wide format with each COMID repeated by row for each diff WY

# need to match format of input sample for models
input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

# make name vector from sample model input
input_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)

# cat_df needs to be renamed and made "semi_long" instead of wide.

cat_df_long <- cat_df %>%
  # drop 2016 and 2017 first:
  dplyr::select(-c(matches("2016|2017"))) %>%
  # then split and pivot
  pivot_longer(!COMID,
               names_to = c("metric", "month","wa_yr"),
               names_pattern = "CAT_([[:alpha:]]{3})_([[:alpha:]]{3})([[:digit:]]{4})",
               #names_prefix = "CAT_",
               values_to = "value") %>%
  janitor::clean_names() %>%
  mutate("comid_wy" = glue("{comid}_{wa_yr}"), .after="comid",
         "wa_yr" = as.integer(wa_yr),
         var_mon_wy = tolower(glue("{metric}_{month}_wy"))) %>%
  select(comid, comid_wy, wa_yr, var_mon_wy, value) %>%
  # add pwy
  arrange(comid, var_mon_wy, wa_yr) %>%
  mutate(var_mon_pwy = lag(value))


# SEASONAL METRICS --------------------------------------------------------

# now need to add seasonal groups based on water year:
## Fall: Oct, Dec, Nov,
## Winter: Jan, Feb, Mar
## Spring: Apr, May, Jun
## Summer: Jul, Aug, Sep

# variables we need to calc?
## sum WY: ann_wy (sum of all months) / ## sum PWY: ann_pwy (sum of all months)
## seasonal WY: sum of each season (fall_wy, wint_wy, sprg_wy, summ_wy)
## seasonal PWY: sum (fall_pwy, wint_pwy, sprg_pwy, summ_pwy)

## quarterly sum: sum1/sum2/sum3/sum4

# Read in Example Input for Models ----------------------------------------

input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

input_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)




# ARCHIVED Calc Monthly: All Years ----------------------------------------------------

# function to calc a month for all water years
# avg_month <- function(month, var, df){
#   # select the cols for that month and var
#   month <- toupper(month)
#   var <- toupper(var)
#   df_sel <- dplyr::select(df, COMID, contains(month))
#   df_sel <- df_sel %>% dplyr::select(COMID, contains(var))
#   # drop 2016 and 2017 for now
#   df_sel <- df_sel %>% dplyr::select(-c(matches("2016|2017")))
#
#   # calc avg for all years (using rowMeans)
#   df_avg <- df_sel %>%
#     mutate("{var}_{month}_wy" := rowMeans(across(c(2:ncol(.)))), .after="COMID") %>%
#     select(1:2) %>%
#     janitor::clean_names()
#   return(df_avg)
# }
#
# ## NOTES AND TESTING
# # testing to convert to check single row:
# # df_sel %>% ungroup() %>% slice(1) %>% select(2:6) %>% t() %>% mean()
# # calc avg for all years (using across and rowwise, more flexible for diff functions)
# #df_avg2 <- df_sel %>%
# #  rowwise(COMID) %>%
# #  summarize("{var}_{month}_wy" := mean(c_across(everything())))
#
# # crossing to get all possible combos arranged by var
# df_x <- crossing("month"=c(factor(x = month.abb, levels = month.abb)),
#                  "var"=c("ppt", "run", "tav")) %>%
#   arrange(var)
#
# # now apply function
# mon_all <- map2(df_x$month, df_x$var,
#                 ~avg_month(month = .x, var = .y, df = cat_df)) %>%
#   reduce(left_join, by="comid")
#

