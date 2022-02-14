# pull only variables we need and combine/merge

suppressPackageStartupMessages({
  library(purrr)
  library(tidyverse)
  library(janitor)
  library(glue)
  library(here)
  library(fs)
})

# Get Data ----------------------------------------------------------------

# get ppt/tav/run data for monthly
cat_df <- read_rds("data_clean/05_lsh_scibase_mon_raw_ppt_tav_run.rds")

# Get Model Inputs --------------------------------------------------------

# wide format with each COMID repeated by row for each diff WY

# need to match format of input sample for models
input_sample <- read_csv("data_raw/sample.csv") %>%
  clean_names()

# make name vector from sample model input
input_names <- names(input_sample) %>% as_tibble() %>% rename("input"=value)

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 2)

# Make Long ---------------------------------------------------------------

# make made "semi_long" instead of wide, row for each year and comid

cat_df_long <- cat_df %>%
  # drop 2016 and 2017 first:
  dplyr::select(-c(matches("2016|2017"))) %>%
  # then split and pivot
  pivot_longer(!COMID,
               names_to = c("metric", "month","wa_yr"),
               names_pattern = "CAT_([[:alpha:]]{3})_([[:alpha:]]{3})([[:digit:]]{4})",
               values_to = "value") %>%
  janitor::clean_names() %>%
  # fix names to reflect sample model needs
  mutate("comid_wy" = glue("{comid}_{wa_yr}"), .after="comid",
         "wa_yr" = as.integer(wa_yr),
         var_mon_wy = tolower(glue("{metric}_{month}_wy"))) %>%
  select(comid, comid_wy, wa_yr, month, metric, var_mon_wy, value) %>%
  # add pwy
  arrange(comid, var_mon_wy, wa_yr) %>%
  mutate(value_pwy = lag(value))

# celebrate!
praise::praise()

# SEASONAL METRICS --------------------------------------------------------

# now need to add seasonal groups based on water year:
## Fall: Oct, Dec, Nov,
## Winter: Jan, Feb, Mar
## Spring: Apr, May, Jun
## Summer: Jul, Aug, Sep

# TO CALC
## seasonal WY: sum of each season (fall/wint/sprg/summ)
## seasonal PWY: sum (fall/wint/sprg/summ)
## sum WY: ann_wy (sum of all months), & prev water year (pwy)

cat_df_seas <- cat_df_long %>%
  mutate(season = case_when(
    grepl("JAN|FEB|MAR", month, ignore.case = TRUE) ~ "wint",
    grepl("APR|MAY|JUN", month, ignore.case = TRUE) ~ "sprg",
    grepl("JUL|AUG|SEP", month, ignore.case = TRUE) ~ "summ",
    grepl("OCT|NOV|DEC", month, ignore.case = TRUE) ~ "fall"
  )) %>%
  group_by(comid_wy, metric) %>%
  mutate(ann_wy = sum(value),
         ann_pwy = sum(value_pwy)) %>%
  ungroup() %>%
  group_by(comid_wy, metric, season) %>%
  mutate(seas_wy = sum(value),
         seas_pwy = sum(value_pwy)) %>%
  ungroup()

# Now Pivot Wide Again to Match Model Input -------------------------------

cat_metrics_wy <- cat_df_seas %>%
  pivot_wider(id_cols = c(comid, comid_wy, wa_yr),
              names_from = c(metric, month),
              names_glue = "{metric}_{month}_{.value}",
              values_from = c(value, value_pwy, ann_wy, ann_pwy)) %>%
  rename_with(tolower) %>%
  rename_with(., ~gsub("_value$", "_wy", .x, perl = TRUE)) %>%
  rename_with(., ~gsub("_value_", "_", .x)) %>%
  # probably better way to do this but easy for now...
  # just pull one ann col and rename
  select(comid:tav_sep_pwy,
         ppt_ann_wy = ppt_apr_ann_wy,
         run_ann_wy = run_apr_ann_wy,
         tav_ann_wy = tav_apr_ann_wy,
         ppt_ann_pwy = ppt_apr_ann_pwy,
         run_ann_pwy = run_apr_ann_pwy,
         tav_ann_pwy = tav_apr_ann_pwy
         )

names(cat_metrics_wy)

# now do same for seas!
cat_seas_wy <- cat_df_seas %>%
  select(comid:wa_yr, metric, season, seas_wy, seas_pwy) %>%
  distinct(.keep_all = TRUE) %>%
  pivot_wider(id_cols = c(comid, comid_wy, wa_yr),
              names_from = c(metric, season),
              names_glue = "{metric}_{season}_{.value}",
              values_from = c(seas_wy, seas_pwy)) %>%
  rename_with(tolower) %>%
  rename_with(., ~gsub("_seas_wy$", "_wy", .x, perl = TRUE)) %>%
  rename_with(., ~gsub("_seas_pwy", "_pwy", .x))
names(cat_seas_wy)

# Join and add Quarterly sums -----------------------------------------------

clim_df_full <- left_join(cat_metrics_wy, cat_seas_wy) %>%
  ## add quarterly sums: sum1/sum2/sum3/sum4
  #sum1 <- ann_wy + ann_pwy
  #sum2 <- ann_wy + summ_pwy
  #sum3 <- ann_wy + sprg_pwy
  #sum4 <- ann_wy + wint_pwy
  mutate(ppt_sum1 = ppt_ann_wy + ppt_ann_pwy,
         ppt_sum2 = ppt_ann_wy + ppt_summ_pwy,
         ppt_sum3 = ppt_ann_wy + ppt_sprg_pwy,
         ppt_sum4 = ppt_ann_wy + ppt_wint_pwy,
         #tav
         tav_sum1 = tav_ann_wy + tav_ann_pwy,
         tav_sum2 = tav_ann_wy + tav_summ_pwy,
         tav_sum3 = tav_ann_wy + tav_sprg_pwy,
         tav_sum4 = tav_ann_wy + tav_wint_pwy,
         #run
         run_sum1 = run_ann_wy + run_ann_pwy,
         run_sum2 = run_ann_wy + run_summ_pwy,
         run_sum3 = run_ann_wy + run_sprg_pwy,
         run_sum4 = run_ann_wy + run_wint_pwy)

names(clim_df_full)

# Match Names -------------------------------------------------------------

clim_final <- clim_df_full %>%
  select(any_of(names(input_sample))) # match col order in sample input

# check names
names(clim_final)

# check names
janitor::compare_df_cols(clim_df_full, clim_final)
## dropped the fall, oct/nov/dec pwy vars, n=117 vs. n=105

# WRITE IT OUT!! ----------------------------------------------------------

write_csv(clim_final, file = "data_clean/06_seas_prism_metrics_for_mod.csv")
write_rds(clim_final, file = "data_clean/06_seas_prism_metrics_for_mod.rds")


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
#ppt_files <- filter(clean_list, grepl("(?=.*ppt)(?=.*cat)", x = filename, perl=TRUE, ignore.case = TRUE))


