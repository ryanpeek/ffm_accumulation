# function to run accumulations

library(readr)
library(glue)
library(dplyr)
library(here)
library(janitor)
library(purrr)
library(sf)

# get hydroids
networkids <- read_csv(here("data_clean/lshasta_hydroid_network.csv"))

# get list of column names that require mean
varnames <- read_csv(here("data_raw/input_var_name_xwalk_sample_input_var_name.csv")) %>%
  clean_names() %>%
  filter(accumulation_operation == "avg of catchment values") %>%
  filter(!is.na(streamcat_source_var),
         !model_input %in% c("slope_pct_30m",
                             "elev_mean_m_basin_30m"))

# need to make cross walk between hydroid and comid!!! yarg
hydroid_lists <- map(networkids$paths_lst, ~unlist(strsplit(.x, ", ")))

# flowlines
hydroids <- read_rds(here("data_clean/final_flowlines_w_full_nhd_vaa.rds")) %>%
  select(id, comid, hydroseq, gnis_name, areasqkm, geom) %>%
  st_drop_geometry()

# filter to just comids: make cross walk between hydroid and comid!!!
comid_lists <- map(hydroid_lists, ~hydroids %>% filter(hydroseq %in% .x) %>% pull(comid))

# the function
f_accumulate_mean <- function(hydroid_list, varnames){

  # get data
  streamcat <- read_csv("data_clean/lsh_shasta_nhdv2_streamcat_vars.csv") %>%
    select(COMID, varnames$streamcat_source_var)

  dat_out <- map(comid_lists, ~filter(streamcat, COMID %in% .x) %>% select(-COMID))

  # iterate over
  dat_df <- map(dat_out, ~summarise(.x,
    across(
      .cols  = everything(),
      .fns   = mean,
      .names = "{col}_mean"
    )))

  return(dat_df)

}


f_accumulate_mean()
