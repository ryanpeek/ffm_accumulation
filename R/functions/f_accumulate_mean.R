# function to run accumulations

# need a few things: list of comids and directionality
# the thing to accumulate over
library(readr)
library(glue)
library(dplyr)
library(here)
library(sf)

f_accumulate_mean <- function(hydroid_list, varname){

  # get data
  streamcat <- read_csv("data_clean/lsh_shasta_nhdv2_streamcat_vars.csv")
  # flowlines
  hydroids <- read_rds(here("data_clean/final_flowlines_w_full_nhd_vaa.rds")) %>%
    select(id, comid, hydroseq, gnis_name, areasqkm, geom) %>% st_drop_geometry()
  # get hydroids
  networkids <- read_csv(here("data_clean/lshasta_hydroid_network.csv")) %>%
    mutate(hydroid_csv = c(gsub(pattern = " ",
                                replacement = ", ",
                                x = networkids$paths_chr)))

  # need to make cross walk between hydroid and comid!!! yarg
  comid_hydroid <- hydroids %>% filter(hydroseq %in% hydroid_list) %>% pull(comid)

  # filter to ids of interest:
  streamcat_filt <- filter(streamcat, COMID %in% comid_hydroid)

  # now calc
  (dat_out <- mean(streamcat_filt[[varname]], na.rm=TRUE))

}



# test
varname <- "cat_bfi"
# get hydroids
networkids <- read_csv(here("data_clean/lshasta_hydroid_network.csv")) %>%
  mutate(hydroid_csv = c(gsub(pattern = " ",
                              replacement = ", ",
                              x = networkids$paths_chr)))

# need to make cross walk between hydroid and comid!!! yarg
hydroid_list <- unlist(strsplit(networkids$hydroid_csv[5], ", "))
f_accumulate_mean()
