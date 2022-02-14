# ScienceBase tools
# tutorial here: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-discovery/

library(tidyverse)
library(purrr)
library(glue)
library(fs)
options(scipen = 100)

# Load Functions ----------------------------------------------------------

source("R/functions/f_functions.R")
source("R/functions/f_extract_filter_to_comids_single.R")

# Now Aggegrate -----------------------------------------------------------

# get comids (comid_catch)
comids <- read_csv("data_clean/08_catch_areas_filt_comid.csv") %>%
  pull(comid_catch)

# set up data
subdir <- "data_raw/scibase_flow/"
outdir <- "data_clean/scibase_flow"

# get file list
file_list <- get_zip_list(glue("{subdir}"), "*zip", recurse = FALSE)
head(file_list)

# extract single (for testing)
#f_extract_filter_to_comids(subdir = data_dir, comids = coms, outdir = "data_clean/flow_nhd_v2", recurse = FALSE)

alldat <- map(file_list$path,
    ~f_extract_filter_to_comids(.x, comids = comids,
                                outdir = outdir))

# check dimensions (one way to filter out zeros)
sum(unlist(map(alldat, ~nrow(.x)>0))) # all items have >0 rows if TRUE

# Drop Zero Data --------------------------------------------------------------

# drop data with zero rows
alldats <- compact(alldat, ~nrow(.x)==0)



