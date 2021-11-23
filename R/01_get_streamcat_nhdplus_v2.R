# retrieve NHDV2 Stream Cat Data for specific COMID list

library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sf)
library(here)
library(mapview)
mapviewOptions(fgb=FALSE)


# Source Functions --------------------------------------------------------

source("R/functions/f_download_nhd_attributes_scibase.R")

# Download Data -----------------------------------------------------------

# this takes about a half hour and is ~18 GB
# f_download_nhd_attributs_scibase(download_dir = "~/Downloads/")

# FLOWLINES: Get Final Little Shasta FLOWLINE -------------------------------------------

flowlines <- read_rds(here("data_clean/final_flowlines_w_full_nhd_vaa.rds"))

# reduce fields for plotting purposes
flowlines_trim <- flowlines %>% select(id, comid, contains("seq"), hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc, geom)

# fix flowlines comid to factor in correct order
flowlines_trim <- flowlines_trim %>%
  mutate(comid_f = factor(as.character(comid),
                          levels=c(3917198, 3917200, 3917948,
                                   3917950, 3917244, 3917946)),
         comid_ff = as.factor(comid))

# drop sinks (isolated channels)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)

flowlines_trim <- flowlines_trim %>%
  filter(!comid %in% sinks)

# preview
# mapview(flowlines_trim, zcol="comid_ff", legend=FALSE)

# Get Catchments ----------------------------------------------------------

catch_final <- read_rds("data_clean/08_catchments_final_lshasta.rds")

# n=33 unique COMIDs
(coms <- unique(flowlines_trim$comid))

# Set up Directories & Files ----------------------------------------------

# main directory
dat_dir <- "~/Downloads/NHDplus_Accumulated_attributes"
#dat_dir <- "/Volumes/GENOMICS/NHDplus_Accumulated_attributes"
dir_exists(dat_dir)

# list subdir
(subdirs <- fs::dir_ls(dat_dir, type = "directory"))
(subdirs <- subdirs[!grepl("Best_Management|Chemical_Attributes|Hydrologic_Modification|Land_Cover|
                Population_Infrastructure|Water_Use", subdirs)])

# the subdir of interest
# subfold_dir <- ("Climate_Attributes") # all zips and xml
# subfold_dir <- ("Climate_Water") # all zips
# subfold_dir <- ("Geologic_Attributes") # has .txt and zip, lu_soller.zip is broken?
# subfold_dir <- ("Hydrologic_Attributes")
# subfold_dir <- ("Regional_Attributes")
# subfold_dir <- ("Soil_Attributes")
# subfold_dir <- ("Topographic_Attributes")

# Load Functions ----------------------------------

source("R/functions/f_functions.R")
source("R/functions/f_extract_filter_to_comids.R")

# RUN ---------------------------------------------------------------------

# test and run
f_extract_filter_to_comids(subdir = subdirs[1], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[2], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[4], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[5], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[6], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[7], comids = coms, outdir = "data_clean/nhd_v2")
f_extract_filter_to_comids(subdir = subdirs[8], comids = coms, outdir = "data_clean/nhd_v2")

# geology breaks bc of soller zip?
#f_extract_filter_to_comids(subdir = subdirs[3], comids = coms, outdir = "data_clean/nhd_v2")
