# Clean NHD attributes

# assumes data has been downloaded (via 01_download_NHD_attributes_mjc.R)
# functions from f_Cleanup_xml-1.r must be loaded


# Load Library & Functions -----------------------------------------------

library(tidyverse)
library(here)
library(fs)
library(haven) # to read SAS files
source("R/get_nhd_scibase/f_Cleanup_xml-1.r")


# Set Data Path -----------------------------------------------------------

nhd_data <- "~/Downloads/NHDplus_Accumulated_attributes"

# get list of files in folder
all_files <- fs::dir_ls(nhd_data, recurse = TRUE)
data_files <- all_files[!grepl(".xml", all_files)]

# Build Metadata ----------------------------------------------------------

build_metadata(data_files = data_files, data_folder = nhd_data)


# Read sas7bdat Files -----------------------------------------------------


index_nhd <- read_sas(here(nhd_data,"index_master","index_master.sas7bdat"))
nhd_elevvars <- read_sas(here(nhd_data, "index_master", "nhdplus_elevvars.sas7bdat"))
nhd_sinkreachcode <- read_sas(here(nhd_data, "index_master", "nhdplus_sinkreachcode.sas7bdat"))
nhd_v2_us <- read_sas(here(nhd_data, "nhdplusv2_us.sas7bdat")) ## BIG FILE ~2.5 GB

# save out as RData files for easier work:
save(index_nhd, file = "data_output/nhdplus/index_master.rda")
save(nhd_elevvars, file = "data_output/nhdplus/nhd_elevvars.rda")
save(nhd_sinkreachcode, file = "data_output/nhdplus/nhd_sinkreachcode.rda")
save(nhd_v2_us, file = "data_output/nhdplus/nhd_v2_us.rda", compress = "xz")
