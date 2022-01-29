# ScienceBase tools
# tutorial here: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-discovery/

library(sbtools)
library(readr)
library(purrr)
library(glue)
library(fs)
library(xml2)
library(tidyr)
library(tibble)
library(dplyr, warn.conflicts = FALSE)
options(scipen = 100)

# Query ScienceBase based on catchment keyword
#query_sb_text('Catchment Values', limit=1)

# https://www.sciencebase.gov/catalog/items?parentId=566f6c76e4b09cfe53ca77fe


# Query Use the DOI -------------------------------------------------------

# query using DOI
(doi_info <- query_sb_doi('10.5066/F7765D7V', limit = 5000))

id_item <- doi_info[[1]]$id

# check it's legit
identifier_exists(id_item)

# get parent item
(id_parent <- item_get_parent(id_item))
id_parent$title

# Inspect all the pieces ("children")
children <- item_list_children("566f6c76e4b09cfe53ca77fe", limit = 1000)
sapply(children, function(child) child$title)
length(children)


# List Files --------------------------------------------------------------

# list all files
all_files <- item_list_files(id_item,recursive = TRUE)
dim(all_files) # 1412 files!

# add sizes
all_files <- all_files %>%
  mutate(size_mb = size * 1e-6, .after=size)

# get list of main files
main_files <- item_list_files(id_item)

# get it?
(id_item_get <- item_get(id_item))
names(id_item_get)


# Prep Directories --------------------------------------------------------

# clean dir
fs::dir_delete("data_raw/scibase_flow/")
# create dir
fs::dir_create("data_raw/scibase_flow")

# Download Files ----------------------------------------------------------

# download: this takes a while
map2(all_files$url[7:nrow(all_files)], all_files$fname[7:nrow(all_files)], ~download.file(.x, glue("data_raw/scibase_flow/{.y}")))


# Look at XML Files -------------------------------------------------------

# get only xml NHDV2 files
xml_ls <- dir_ls("data_raw/scibase_flow/", type = "file", regexp = "(.*)NHDV2(.*).xml")
xml_ls

# read one in and parse?
(xml1 <- as_list(xml2::read_xml(xml_ls[1])))

# make long dataframe of data
(xml_df <- as_tibble(xml1) %>% unnest_longer(metadata))

# look at all column names
unique(xml_df$metadata_id)

# see here?
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/



# Now Aggegrate -----------------------------------------------------------


source("R/functions/f_functions.R")
source("R/functions/f_extract_filter_to_comids_single.R")

# get coms
comids <- read_rds("data_clean/comid_list.rds")
subdir <- "data_raw/scibase_flow/"
outdir <- "data_clean/scibase_flow"

# tst
file_list <- get_zip_list(glue("{subdir}"), "*zip", recurse = FALSE)

# extract
#f_extract_filter_to_comids(subdir = data_dir, comids = coms, outdir = "data_clean/flow_nhd_v2", recurse = FALSE)

alldat <- map(file_list$path,
    ~f_extract_filter_to_comids(.x, comids = comids,
                                outdir = outdir))

# check dimensions (one way to filter out zeros)
map(alldat, ~nrow(.x)>0) # all items have >0 rows if TRUE

# Drop Zero Data --------------------------------------------------------------

# drop data with zero rows
alldat <- discard(alldat, ~nrow(.x)==0)

# Save into one file ------------------------------------------------------

alldat_combine <- alldat %>%
  reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source")))

# write out
write_csv(alldat_combine, file = glue("{outdir}/scibase_data_merged_by_comids.csv"))


