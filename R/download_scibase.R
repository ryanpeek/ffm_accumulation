# ScienceBase tools
# tutorial here: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-discovery/
library(sbtools)
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

# list all files
all_files <- item_list_files(id_item,recursive = TRUE)
dim(all_files) # 1412 files!
# add sizes
all_files <- all_files %>%
  mutate(size_mb = size * 1e-6, .after=size)

main_files <- item_list_files(id_item)

# get it?
(id_item_get <- item_get(id_item))
names(id_item_get)

# clean dir
fs::dir_delete("data_raw/scibase_flow/")
# create dir
fs::dir_create("data_raw/scibase_flow")

# download
map2(all_files$url[7:nrow(all_files)], all_files$fname[7:nrow(all_files)], ~download.file(.x, glue("data_raw/scibase_flow/{.y}")))


# get only xml files
xml_ls <- dir_ls("data_raw/scibase_flow/", type = "file", glob = "*.xml")

# read one in and parse?
xml1 <- as_list(xml2::read_xml(xml_ls[2]))

# make long
(xml_df <- as_tibble(xml1) %>% unnest_longer(metadata))
unique(xml_df$metadata_id)

xml_df_wide <- xml_df %>%
  filter(DATA_ID=="LP")
xml2::xml_attrs(xml1, "metadata")

# see here?
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/

# look at fields
item_get_fields(id_item, c("summary", "tags"))
