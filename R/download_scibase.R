# ScienceBase tools
# tutorial here: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-discovery/
library(sbtools)


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
main_files <- item_list_files(id_item)
main_files

# get it?
(id_item_get <- item_get(id_item))
names(id_item_get)

# test download
fs::dir_create("data_raw/scibase_flow")

library(purrr)
library(glue)
map2(all_files$url[6:10], all_files$fname[6:10], ~download.file(.x, glue("data_raw/scibase_flow/{.y}")))
