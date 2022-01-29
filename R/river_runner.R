# river runner:

# https://ksonda.github.io/global-river-runner/

library(dplyr)
library(glue)
library(here)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)


# pt to use as start
lonlat <- c(-122.2385, 41.8014)
lonlat <- c(-120.27390, 39.21696)

# url to get file
baseurl <- "https://merit.internetofwater.app/processes/river-runner/execution?"

river_run <- glue("{baseurl}lat={lonlat[2]}&lng={lonlat[1]}")
#river_run

# download
download.file(river_run, "data_raw/riverrun_lsh.geojson")

# read
river_file <- st_read("data_raw/riverrun_lsh.geojson")

# map
mapview(river_file)
