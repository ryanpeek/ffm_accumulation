# floodplains


# Libraries ---------------------------------------------------------------

library(here)
library(glue)
library(sf)
library(tidyverse)
library(fs)
library(mapview)
mapviewOptions(fgb = FALSE)


# Get Data ----------------------------------------------------------------

db <- "~/Downloads/floodmaps/"
fs::dir_ls(db, glob="*.shp")
levee_areas <- st_read(here(db, "Levee Areas.shp"))


# Viz ---------------------------------------------------------------------

mapview(levee_areas, zcol="StudyArea", legend=FALSE)
