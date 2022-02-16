# retrieve NHDV2 Stream Cat Data for specific COMID list

library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sf)
library(here)
library(mapview)
mapviewOptions(fgb=FALSE)


# Get COMIDs --------------------------------------------------------------

# get comids (comid_catch)
comids <- read_csv("data_clean/08_catch_areas_filt_comid.csv") %>%
  pull(comid_catch)

# # FLOWLINES: Get Final Little Shasta FLOWLINE -------------------------------------------

# flowlines with VAA data
flow <- read_rds("data_clean/final_flowlines_w_full_nhd_vaa.rds") %>%
  select(id:comid, gnis_name:reachcode, ftype:fcode, fromnode:dnhydroseq, ends_with("km"), geom)

length(unique(flow$comid))

# List sink/isolated segments (N=19)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)

# make just sinks vs
flow_sinks <- flow %>%
  filter(comid %in% sinks) %>%
  select(comid, hydroseq, uphydroseq, dnhydroseq, areasqkm, totdasqkm, divdasqkm)

# make trimmed version
flow_trim <- flow %>%
  filter(!comid %in% sinks) %>%
  select(comid, hydroseq, uphydroseq, dnhydroseq, areasqkm, totdasqkm, divdasqkm)

# preview
mapview(flow_trim, zcol="comid", legend=FALSE)


# Get CATCHMENTS ----------------------------------------------------------

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)

catch_final <- read_rds("data_clean/catchments_final_lshasta.rds")

# mapview(flowlines_trim,  zcol="comid_ff", legend=FALSE) +
#   mapview(catch_final, zcol="comid_f", legend=FALSE)
#
# # n=33 unique COMIDs
#(coms <- unique(flowlines_trim$comid))

coms <- catch_final %>% filter(FEATUREID %in% comids)

length(unique(coms$FEATUREID))
length(unique(coms$comid))

# Get KRUG Runoff ------------------------------------

# see here: https://water.usgs.gov/GIS/metadata/usgswrd/XML/runoff.xml

# https://water.usgs.gov/GIS/dsdl/runoff.e00.zip
# get the file:
download.file("https://water.usgs.gov/GIS/dsdl/runoff.e00.zip", destfile = "data_raw/krug_runoff_avg_ann_1951-1980.e00.zip")
# unzip
unzip("data_raw/krug_runoff_avg_ann_1951-1980.e00.zip", exdir = "data_clean/") # makes "runoff.e00"
# rename
fs::file_move("data_clean/runoff.e00", new_path = "data_clean/krug_runoff_avg_ann_1951-1980.e00")


# read in
krug <- st_read("data_clean/krug_runoff_avg_ann_1951-1980.e00") %>%
  st_transform(5070)

st_crs(krug)$epsg

catch_final_aea <- st_transform(catch_final, 5070)

# get ca boundary
ca <- urbnmapr::get_urbn_map("states", sf=TRUE) %>%
  filter(state_abbv =="CA") %>%
  st_transform(5070)

# crop by State
krug_crop <- st_intersection(krug, ca)

# dissolve into polygons? this is hard: https://chitchatr.wordpress.com/2014/03/15/creating-dems-from-contour-lines-using-r/

# making too complicated...merge with watershed boundary and then just split boundary by line, assign value and done.

library(smoothr)
library(rmapshaper)
catch_diss <- ms_dissolve(catch_final_aea)
mapview(catch_diss) + mapview(krug_crop)

# split
tst <- lwgeom::st_split(catch_diss, krug_crop)
# now make into polygons
catch_split <- tst %>%
  st_collection_extract(c("POLYGON")) %>%
  dplyr::mutate(krug=c(10,5)) %>%
  dplyr::filter(krug>0)

mapview(catch_split, zcol="krug") + mapview(krug_crop, zcol="INCHES")

#write_rds(catch_split, file = "data_clean/krug_runoff_little_shasta_sf_poly.rds")

#catch_split <- read_rds("data_clean/krug_runoff_little_shasta_sf_poly.rds")

# Now Apply to Each COMID  -----------------------------------



krug_w_coms <- st_join(st_transform(coms, 5070), catch_split) %>%
  distinct(FEATUREID, .keep_all = TRUE)
  #distinct(comid, .keep_all = TRUE)
mapview(krug_w_coms, zcol="krug") + mapview(krug_crop) +
  mapview(flow_trim, zcol="comid", legend=FALSE)

length(unique(krug_w_coms$comid))

# make csv of this
krug_runoff_csv <- krug_w_coms %>% st_drop_geometry() %>%
  select(COMID=FEATUREID, flowCOMID=comid, krug_runoff = krug) %>%
  mutate(source = "krug_runoff_avg_ann_1951-1980.e00")

write_csv(krug_runoff_csv, file = "data_clean/scibase_flow/KRUG_RUNOFF.csv")



