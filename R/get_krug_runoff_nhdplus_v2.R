# retrieve NHDV2 Stream Cat Data for specific COMID list

library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sf)
library(here)
library(mapview)
mapviewOptions(fgb=FALSE)

# FLOWLINES: Get Final Little Shasta FLOWLINE -------------------------------------------

flowlines <- read_rds(here("data_output/final_flowlines_w_full_nhd_vaa.rds"))

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
mapview(flowlines_trim, zcol="comid_ff", legend=FALSE)


# Get CATCHMENTS ----------------------------------------------------------

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
# load(here("data_output/06_catcharea_final_adjust.rda"))
#
# rm(df_catch_rev, df_catch_diss, df_da_final)
#
# # make comid char for plotting
# catch_final <- catch_final %>%
#   mutate(comid_c = as.factor(comid),
#          upper = if_else(is.na(comid_f), TRUE, FALSE))
#
# # save out
# write_rds(catch_final, "data_output/08_catch_final_lshasta.rds")

catch_final <- read_rds("data_output/08_catch_final_lshasta.rds")

mapview(flowlines_trim,  zcol="comid_ff", legend=FALSE) +
  mapview(catch_final, zcol="comid_c", legend=FALSE)

# n=33 unique COMIDs
(coms <- unique(flowlines_trim$comid))


# Get KRUG Runoff ------------------------------------

# see here: https://water.usgs.gov/GIS/metadata/usgswrd/XML/runoff.xml

# read in
krug <- st_read("data_output/nhdv2/krug_runoff_avg_ann_1951-1980.e00") %>%
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
  mutate(krug=c(10,5,0,0)) %>%
  filter(krug>0)

mapview(catch_split, zcol="krug") + mapview(krug_crop, zcol="INCHES")
write_rds(catch_split, file = "data_output/krug_runoff_little_shasta_sf_poly.rds")


# Now Apply to Each COMID  -----------------------------------

flowlines_trim_10 <- st_join(st_transform(flowlines_trim, 5070), catch_split) %>%
  distinct(comid, .keep_all = TRUE)
mapview(flowlines_trim_10, zcol="krug") + mapview(krug_crop)

length(unique(flowlines_trim_10$comid))

# make csv of this
krug_runoff_csv <- flowlines_trim_10 %>% st_drop_geometry() %>%
  select(COMID=comid, krug_runoff = krug) %>%
  mutate(source = "krug_runoff_avg_ann_1951-1980.e00")

write_csv(krug_runoff_csv, file = "data_output/nhdv2/KRUG_RUNOFF.csv")



