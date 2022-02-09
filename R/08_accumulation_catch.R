# Accumulation code
# R. Peek 2022

# pull each set of variables (from the 07_final_catchment_data_for_accum)
# and calculate the appropriate summary (avg, area weighted avg, min, or max) for
# each comid (and for each comid we have a lookup to identify all upstream comids)

# need to make sure each catchment comid is present in the dataset to calculate for the watershed...

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(sf)
library(here)
library(janitor)
library(purrr)
library(mapview)
mapviewOptions(fgb = FALSE)
# turn off scipen:
options(scipen = 1e6)

# Final Data ----------------------------------------------------------------

# data by comid
cat_data <- read_csv("data_clean/07_final_catchment_data_for_accum.csv")

# xwalk
xwalk <- read_csv("data_clean/07_final_xwalk_variables_revised.csv")

# Flowlines --------------------------------------------------------------

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


# Catchments --------------------------------------------------------------

# get catchments
catch_lsh <- read_rds("data_clean/catchments_final_lshasta.rds")
# FEATUREID here is original COMID from catchment
# comid and comid_f are tied to the flowlines

# compare areas and get updated areas
catch_areas <- tibble(comid_catch=catch_lsh$FEATUREID,
                      comid_flowline=catch_lsh$comid,
                      "area_old"=catch_lsh$AreaSqKM,
                      "area_sf"=sf::st_area(catch_lsh) %>%
                        units::set_units("km^2") %>%
                        units::drop_units() %>% round(digits=3))

# bind back to sf
catch_areas <- left_join(catch_areas, catch_lsh,
                         by=c("comid_catch"="FEATUREID")) %>%
  select(-c(SOURCEFC:AreaSqKM, comid:comid_f)) %>%
  st_as_sf()

# quick preview
# catch_areas %>% mutate(comid_flowline=as.factor(comid_flowline)) %>% mapview(zcol="comid_flowline") + mapview(flow_trim, color="steelblue")

# filter out small slivers and get just comids of interest
catch_areas_filt <- catch_areas %>%
  filter(comid_flowline %in% flow_trim$comid) # n=76 comids

mapview(catch_areas_filt, zcol="comid_flowline") +
  mapview(flow_trim, zcol="comid")

# get total area
(lsh_area <- sum(catch_areas_filt$area_sf)) # 329.519 sqkm

# add area weight:
catch_areas_filt <- catch_areas_filt %>%
  mutate(area_weight = area_sf/lsh_area, .after="area_sf")

catch_areas_filt_no_sf <- st_drop_geometry(catch_areas_filt)

# write out and use to filter to comids of interest
write_csv(catch_areas_filt_no_sf, file = "data_clean/08_catch_areas_filt_comid.csv")

# Mapview: Flowlines & Catchments -----------------------------------------

# map
mapview(catch_areas_filt,
        col.regions="skyblue", color="dodgerblue",
        alpha.regions=0.1, alpha=0.5,
        layer.name="Catchments <br>trimmed to H10") +
  mapview(flow_trim, zcol="hydroseq", lwd=2, layer.name="Hydroseq") +
  mapview(flow_sinks, color="gray40", lwd=1, layer.name="Sinks to Drop")


# Get Network -------------------------------------------------------------

# get hydro network for u/s routing
hydroseq_network <- read_csv(here("data_clean/lshasta_hydroseq_network_ds_us.csv"))

# select all variables that need "avg of catch values"
varnames_avg <- xwalk %>% filter(accum_op=="avg of catchment values") %>%
  select(dat_output) %>% distinct()

# unlist each hydroseq and make numeric
hydroseq_ls <- map(hydroseq_network$paths_lst, ~unlist(strsplit(.x, ", "))) %>%
  map(., ~as.numeric(.x))# %>%
  #set_names(., hydroseq_network$id_down_up)
map(hydroseq_ls, length)

# filter: cross walk between hydroseq and comid!
comid_ls <- map(hydroseq_ls, ~flow_trim %>%
                     filter(hydroseq %in% .x) %>% pull(comid)) %>%
  set_names(hydroseq_network$hydroseq)

map(comid_ls, length) # check lengths, 1 is a trib or spur
# n=33

# make tribs layer (singletons)
comid_ls_tribs <- comid_ls %>%
  keep(., ~length(.x)==1)

# make layer for sums using area weighted avg
comid_ls_awa <- comid_ls %>%
  keep(., ~length(.x)>1)

# this returns a comid list, but it's NOT in order of us to downstream, it's ordered numerically smallest to largest. downside to comid vs. hydroseq

# for every unique year and group of comids, we need the accumulation for that metric

# filter to vars
cat_df <- cat_data %>%
  # drop the non info vars
  select(comid:wa_yr, varnames_avg$dat_output)

# filter to dataframe that is list of all comids for a given comid
dat_out <- map(comid_ls, ~filter(cat_df, comid %in% .x) %>%
                 select(-c(comid, comid_wy)))

# drop zero value items
#purrr::keep(., ~dim(.x)[1]>0)

# iterate over
dat_mean <- map(dat_out, ~group_by(.x, wa_yr) %>%
                summarise(
                          across(
                            .cols  = everything(),
                            .fns   = mean, na.rm=TRUE,
                            .names = "{col}_mean"
                          )))


# SAVE
write_rds(dat_mean, file = "data_clean/08_accumulated_mean_metrics.rds")



# Area Weighted Average ---------------------------------------------------

#SUM of value * area weight + value * area weight

