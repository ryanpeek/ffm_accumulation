# Accumulation code
# R. Peek 2022

# pull each set of variables (from the 07_final_catchment_data_for_accum)
# and calculate the appropriate summary (avg, area weighted avg, min, or max) for
# each comid (and for each comid we have a lookup to identify all upstream comids)


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(sf)
library(here)
library(janitor)
library(purrr)
library(mapview)
mapviewOptions(fgb = FALSE)

# Final Data ----------------------------------------------------------------

# data by comid
cat_data <- read_csv("data_clean/07_final_catchment_data_for_accum.csv")

# xwalk
xwalk <- read_csv("data_clean/07_final_xwalk_variables.csv")

# Catchments --------------------------------------------------------------

# flowlines with VAA data
flow <- read_rds("data_clean/final_flowlines_w_full_nhd_vaa.rds") %>%
  select(id:comid, gnis_name:reachcode, ftype:fcode, fromnode:dnhydroseq, ends_with("km"), geom)

length(unique(flow$comid))

# List sink/isolated segments (N=19)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)

# make trimmed version
flow_trim <- flow %>%
  filter(comid %in% sinks)

# get catchments
catch_lsh <- read_rds("data_clean/catchments_final_lshasta.rds")

# map
# mapview(catch_lsh, col.regions="skyblue", color="dodgerblue",
#         alpha.regions=0.1, alpha=0.5,
#         layer.name="Catchments <br>trimmed to H10") +
#   mapview(flow, color="steelblue", lwd=2, layer.name="Flowlines") +
#   mapview(flow_trim, color="yellow", lwd=1, layer.name="Sinks to Drop")

# map of hydroseq ID
flow_hydroseq <- flow %>% select(comid, hydroseq, uphydroseq, dnhydroseq, areasqkm, totdasqkm, divdasqkm)

#mapview(flow_hydroseq, zcol = "hydroseq", layer.name="Hydroseq")

# Average of Catchment ----------------------------------------------------

# get hydroids
hydroseq_network <- read_csv(here("data_clean/lshasta_hydroseq_network_ds_us.csv"))

# select all variables that need "avg of catch values"
varnames_avg <- xwalk %>% filter(accum_op=="avg of catchment values") %>%
  select(dat_output) %>% distinct()

# unlist each hydroseq and make numeric
hydroseq_ls <- map(hydroseq_network$paths_lst, ~unlist(strsplit(.x, ", "))) %>%
  map(., ~as.numeric(.x))# %>%
  #set_names(., hydroseq_network$id_down_up)
map(hydroseq_ls, length)

# filter to just comids: make cross walk between hydroid and comid!!!
comid_ls <- map(hydroseq_ls, ~flow_hydroseq %>%
                     filter(hydroseq %in% .x) %>% pull(comid)) %>%
  set_names(hydroseq_network$hydroseq)

map(comid_ls, length) # check lengths, 1 is a trib or spur (so no need to avg)

# make tribs layer (singletons)
comid_ls_tribs <- comid_ls %>%
  keep(., ~length(.x)==1)

# make avg layer
comid_ls_filt <- comid_ls %>%
  keep(., ~length(.x)>1)

# this returns a comid list, but it's NOT in order of us to downstream, it's ordered numerically smallest to largest. downside to comid vs. hydroseq

# for every unique year and group of comids, we need the accumulation for that metric

# filter to vars
cat_df <- cat_data %>%
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


# Area Weighted Mean ------------------------------------------------------

# for vars that need area weighted mean, need to weight by catchment area of total drainage area

mapview(catch_lsh) + mapview(flow_hydroseq, zcol="comid")

# turn off scipen:
options(scipen = 1e6)
# check area
sf::st_area(catch_lsh[catch_lsh$FEATUREID=="3917950",]) %>% units::set_units("km^2")

# compare areas?
catch_areas <- tibble(comid=catch_lsh$FEATUREID, "area_old"=catch_lsh$AreaSqKM, "area_sf"=sf::st_area(catch_lsh) %>% units::set_units("km^2") %>% units::drop_units() %>% round(digits=3))
View(catch_areas)

# filter to areas with >.001 area
catch_areas_filt <- left_join(catch_lsh, catch_areas, by=c("FEATUREID"="comid")) %>%
  filter(area_sf> 0.001) %>%
  select(-c(SOURCEFC, AreaSqKM))

mapview(catch_areas_filt, zcol="area_sf", layer.name="sf") +
  mapview(catch_areas_filt, zcol="area_old", layer.name="Old")

lsh_area <- sum(catch_areas_filt$area_sf) # 329.915 sqkm

# add area weight:
catch_areas_filt <- catch_areas_filt %>%
  mutate(area_weight = area_sf/lsh_area)
