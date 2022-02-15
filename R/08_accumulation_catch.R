# Accumulation code
# R. Peek 2022

# pull each set of variables (from the 07_final_catchment_data_for_accum)
# and calculate the appropriate summary (avg, area weighted avg, min, or max) for
# each comid (and for each comid we have a lookup to identify all upstream comids)

# need to make sure each catchment comid is present in the dataset to calculate for the watershed...n=76 unique catchments?

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
# length(unique(cat_data$comid)) # n=76 unique

# xwalk
xwalk <- read_csv("data_clean/07_final_xwalk_variables_revised.csv")
length(unique(xwalk$dat_output)) # on duplicate, cat_wtdep, but diff calc for each

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

#mapview(catch_areas_filt, zcol="comid_flowline") +
#  mapview(flow_trim, zcol="comid")

# get total area
(lsh_area <- sum(catch_areas_filt$area_sf)) # 329.519 sqkm

# add area weight:
catch_areas_filt <- catch_areas_filt %>%
  mutate(area_weight = area_sf/lsh_area, .after="area_sf")

catch_areas_filt_no_sf <- st_drop_geometry(catch_areas_filt)

# write out and use to filter to comids of interest
#write_csv(catch_areas_filt_no_sf, file = "data_clean/08_catch_areas_filt_comid.csv")

# Mapview: Flowlines & Catchments -----------------------------------------

# map
mapview(catch_areas_filt,
        col.regions="skyblue", color="dodgerblue",
        alpha.regions=0.1, alpha=0.5,
        layer.name="Catchments <br>trimmed to H10") +
  mapview(flow_trim, zcol="hydroseq", lwd=2, layer.name="Hydroseq") +
  mapview(flow_sinks, color="gray40", lwd=1, layer.name="Sinks to Drop")


# Get Network & COMIDs --------------------------------------------------

# get hydro network for u/s routing
hydroseq_network <- read_csv(here("data_clean/lshasta_hydroseq_network_ds_us.csv"))

# unlist each hydroseq and make numeric
hydroseq_ls <- map(hydroseq_network$paths_lst, ~unlist(strsplit(.x, ", "))) %>%
  map(., ~as.numeric(.x))
map(hydroseq_ls, length) # see diff lengths

# filter: cross walk between hydroseq and comid!
comid_ls <- map(hydroseq_ls, ~flow_trim %>%
                  filter(hydroseq %in% .x) %>% pull(comid)) %>%
  set_names(hydroseq_network$hydroseq)

map(comid_ls, length) # check lengths, 1 is a trib or spur
# n=33

# make tribs layer (singletons) (n=15)
comid_ls_tribs <- comid_ls %>%
  keep(., ~length(.x)==1)

# Classify ACCUMULATION Operations ----------------------------------------

# unique calcs for accum?
table(xwalk$accum_op, useNA="ifany")

# organize a bit
xwalk <- xwalk %>%
  mutate(accum_op_class = case_when(
    grepl("area weighted avg", accum_op) ~ "AWA",
    grepl("avg of catchment", accum_op) ~ "AVG",
    grepl("sum", accum_op) ~ "SUM",
    grepl("range", accum_op) ~ "RNG",
    grepl("^min", accum_op) ~ "MIN",
    grepl("dominant", accum_op) ~ "DOM",
    grepl("max", accum_op) ~ "MAX",
    grepl("none", accum_op) ~ "NONE",
    is.na(accum_op) ~ "NONE",
    TRUE ~ accum_op))

table(xwalk$accum_op_class, useNA="ifany")

# some vars are calculated and should be separated
varnames_calc <- xwalk %>%
  filter(grepl("^calculated|caculated", check))

# ACCUM: Area Weighted Average ------------------------------------------

#SUM of value * area weight + value * area weight

# select all variables that need "avg of catch values"
varnames_awa <- xwalk %>%
  filter(accum_op_class=="AWA") %>%
  # drop any of the vars_to_calc vars above
filter(!grepl("^calculated|caculated", check)) %>%
  select(dat_output) %>% distinct()

# make layer for just non singletons?
#comid_ls_awa <- comid_ls #%>%
#  keep(., ~length(.x)>1)

# this returns a comid list, but it's NOT in order of us to downstream, it's ordered numerically smallest to largest. downside to comid vs. hydroseq

# filter to vars
cat_df_awa <- cat_data %>%
  # drop the non info vars
  select(comid:wa_yr, varnames_awa$dat_output) %>%
  # add area weights
  left_join(catch_areas_filt_no_sf, by=c("comid"="comid_catch")) %>%
  select(comid:wa_yr, comid_flowline:upper, everything())

# filter to dataframe that is list of all comids for a given comid
dat_ls_awa <- map(comid_ls, ~filter(cat_df_awa, comid %in% .x) %>%
                 select(-c(comid, comid_wy)))

# function to summarize via AWA
# test:
#dat_df_awa[[30]] %>% select(wa_yr, comid_flowline, cat_olson_fe, area_sf, area_weight, ppt_jan_wy) %>% filter(wa_yr == 1950) %>% group_by(wa_yr) %>% summarize(across(c(cat_olson_fe, ppt_jan_wy), ~sum(.x*area_weight)))

# iterate over
dat_awa <- map(dat_ls_awa, ~group_by(.x, wa_yr) %>%
                  summarise(
                    across(
                      .cols  = ppt_jan_wy:krug_runoff,
                      ~sum(.x*area_weight),
                      .names = "{col}_awa")
                    ))

#dat_awa[[30]] %>% filter(wa_yr == 1950) %>% View()
#dat_df_awa[[30]] %>% filter(wa_yr == 1950) %>% View()

# SAVE (as a list for now, temp)
write_rds(dat_awa, file = "data_clean/08_accumulated_awa_metrics.rds")

# collapse as dataframe:
dat_df_awa <- bind_rows(dat_awa, .id = "COMID")
write_csv(dat_df_awa, file = "data_clean/08_accumulated_awa_metrics.csv")

# ACCUM: MEAN -----------------------------------------------------------

# select all variables that need "avg of catch values"
varnames_avg <- xwalk %>% filter(accum_op_class=="AVG")
# just t_avg_basin: average of cat_tav7100_ann

# filter to vars
cat_df_avg <- cat_data %>%
  #mutate("t_avg_basin" = NA_real_) %>%
  # drop the non info vars
  select(comid:wa_yr, cat_tav7100_ann) %>%
  # add area weights
  left_join(catch_areas_filt_no_sf, by=c("comid"="comid_catch")) %>%
  select(comid:wa_yr, comid_flowline:upper, everything())

# filter to dataframe that is list of all comids for a given comid
dat_ls_avg <- map(comid_ls, ~filter(cat_df_avg, comid %in% .x) %>%
                 select(-c(comid, comid_wy)))

# iterate over
dat_avg <- map(dat_ls_avg, ~group_by(.x, wa_yr) %>%
                  summarise(
                    across(
                      .cols  = cat_tav7100_ann,
                      .fns   = mean, na.rm=TRUE,
                      .names = "{col}_mean"
                    )))

dat_avg[[30]] %>% filter(wa_yr == 1950) %>% View()

# collapse and rename to t_avg_basin
dat_df_avg <- bind_rows(dat_avg, .id = "COMID") %>%
  rename(t_avg_basin=cat_tav7100_ann_mean)

# SAVE
write_rds(dat_avg, file = "data_clean/08_accumulated_avg_metrics.rds")
write_csv(dat_df_avg, file = "data_clean/08_accumulated_avg_metrics.csv")



# ACCUM: Max-Min-Range-sum ----------------------------------------------------

# so for all basin based metrics, shouldn't we keep those as is (as they are likely to be the same across all COMIDs? or just average?), and for CATCHMENT based metrics, we would use area weighted avg for everything but max/min/range/sum variables?

# select all variables that need "avg of catch values"
varnames_oth <- xwalk %>%
  filter(accum_op_class %in% c("MAX","MIN","RNG","SUM","NONE")) %>%
  # drop any of the vars_to_calc vars above
  #filter(!grepl("^calculated|caculated", check)) %>%
  select(dat_output) %>% distinct()

# filter to vars
cat_df_oth <- cat_data %>%
  # drop the non info vars
  select(comid:wa_yr, varnames_awa$dat_output) %>%
  # add area weights
  left_join(catch_areas_filt_no_sf, by=c("comid"="comid_catch")) %>%
  select(comid:wa_yr, comid_flowline:upper, everything())

# filter to dataframe that is list of all comids for a given comid
dat_ls_oth <- map(comid_ls, ~filter(cat_df_oth, comid %in% .x) %>%
                    select(-c(comid, comid_wy)))

# function to summarize via AWA
# test:
#dat_df_awa[[30]] %>% select(wa_yr, comid_flowline, cat_olson_fe, area_sf, area_weight, ppt_jan_wy) %>% filter(wa_yr == 1950) %>% group_by(wa_yr) %>% summarize(across(c(cat_olson_fe, ppt_jan_wy), ~sum(.x*area_weight)))

# iterate over
dat_awa <- map(dat_ls_awa, ~group_by(.x, wa_yr) %>%
                 summarise(
                   across(
                     .cols  = ppt_jan_wy:krug_runoff,
                     ~sum(.x*area_weight),
                     .names = "{col}_awa")
                 ))

#dat_awa[[30]] %>% filter(wa_yr == 1950) %>% View()
#dat_df_awa[[30]] %>% filter(wa_yr == 1950) %>% View()

# SAVE (as a list for now, temp)
write_rds(dat_awa, file = "data_clean/08_accumulated_awa_metrics.rds")

# collapse as dataframe:
dat_df_awa <- bind_rows(dat_awa, .id = "COMID")
write_csv(dat_df_awa, file = "data_clean/08_accumulated_awa_metrics.csv")
