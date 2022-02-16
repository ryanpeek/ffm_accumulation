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
cat_data <- read_csv("data_clean/07_final_catchment_data_for_accum.csv") %>%
  # add in area_sf for later
  mutate(area_sf = NA_real_)
# length(unique(cat_data$comid)) # n=76 unique

# xwalk
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 2)
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
#map(hydroseq_ls, length) # see diff lengths

# filter: cross walk between hydroseq and comid:
# returns a list of comids for any given hydroseq
comid_ls <- map(hydroseq_ls, ~flow_trim %>%
                  filter(hydroseq %in% .x) %>% pull(comid)) %>%
  set_names(hydroseq_network$hydroseq)

# hydroseq ls of comids
map(comid_ls, length) # check lengths, 1 is a trib or spur
# n=33

# make tribs layer (singletons) (n=15)
# comid_ls_tribs <- comid_ls %>%
#   keep(., ~length(.x)==1)

rm(hydroseq_network, hydroseq_ls, catch_lsh, flow_sinks, flow)

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
    TRUE ~ accum_op))

table(xwalk$accum_op_class, useNA="ifany")

# ACCUM: Area Weighted Average ------------------------------------------

#SUM of value * area weight + value * area weight

# select all variables that need "avg of catch values"
varnames_awa <- xwalk %>%
  filter(accum_op_class=="AWA") %>%
  select(dat_output) %>%
  # rename the calculated vars to match
  mutate(dat_output = case_when(
    dat_output == "ann_min_precip_basin" ~ "cat_minp6190",
    dat_output == "ann_max_precip_basin" ~ "cat_maxp6190",
    dat_output == "pptavg_basin" ~ "cat_ppt7100_ann",
    dat_output == "et_basin" ~ "cat_et",
    dat_output == "pet_basin" ~ "cat_pet",
    dat_output == "rh_basin" ~ "cat_rh",
    dat_output == "wtdepave" ~ "cat_wtdep",
    TRUE ~ dat_output
  ))


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

# collapse as dataframe:
dat_df_awa <- bind_rows(dat_awa, .id = "hydroseq") %>%
  mutate(hydroseq=as.numeric(hydroseq)) %>%
  # fix the awa ending
  rename_with(~str_remove(., '_awa')) %>%
  # rename calculated vars back to original variables of interest
  rename(ann_min_precip_basin = cat_minp6190,
         ann_max_precip_basin = cat_maxp6190,
         pptavg_basin = cat_ppt7100_ann,
         et_basin = cat_et,
         pet_basin = cat_pet,
         rh_basin = cat_rh,
         wtdepave = cat_wtdep) %>%
  # bind back to comid
  left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
  relocate(comid, hydroseq, .before="wa_yr")

# rm temp files
rm(dat_ls_awa, varnames_awa, cat_df_awa, dat_awa)

# SAVE (as a list for now, temp)
#write_rds(dat_awa, file = "data_clean/08_accumulated_awa_metrics.rds")
#write_csv(dat_df_awa, file = "data_clean/08_accumulated_awa_metrics.csv")

# ACCUM: Max-Min-Range-sum ----------------------------------------------------

# select all variables that need "avg of catch values"
varnames_oth <- xwalk %>%
  filter(accum_op_class %in% c("AVG", "MAX","MIN","RNG","SUM")) %>%
  filter(!dat_output %in% c("comid", "comid_wy", "wa_yr")) %>%
  mutate(dat_output = case_when(
    dat_output=="t_avg_basin" ~ "cat_tav7100_ann",
    TRUE ~ dat_output
  ))

# filter to vars
cat_df_oth <- cat_data %>%
  # drop the non info vars
  select(comid:wa_yr, varnames_oth$dat_output) %>%
  # add area weights
  left_join(catch_areas_filt_no_sf, by=c("comid"="comid_catch")) %>%
  select(comid:wa_yr, comid_flowline:upper, everything()) %>%
  # fix area_sf
  select(-area_sf.x) %>%
  rename(area_sf = area_sf.y)

# filter to dataframe that is list of all comids for a given comid
dat_ls_oth <- map(comid_ls, ~filter(cat_df_oth, comid %in% .x) %>%
                    select(-c(comid, comid_wy)))

# iterate over
dat_oth <- map(dat_ls_oth, ~group_by(.x, wa_yr) %>%
                 summarise(
                   across(
                     # min cols
                     .cols  = c(cat_elev_min),
                     ~min(.x),
                     .names = "{col}_min"),
                   across(
                     # max
                     .cols = c(cat_elev_max),
                     ~max(.x),
                     .names = "{col}_max"),
                   # mean
                   across(
                     .cols = c(cat_tav7100_ann),
                     ~mean(.x, na.rm=TRUE),
                     .names = "{col}_avg"),
                   across(
                     .cols = c(area_sf),
                     ~sum(.x),
                     .names = "{col}_sum")
                 )
)

# add range after
dat_oth <- map(dat_oth, ~mutate(.x,
                                elv_rng = cat_elev_max_max - cat_elev_min_min))

#dat_oth[[30]] %>% filter(wa_yr == 1950) %>% names()

# collapse and rename to t_avg_basin
dat_df_oth <- bind_rows(dat_oth, .id = "hydroseq") %>%
  mutate(hydroseq = as.numeric(hydroseq)) %>%
  rename(t_avg_basin=cat_tav7100_ann_avg) %>%
  rename(cat_elev_min = cat_elev_min_min,
         cat_elev_max = cat_elev_max_max,
         area_sf = area_sf_sum) %>%
  # bind back to comid
  left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
  relocate(comid, hydroseq, .before="wa_yr")

rm(dat_ls_oth, varnames_oth, cat_df_oth, dat_oth)


# SAVE
#write_rds(dat_df_oth, file = "data_clean/08_accumulated_oth_metrics.rds")
#write_csv(dat_df_oth, file = "data_clean/08_accumulated_oth_metrics.csv")

# ACCUM: No Calc ----------------------------------------------------------

# need the no calc vars, subset and then join
varnames_nocalc <- xwalk %>%
  filter(accum_op_class %in% c("NONE")) %>%
  filter(!dat_output %in% c("comid", "comid_wy", "wa_yr")) %>%
  select(dat_output)

# subset data
dat_df_nocalc <- cat_data %>% select(comid, wa_yr, varnames_nocalc$dat_output) %>%
  # rename
  rename(pptavg_cat = cat_ppt7100_ann,
         t_avg_cat = cat_tav7100_ann,
         ann_min_precip = cat_minp6190,
         ann_max_precip = cat_maxp6190,
         et_cat = cat_et,
         pet_cat = cat_pet,
         rh_cat = cat_rh,
         depth_wattab = cat_wtdep) %>%
  left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
  relocate(comid, hydroseq, .before="wa_yr")

names(dat_df_nocalc)
# here cat_wtdep is actually "depth_wattab", cat_wtdep w awa is wtdepave

rm(varnames_nocalc)

# Need Eco Dominant -------------------------------------------------------

cat_df_eco <- cat_data %>%
  select(comid:wa_yr, eco3)

# filter to dataframe that is list of all comids for a given comid
dat_ls_eco <- map(comid_ls, ~filter(cat_df_eco, comid %in% .x) %>%
                    select(-c(comid, comid_wy)))

dat_eco <- map(dat_ls_eco, ~group_by(.x) %>%
                 summarise(
                   eco3_dom = names(which.max(table(eco3)))))

dat_df_eco <- bind_rows(dat_eco, .id = "hydroseq") %>%
  mutate(hydroseq=as.numeric(hydroseq)) %>%
  rename(eco3=eco3_dom) %>%
  left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
  relocate(comid, hydroseq, .before=1)

rm(dat_eco, cat_df_eco, dat_ls_eco)

# Combine All -------------------------------------------------------------

dat_final <- left_join(dat_df_awa, dat_df_oth) %>%
  left_join(dat_df_nocalc) %>%
  left_join(dat_df_eco) %>%
  select(-hydroseq) # drop

# RENAME TO MATCH -----------------------------------------

# use the cross walk to reorder and rename
# first pull the column names into a df
final_names <- dat_final %>% colnames %>% as_tibble("final_names")

# now join with the xwalk and match up or fix
xwalk_final <- left_join(final_names, xwalk, by=c("value"="dat_output")) %>%
  select(mod_input_final = value, mod_input_raw, model_input_clean, check, accum_op, accum_op_class, source_file, variable_description)

# identify the missmatches and pull out:
xwalk_final_mismatch <- xwalk_final %>% filter(is.na(model_input_clean)) %>%
  select(mod_input_final)

# now match back to the xwalk but use the model_input_clean field
xwalk_final_sel <- left_join(xwalk_final_mismatch, xwalk, by=c("mod_input_final"="model_input_clean")) %>%
  select(mod_input_final, mod_input_raw, check, accum_op, accum_op_class, source_file, variable_description)

# now merge it all together!!
xwalk_fin <- xwalk_final %>% filter(!is.na(mod_input_raw)) %>%
  bind_rows(xwalk_final_sel) %>%
  select(-model_input_clean)

# WRITE IT OUT
write_csv(xwalk_fin, file="data_clean/08_accumulated_final_xwalk.csv")

rm(xwalk_final, xwalk_final_sel, xwalk_final_mismatch)


# REORDER -----------------------------------------------------------------

input_sample <- read_csv("data_raw/sample.csv")
input_sample_names <- data.frame("inputs" = names(input_sample))

# bind w cross walk
names_df <- input_sample_names %>% left_join(., xwalk_fin, by=c("inputs"="mod_input_raw")) %>%
  filter(!is.na(mod_input_final))


# reorder to match sample
dat_final2 <- dat_final %>% select(unique(names_df$mod_input_final))

# Write Out ---------------------------------------------------------------

write_csv(dat_final2, file = "data_clean/08_accumulated_all_metrics.csv")
