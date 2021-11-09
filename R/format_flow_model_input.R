# Stitch the NHDv2 StreamCat data together to match Flow Model Input Sample

library(purrr)
library(dplyr)
library(readr)
library(vroom)
library(janitor)
library(glue)
library(here)
library(fs)

# Read In Files -----------------------------------------------------------

dat_dir <- here("data_output/")
dir_exists(dat_dir)

files_to_get <- list.files(path = dat_dir, pattern = "^little_shasta_.*\\.csv$")

tst <- map(files_to_get, ~read_csv(glue("{dat_dir}/{.x}")))

# bind together
alldat <- tst %>% reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source"))) %>%
  clean_names() %>%
  rename(sid=id_y) %>%
  # drop dup cols:
  select(-c(starts_with("x"), starts_with("id")))

dall <- names(alldat) %>% as_tibble() %>% rename("all"=value)

View(dall)

write_csv(dall, file="data_output/lsh_nhdv2_streamcat_var_names.csv")

# Read in Example Input for Models ----------------------------------------

input_sample <- read_csv("code/flow_calcs/sample.csv") %>%
  clean_names()

dsel <- names(input_sample) %>% as_tibble() %>% rename("input"=value)
View(dsel)

write_csv(dsel, file="data_output/lsh_nhdv2_sample_input_var_names.csv")

# Compare column names ----------------------------------------------------

filter(dsel, input %in% dall$all) # only two??
# all names are slightly_diff...of course

# Write it Out ------------------------------------------------------------

write_csv(alldat, file="data_output/lsh_nhdv2_streamcat_vars.csv")
write_rds(alldat, file="data_output/lsh_nhdv2_streamcat_vars.rds")



