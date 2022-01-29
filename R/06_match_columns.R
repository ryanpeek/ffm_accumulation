library(readxl)
library(tidyverse)
library(glue)

# Read in -----------------------------------------------------------------

# this is old needs updating
nhd_dat <- read_csv("data_clean/lsh_shasta_nhdv2_streamcat_vars.csv")

# this is from google drive
xwalk <- readxl::read_xlsx("data_raw/gsheet_input_var_name_xwalk.xlsx", sheet = 1)


# Runoff Monthly ----------------------------------------------------------

# are these 1950-2000 averages we need to calculate first, then accumlate?

jan_run <- nhd_dat %>% select(contains("jan"))
# cat_wb5100_jan?
# acc_wb5100_jan?

## PPT --------------------------------------
cat_wd <- nhd_dat %>% select(starts_with("cat_wd"))
ppt <- nhd_dat %>% select(contains("ppt"))
names(ppt)

## TAV ---------------------------------------
tav <- nhd_dat %>% select(contains("tav"))
names(tav)

## RUNOFF -------------------------------------
runoff <- nhd_dat %>% select(contains("acc_run"))
names(runoff)

# 1950-2015
## need ppt/tav/run_MON_wy (12)
## need ppt/tav/run_MON_pwy (9: jan-sep?)
## need:
# ppt_ann_wy
# ppt_fall_wy
# ppt_wint_wy
# ppt_sprg_wy
# ppt_summ_wy
# ppt_ann_pwy previous water year
# ppt_wint_pwy
# ppt_sprg_pwy
# ppt_summ_pwy
# ppt_sum1
# ppt_sum2
# ppt_sum3
# ppt_sum4



# Gather vars? ------------------------------------------------------------


# need 1950-2015 for months and water year
