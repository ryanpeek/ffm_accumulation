# extract data from NHD and filter to comids
# relies on f_functions

f_extract_filter_to_comids <- function(subdir, comids, outdir, recurse=FALSE){
  print(glue("working on {subdir}"))
  # look for zips here
  ziplist <- get_zip_list(glue("{subdir}"), "*zip", recurse)

  # Filter to Comids --------------------------------------------------------

  print(glue("reading in zips and filtering to comids of interest..."))
  # filter to comids
  alldat <- map(ziplist$path, ~comid_filter(comids, .x))

  # check dimensions (one way to filter out zeros)
  map(alldat, ~nrow(.x)>0)

  # set names using ziplist
  alldat_src <- alldat %>%
    # sets list names to filename
    set_names(fs::path_ext_remove(ziplist$filename)) %>%
    # adds a "source" column with filename
    imap(., ~ add_column(.x, source = .y))

  names(alldat_src) # check

  # check source in everything?
  map_depth(alldat_src, .depth = 1, ~head(.x))

  # Drop Zero Data --------------------------------------------------------------

  # drop data with zero rows
  alldat_filt <- discard( alldat_src, ~nrow(.x)==0)

  # Write each file to single csv -----------------------------------------------
  print("Writing out files")
  # use names of list to make filenames
  fs::dir_create(outdir)
  pmap(list(alldat_filt, outdir, names(alldat_filt)),
       ~comid_writeout(..1, ..2, ..3))

  # Save into one file ------------------------------------------------------

  alldat_combine <- alldat_filt %>%
    reduce(left_join, by = c("COMID")) %>% # join by COMID
    # drop dup columns
    select(-c(ends_with(".x"), contains("NODATA"), starts_with("source")))

  # write out
  write_csv(alldat_combine, file = glue("{outdir}/{janitor::make_clean_names(fs::path_file(subdir))}_data_merged_by_comids.csv"))

}
