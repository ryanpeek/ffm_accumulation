# extract data from NHD and filter to comids all in same function (not looped)
# relies on f_functions

f_extract_filter_to_comids <- function(pathfile, comids, outdir, recurse=FALSE){
  print(glue("working on {pathfile}"))


# Read in -----------------------------------------------------------------

  print(glue("reading in file and filter to comids..."))

  print(glue("Reading {path_file(fullpath)}"))
  f1 <- vroom(pathfile, show_col_types = FALSE) %>% # fast readin of zips
    dplyr::rename_with(., toupper) %>%
    dplyr::filter({if("COMID" %in% names(.)) COMID else NULL} %in% comids)

  # check dimensions (one way to filter out zeros)
  print(glue("Dimensions are greater than zero: {nrow(f1)>0}"))

  # get filename
  f1 <- f1 %>% mutate(filename = fs::path_file(pathfile))

  # Write out to single csv -----------------------------------------------
  print("Writing out files")
  # use names of list to make filenames
  fs::dir_create(outdir)
  comid_writeout()

}
