## ----- Script Header ---------
##
## Script name:  Get Sciencebase data for Mike W's NHDplus Accmumulated Attributes
##
## Purpose of script: Automated retrieval of the ScienceBase Item "Select Attributes for NHDPlus Version 2.1 Reach Catchments and
##                      Modified Network Routed Upstream Watersheds for the Conterminous United States" by
##                      Wieczorek, Jackson and Schwarz, 2018. https://doi.org/10.5066/P9PA63SM.
## Author: Matthew J Cashman
## Email: mcashman@usgs.gov
##
## Date Created: 2019-Dec-20
##
## Notes: Beginning of workflow for SEM analysis. This script might be able to be adapted to other sciencebase items but
##        currently only works for the structure found in Mike's dataset
##
##


## ---- Script Header settings   --------
download_dir <- "/Volumes/GENOMICS/"  #Where locally you want to download the data to and create the folder structure
parent_folder <- "NHDplus_Accumulated_attributes" #Name of the parent folder to hold the data
starting_doi <- '10.5066/F7765D7V' #Only the DOI number after https://doi.org/ or doi:
folder_name_sep <- ":" #Needed to shorten child item names to only logical information after
max_folder_length <- 100 #If above separator is missing, what is the max character length to cut names to (backwards from the end of the string)

## ---- Options to Skip Files for Redownloads   --------
download_parent_files <-  TRUE
child_items_to_skip <- c("")
gchild_items_to_skip <- c("")

## ---- Load packages   --------
#install.packages("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, tidyverse, here, data.table, sbtools, httr)


## ---- Define Custom Functions   --------
`%not_in%` <- purrr::negate(`%in%`)


## ---- Query specifics for pulling Mike W's NHDplus accumulated attribuets for NHD1:100k data  --------
queryDoi <-query_sb_doi(starting_doi)
queryDoi[[1]] #Parent directory has 6 ITEMS, mostly all relating to metadata and SAS scripts used

#Create parent folder
dir.create(paste(download_dir, parent_folder, sep="/"))

start <- Sys.time()
#Creating and downloading parent items
for (i in seq(queryDoi)){
  queryDoi
  sapply(queryDoi, function(item) item$title)
  sapply(queryDoi, function(item) item$id)

  item_list_files(queryDoi[[i]])

  if(download_parent_files == TRUE){
  for (o in 1:length(item_list_files(queryDoi[[i]])$fname)){
    current_file_name <-item_list_files(queryDoi[[i]])$fname[o]
    current_file_url <-item_list_files(queryDoi[[i]])$url[o]
    current_file_size <-item_list_files(queryDoi[[i]])$size[o]

    message(paste("Downloading file", o, "of", length(item_list_files(queryDoi[[i]])$fname), ":", current_file_name))
    message(paste("File size of", current_file_size))

    download.file(current_file_url, destfile = paste(sep = "/", download_dir, parent_folder, current_file_name), method = "curl")
    }
  } #Downloading parent files

  #Finding the child items
  children <- item_list_children(queryDoi[[i]])
  child_names <- sapply(children, function(item) item$title)
  child_id <- sapply(children, function(item) item$id)

  #Create directory tree for child items
  short_child_names <- word(str_remove(child_names, " and"), start = 2L, end = 3L)
  short_child_names <- gsub(" ", "_",short_child_names)
  message("Creating Child item sub-directories:")
  message(paste("\t", short_child_names, "\n"))

  sapply(short_child_names,  function(item) dir.create(paste(sep = "/", download_dir, parent_folder, item)))

  for (p in 1:length(children)){
    current_child_name <- child_names[p]
    current_short_child_name <- short_child_names[p]

    if (current_short_child_name %not_in% child_items_to_skip){

    message(paste("Exploring child item --", current_short_child_name))

    #Finding the grandchild items
    gchildren <- item_list_children(children[[p]])
    gchild_names <- sapply(gchildren, function(item) item$title)
    gchild_id <- sapply(gchildren, function(item) item$id)

    #Create directory tree for child items
    sub_string <- paste0(".*",folder_name_sep," ")
    short_gchild_names <- gsub(".*: ","",gchild_names)
    short_gchild_names <- str_replace_all(short_gchild_names, "[^[:alnum:]]", " ")
    short_gchild_names <- gsub("  "," ", short_gchild_names)
    short_gchild_names <- gsub(" ", "_",short_gchild_names)
    if (max(nchar(short_gchild_names)) > max_folder_length){
      short_gchild_names <- str_sub(short_gchild_names, start = -max_folder_length, end = -1)
    }
    message(paste("Contains grandchild items:"))
    message(paste("\t", short_gchild_names, "\n"))
    message(paste("Creating grandchild sub-directories"))

    sapply(short_gchild_names,  function(item) dir.create(paste(sep = "/", download_dir, parent_folder, current_short_child_name, item)))

    #gchildren

    for (l in 1:length(gchildren)){
      current_gchild_name <- gchild_names[l]
      current_short_gchild_name <- short_gchild_names[l]
      if (current_short_gchild_name %not_in% gchild_items_to_skip){


      #What files in the grandchild
      item_list_files(gchild_id[l])
      gchild_file_url <- item_list_files(gchild_id[[l]])$url
      gchild_file_name <-item_list_files(gchild_id[[l]])$fname
      gchild_file_size <-item_list_files(gchild_id[[l]])$size

      #If there are files in the granchild, download
      if (ncol(item_list_files(gchild_id[l])) > 0){

        message(paste("Downloading", length(gchild_file_url), " files for:"))
        message(paste0("\t", current_short_child_name))
        message(paste0("\t \t",  current_short_gchild_name))
        message(paste0("\t \t \t",  gchild_file_name, "\n"))

        #Download all files individually
        for (k in 1:length(gchild_file_name)){
          current_file_url <-gchild_file_url[k]
        current_file_name <-gchild_file_name[k]
        current_file_size <- gchild_file_size[k]

        message(paste("Downloading file", k, "of", length(gchild_file_name), ":", current_file_name))
        message(paste("File size of", current_file_size))

        tryCatch(download.file(current_file_url, method = "curl", destfile = paste(sep = "/", download_dir, parent_folder, current_short_child_name, current_short_gchild_name, current_file_name)),
                 error = function(e) print(paste("Download of", current_file_name, 'did not work')))
        }
      }

      if (ncol(item_list_files(gchild_id[l])) == 0){

        ggchildren <- item_list_children(gchild_id[l])
        ggchild_names <- sapply(ggchildren, function(item) item$title)
        ggchild_id <- sapply(ggchildren, function(item) item$id)

        #Create directory tree for child items
        short_ggchild_names <- gsub(".*: ","",ggchild_names)
        short_ggchild_names <- str_replace_all(short_ggchild_names, "[^[:alnum:]]", " ")
        short_ggchild_names <- gsub("  "," ", short_ggchild_names)
        short_ggchild_names <- gsub(" ", "_",short_ggchild_names)

        message(paste("Contains great-grandchild items:"))
        message(paste("\t", short_ggchild_names, "\n"))
        message(paste("Creating great-grandchild sub-directories"))

        sapply(short_ggchild_names,  function(item) dir.create(paste(sep = "/", download_dir, parent_folder, current_short_child_name, current_short_gchild_name, item)))

        short_ggchild_names
        for (m in 1:length(ggchildren)){
          current_ggchild_name <- ggchild_names[m]
          current_short_ggchild_name <- short_ggchild_names[m]


          #What files in the grandchild
          item_list_files(ggchild_id[m])
          ggchild_file_url <- item_list_files(ggchild_id[[m]])$url
          ggchild_file_name <-item_list_files(ggchild_id[[m]])$fname
          ggchild_file_size <-item_list_files(ggchild_id[[m]])$size

          #If there are files in the granchild, download
         # if (ncol(item_list_files(ggchild_id[m])) > 0){

            message(paste("Downloading", length(ggchild_file_url), " files for:"))
            message(paste0("\t", current_short_child_name))
            message(paste0("\t \t",  current_short_gchild_name))
            message(paste0("\t \t \t",  current_short_ggchild_name))
            message(paste0("\t \t \t \t",  ggchild_file_name, "\n"))

            #Download all files individually
            for (n in 1:length(ggchild_file_name)){
              current_file_url <-ggchild_file_url[n]
            current_file_name <-ggchild_file_name[n]
            current_file_size <- ggchild_file_size[n]

            message(paste("Downloading file", n, "of", length(ggchild_file_name), ":", current_file_name))
            message(paste("File size of", current_file_size))

            download.file(current_file_url, method = "curl",
                          destfile = paste(sep = "/", download_dir, parent_folder, current_short_child_name, current_short_gchild_name, current_file_name))
            }
        }
      }
      } else{message(paste("Skipping over grandchild item --", current_short_gchild_name))}
    }
    } else {
      message(paste("Skipping over child item --", current_short_child_name))
    }
  }
}
end <- Sys.time()
(duration <- end-start)
message(paste("ScienceBase download complete"))
message(paste("\t DOI Item Number", starting_doi,"\n" ))
message(paste("\t Title:", sapply(queryDoi, function(item) item$title)))
message(paste("\t Download Duration:", round(duration, 2),  units(duration)))
