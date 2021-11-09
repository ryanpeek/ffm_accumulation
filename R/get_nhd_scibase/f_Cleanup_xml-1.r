library(fs)
library(XML)

build_metadata <- function(data_files, data_folder) {
  # list all xmls
  #xml_files <- fs::dir_ls(data_folder, recurse=TRUE, glob = "*.xml")
  xml_files <- list.files(data_folder, pattern = '.xml', full.names = TRUE, recursive = TRUE)

  # drop the general landing page and pop serve
  xml_files <- xml_files[!(grepl("NHDPlusV2_us_Attributes_LandingPage.xml", xml_files) |
                             grepl("pop_serve_with_1.xml", xml_files))]

  output <- NULL

  for (i in xml_files){
    dat <- xml(i) # this is a function? does not exist
    message(i)

    xml_list <- xmlToList(dat)

    for (k in seq (1, length(xml_list$eainfo))){

      message(paste("file:", k))

      generalInfo <- xml_list$eainfo[[k]][[1]]

      filename <- generalInfo$enttypl
      description <- generalInfo$enttypd
      def <- generalInfo$enttypds

      myval <- NULL
      for (j in seq (2, length(xml_list$eainfo[[k]]))){

        message(paste("var:", j))

        column_name <- xml_list$eainfo[[k]][[j]]$attrlabl
        column_def <- xml_list$eainfo[[k]][[j]]$attrdef
        attr_units <- unlist(xml_list$eainfo[[k]][[j]]$attrdomv)

        if (length(attr_units) > 0){
          column_units <- attr_units[3]
        } else {
          column_units <- attr_units
        }

        myval <- rbind (myval, c(column_name,column_def, column_units, filename, description, def))
      }

      myval <- as.data.frame (myval)

      names (myval) <- paste0('col', seq (1, ncol(myval)))

      output <- rbind (output, myval)

      names (output) <- paste0('col', seq (1, ncol(output)))
    }
  }

  names(output) <- c('Col_name', 'Description', 'Units', 'Filename', 'GenInfo','Originator')

  data_files_df <- data.frame(sb_id = sapply(data_files, names))
  data_files_df$filename <- lapply(data_files, function(x) x[[1]])

  data_files_df <- tidyr::unnest(data_files_df, cols = filename)

  data_files_df$filename <- remove_endings(data_files_df$filename)

  output$Filename <- remove_endings(output$Filename)

  output <- dplyr::left_join(output, data_files_df, by = c("Filename" = "filename"))

  return(output)
}

remove_endings <- function(x) {
  gsub(".txt", "",
       gsub(".zip", "",
            gsub(".dbf", "",
                 gsub(".csv", "",
                      x))))
}

reconcile_metadata <- function(hand_md, auto_md, sb_data) {

  hand_md <- get_metadata(hand_md)
  sb_data <- get_data_files_df(sb_data)

  # filter automd to only include cat, tot, and acc cols
  auto_md2 <- filter(auto_md, (grepl("ACC_", Col_name) |
                                 grepl("CAT_", Col_name) |
                                 grepl("TOT_", Col_name)) & !grepl("NODATA", Col_name)) %>%
    mutate(base_name = substring(Col_name, 5))

  auto_md3 <- data.frame(base_name = unique(auto_md2$base_name)) %>%
    left_join(select(filter(auto_md2, grepl("CAT_", Col_name)), Catchment.Item.Name = Col_name, base_name,
                     UNITS = Units, Science.Base.Link = sb_id, DESCRIPTION = Description), by = "base_name") %>%
    left_join(select(filter(auto_md2, grepl("ACC_", Col_name)), Divergence.Item.Name = Col_name, base_name), by = "base_name") %>%
    left_join(select(filter(auto_md2, grepl("TOT_", Col_name)), Total.Upstream.Item.Name = Col_name, base_name), by = "base_name") %>%
    select(DESCRIPTION, Catchment.Item.Name,  Divergence.Item.Name, Total.Upstream.Item.Name, UNITS, Science.Base.Link) %>%
    mutate(Science.Base.Link = ifelse(!is.na(Science.Base.Link),
                                      paste0("https://www.sciencebase.gov/catalog/item/", Science.Base.Link),
                                      Science.Base.Link),
           Auto.Added = "TRUE")

  left_join(hand_md, select(auto_md3, Catchment.Item.Name, new_sb = Science.Base.Link), by = "Catchment.Item.Name") %>%
    mutate(Science.Base.Link = ifelse(!is.na(new_sb), new_sb, Science.Base.Link)) %>% # fixes some broken sciencebase URLs
    select(-new_sb) %>%
    mutate(Auto.Added = "FALSE") %>%
    bind_rows(filter(auto_md3, !Catchment.Item.Name %in% hand_md))

}

get_data_files_df <- function(data_files) {
  data_files_df <- data.frame(sb_id = sapply(data_files, names))
  data_files_df$filename <- lapply(data_files, function(x) x[[1]])

  data_files_df <- tidyr::unnest(data_files_df, cols = filename)
}

get_metadata <- function(in_file) {
  f <- file(in_file)
  inputText <- readLines(f)
  close(f)

  inputText <- cleanText(inputText)

  read.delim(text = inputText, sep = "\t", stringsAsFactors = FALSE)
}

# Reads in metadata table from Mike W.
# Cleans it up and writes it into a structured format.
# Creates dataList.json for later.

make_datalist <- function(in_file, metadata_table, sb_data, out) {
  if(file.exists(out)) return(out)

  f <- file(in_file)
  inputText <- readLines(f)
  close(f)

  inputText <- cleanText(inputText)

  meta <- read.delim(text = inputText, sep = "\t", stringsAsFactors = FALSE)

  sb_data <- setNames(lapply(sb_data, function(x) x[[1]]), sapply(sb_data, names))

  dataList<-list()

  for(i in 1:nrow(meta)) {

    item<-meta[i,]

    if(nchar(item$Theme)>0) {theme<-item$Theme}
    if(nchar(item$DESCRIPTION)>0) {title<-item$DESCRIPTION}
    if(grepl("http", item$`Science.Base.Link`)) {

      if(grepl("TOTalog", item$`Science.Base.Link`)) {
        item$`Science.Base.Link` <- stringr::str_replace(item$`Science.Base.Link`, "TOTalog", "catalog")
      }
      if(grepl("ACCalog", item$`Science.Base.Link`)) {
        item$`Science.Base.Link` <- stringr::str_replace(item$`Science.Base.Link`, "ACCalog", "catalog")
      }

      sciBu<-item$`Science.Base.Link`
      sb_id<-str_split(sciBu,"/")[[1]][6]
    }

    if(sb_id %in% names(sb_data)) {
      if(!sciBu %in% names(dataList)){

        item_files <- NA
        try(item_files <- sb_data[[sb_id]])

        if(!all(is.na(item_files))){
          dataList[[sciBu]] <- list()
          dataList[[sciBu]][["files"]]<-item_files
          if(!length(item_files)>0) {
            children<-item_list_children(sb_id)
            dataList[[sciBu]][["files"]]<-c()
            for(child in children) {
              item_files<-item_list_files(child$id)$fname
              dataList[[sciBu]][["files"]]<-c(dataList[[sciBu]][["files"]], item_files)
            }
          }
        }

        dataList[[sciBu]][["vars"]]<-list(description=c(),
                                          localCatch_name=c(),
                                          divRoute_name=c(),
                                          totRoute_name=c(),
                                          units=c())
      }

      dataList[[sciBu]][["theme"]]<-theme
      dataList[[sciBu]][["themeURL"]]<-themeURLs[[theme]]
      dataList[[sciBu]][["title"]]<-title
      dataList[[sciBu]][["vars"]][["description"]]<-c(dataList[[sciBu]][["vars"]][["description"]],
                                                      item$DESCRIPTION)
      dataList[[sciBu]][["vars"]][["localCatch_name"]]<-c(dataList[[sciBu]][["vars"]][["localCatch_name"]],
                                                          item$`Catchment.Item.Name`)
      dataList[[sciBu]][["vars"]][["divRoute_name"]]<-c(dataList[[sciBu]][["vars"]][["divRoute_name"]],
                                                        item$`Divergence.Item.Name`)
      dataList[[sciBu]][["vars"]][["totRoute_name"]]<-c(dataList[[sciBu]][["vars"]][["totRoute_name"]],
                                                        item$`Total.Upstream.Item Name`)
      dataList[[sciBu]][["vars"]][["units"]]<-c(dataList[[sciBu]][["vars"]][["units"]],
                                                item$UNITS)
    }
  }

  sink(out)
  cat(toJSON(dataList))
  sink()

  return(out)

  # sink('datatext.txt')
  # str(dataList)
  # sink()

}

cleanText <- function(x) {
  x <- gsub('\x89|\xd5', "'", x, perl = TRUE)
  x <- gsub('\xd0', "-", x, perl = TRUE)
  x <- gsub('\xd2|\xd3', '"', x, perl = TRUE)
  x <- gsub('\xca', ' ', x, perl = TRUE)
  x <- iconv(x, "UTF-8", "ASCII", sub="")
}

themeURLs<-list(
  "Chemical"="https://www.sciencebase.gov/catalog/item/56fd600fe4b022712b81bf9a",
  "Climate"="https://www.sciencebase.gov/catalog/item/566ef828e4b09cfe53ca76f7",
  "Climate and Water Balance Model" = "https://www.sciencebase.gov/catalog/item/566f6c76e4b09cfe53ca77fe",
  "Geology" = "https://www.sciencebase.gov/catalog/item/5703c816e4b0328dcb82295a",
  "Hydro Mod" = "https://www.sciencebase.gov/catalog/item/570670f2e4b03f95a075aacd",
  "Hydro_Mod" = "https://www.sciencebase.gov/catalog/item/570670f2e4b03f95a075aacd",
  "Hydrologic" = "https://www.sciencebase.gov/catalog/item/5669a8a4e4b08895842a1d4c",
  "Landscape" = "https://www.sciencebase.gov/catalog/item/5669a834e4b08895842a1d49",
  "Population Infrastructure" = "https://www.sciencebase.gov/catalog/item/57067106e4b03f95a075aacf",
  "Regions" = "https://www.sciencebase.gov/catalog/item/5785585ee4b0e02680bf2fd6",
  "Soils" = "https://www.sciencebase.gov/catalog/item/568d6554e4b0e7a44bc207f7",
  "Topographic Characteristics" = "https://www.sciencebase.gov/catalog/item/5789408ee4b0c1aacab7770b")
