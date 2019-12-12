library(tidyverse)
library(RSelenium)
library(rvest)
library(lubridate)
library(readr)
library(haven)
library(here)
source(here::here('R/utils_objects.R'))


convert_codebook_to_pdf <- function(file_name) {
  temp      <- readLines(file_name)
  old_name  <- file_name
  file_name <- gsub(".txt", ".Rmd", file_name)
  cat(temp, sep = "  \n", file = file_name)
  suppressMessages(rmarkdown::render(file_name,
                                     rmarkdown::pdf_document(), quiet = TRUE))
  file.remove(old_name)
  file.remove(file_name)
}

save_files <- function(data, year, file_name, save_name, rda_only = FALSE) {
  data <-
    data %>%
    dplyr::mutate_if(is.Date, as.character)

  if (any(nchar(names(data)) > 30)) {
    print(names(data)[nchar(names(data)) > 30])
  }

  assign(paste0(file_name, year), data) # Change name
  save( list = paste0(file_name, year),
        file = paste0(save_name, year, ".rda"))

  if (rda_only == FALSE) {
    codebook_name <- paste0(file_name, "_codebook_", year, ".txt")
    codebook_name <- gsub("__", "_", codebook_name)
    codebook_name <- gsub("_\\.", "\\.", codebook_name)
    memisc::Write(memisc::codebook(data),
                  file = codebook_name)
    convert_codebook_to_pdf(codebook_name)

    do.call("write_dta", list(as.name(paste0(file_name, year)),
                              path = paste0(save_name,
                                            year, ".dta")))

     do.call("write_csv", list(as.name(paste0(file_name, year)),
                               path = paste0(save_name,
                                             year, ".csv")))


    do.call("rm", list(as.name(paste0(file_name, year))))
  }
}

save_as_zip <- function(file_name, pattern = NULL) {
  file_ext <- c("rda", "dta", "csv")
  all_files <- list.files()
  if (!is.null(pattern)) {
    sps_files <- all_files[grep("maltz|manual|sps$|record description",
                                all_files, ignore.case = TRUE)]
    all_files <- list.files(pattern = pattern)
    all_files <- c(sps_files, all_files)
  }

  codebooks <- all_files[grep("maltz|manual|codebook|pdf$|sps$",
                              all_files,
                              ignore.case = TRUE)]
  for (i in seq_along(file_ext)) {
    zip_files <- all_files[grep(file_ext[i], all_files)]
    zip_files <- c(zip_files, codebooks)
    zip_files <- sort(zip_files)

    zip::zip(zipfile = paste0(file_name,
                              file_ext[i], ".zip"),
             files = zip_files)
  }
}



fix_names <- function(names) {
  names <- tolower(names)
  names <- gsub("'|\\(|\\)", "", names)
  names <- gsub("[[:punct:]]", "_", names)
  names <- gsub(" ", "_", names)
  names <- gsub("_+", "_", names)
  names <- gsub("_$|^_", "", names)

  names <- stringr::str_replace_all(names, name_changes)
  return(names)
}

fix_abbreviations <- function(col) {
  col <- gsub("^D$|^U$", NA, col, ignore.case = TRUE)
  return(col)
}




selectTag = function(element){
  if (!inherits(element, "webElement")){
    stop("element should be a web element.")
  }
  if (!identical(element$getElementTagName()[[1]], "select")){
    stop("element does not appear to point to a select element in DOM.")
  }
  options <- element$findChildElements("css", "option")
  optiontext <- vapply(options,
                       function(x)x$getElementText()[[1]],
                       character(1)
  )
  optionvalues <- vapply(options,
                         function(x)x$getElementAttribute("value")[[1]],
                         character(1)
  )
  list(elements = options, text = optiontext, value = optionvalues)
}


selenium_clicker <- function(remDr, using, value) {
  webElem <- remDr$findElement(using = using,
                               value)
  webElem$clickElement()
}

start_rsDriver <- function() {
  count <- 1
  catcher <- NULL
  while (is.null(catcher) & count < 100) {
    catcher <- tryCatch({
      rD <- rsDriver(browser = "firefox",
                     port = as.integer(Sys.Date() + count))
    },
    error = function(e){})
    count <- count + 1
    message(count)
  }
  return(rD)
}

