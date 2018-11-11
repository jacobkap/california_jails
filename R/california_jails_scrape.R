library(RSelenium)
library(rvest)
source('C:/Users/user/Dropbox/R_project/california_jails/R/california_jails_utils.R')
rD <- start_rsDriver(extraCapabilities = extraCapabilities)
remDr <- rD$client

get_jails("facility_monthly", 1, old_data = FALSE)
get_jails("county_monthly",   2, old_data = FALSE)
get_jails("county_quarterly", 3, old_data = FALSE)
get_jails("facility_monthly", 1, old_data = TRUE)
get_jails("county_monthly",   2, old_data = TRUE)
get_jails("county_quarterly", 3, old_data = TRUE)

get_jails <- function(type, type_number, old_data = FALSE) {
  url <- "https://app.bscc.ca.gov/joq//jps/queryselection.asp"
  remDr$navigate(url); Sys.sleep(2)
  date_range <- 1
  if (old_data) date_range <- 2

  webElem <- remDr$findElement(using = "name", "ReportingRange")
  reporting_range <- selectTag(webElem)
  reporting_range$elements[[date_range]]$clickElement()

  selenium_clicker(remDr, using = "name", value = "Continue")


  webElem <- remDr$findElement(using = "name", "year")
  years <- selectTag(webElem)
  length_years <- length(years$elements)
  for (year in 1:length_years) {

    remDr$navigate(url); Sys.sleep(2)
    webElem <- remDr$findElement(using = "name", "DataType")
    data_type <- selectTag(webElem)
    data_type$elements[[type_number]]$clickElement()

    webElem <- remDr$findElement(using = "name", "ReportingRange")
    reporting_range <- selectTag(webElem)
    reporting_range$elements[[date_range]]$clickElement()

    # Go to next page
    selenium_clicker(remDr, using = "name", value = "Continue")
    webElem <- remDr$findElement(using = "name", "year")
    years <- selectTag(webElem)
    years$elements[[year]]$clickElement()

    if (type != "county_quarterly") {
      # Set ending month to december
      webElem <- remDr$findElement(using = "name", "Month_To")
      month_to <- selectTag(webElem)
      month_to$elements[[length(month_to$elements)]]$clickElement()

      # Get starting month
      webElem <- remDr$findElement(using = "name", "Month_From")
      month_from <- selectTag(webElem)

      if (years$value[year] == "1995") {
        month_from$elements[[10]]$clickElement()
      } else {
        month_from$elements[[1]]$clickElement()
      }
      if (years$value[year] == "2018") {
        month_to$elements[[length(month_to$elements)-6]]$clickElement()
      }
    } else {
      # Set ending quarter to quarter 4
      webElem <- remDr$findElement(using = "name", "Quarter_To")
      quarter_to <- selectTag(webElem)
      quarter_to$elements[[4]]$clickElement()

      # Get starting quarter
      webElem <- remDr$findElement(using = "name", "Quarter_From")
      quarter_from <- selectTag(webElem)


      if (years$value[year] == "1995") {
        quarter_from$elements[[4]]$clickElement()
      } else {
        quarter_from$elements[[1]]$clickElement()
      }
      if (years$value[year] == "2018") {
        quarter_to$elements[[2]]$clickElement()
      }
    }


    # Select all jurisdictions
    webElem <- remDr$findElement(using = "name", "jurisdictions")
    jurisdictions <- selectTag(webElem)
    for (i in 1:length(jurisdictions$elements)) {
      jurisdictions$elements[[i]]$clickElement()
    }

    # Select all variables
    webElem <- remDr$findElement(using = "name", "variables")
    variables <- selectTag(webElem)
    for (i in 1:length(variables$elements)) {
      variables$elements[[i]]$clickElement()
    }


    # Save as Excel
    selenium_clicker(remDr, using = "css",
                     value = 'body > form > table:nth-child(4) > tbody > tr > td:nth-child(4) > div > font > font > font:nth-child(3) > font > font > input[type="submit"]')
    Sys.sleep(25)

    setwd("C:/Users/user/Dropbox/R_project/california_jails/raw_data")
    files <- list.files(pattern = "QueryResult.xls")
    file.rename(files, paste0(type, "_", years$value[year], ".xls"))

  }
}


