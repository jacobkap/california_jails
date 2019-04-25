source(here::here('R/utils.R'))


jails <- clean_jails()
california_jails_facility_monthly <- jails[[1]]
california_jails_county_monthly   <- jails[[2]]
california_jails_county_quarterly <- jails[[3]]

summary(california_jails_facility_monthly)
summary(california_jails_county_monthly)
summary(california_jails_county_quarterly)

setwd(here::here("clean_data"))
save_files(california_jails_facility_monthly,
           "_1995_2018",
           file_name = "california_jail_facility_monthly",
           save_name = "california_jail_facility_monthly")
save_files(california_jails_county_monthly,
           "_1995_2018",
           file_name = "california_jail_county_monthly",
           save_name = "california_jail_county_monthly")
save_files(california_jails_county_quarterly,
           "_1995_2018",
           file_name = "california_jail_county_quarterly",
           save_name = "california_jail_county_quarterly")

save_as_zip("california_jail_survey_1995_2018_")

clean_jails <- function() {

  setwd(here::here("raw_data"))
  facility_files         <- list.files(pattern = "facility_monthly")
  county_monthly_files   <- list.files(pattern = "county_monthly")
  county_quarterly_files <- list.files(pattern = "county_quarterly")

  facility         <- data.frame(stringsAsFactors = FALSE)
  county_monthly   <- data.frame(stringsAsFactors = FALSE)
  county_quarterly <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:length(facility_files)) {
    facility <-
      read_html(facility_files[i]) %>%
      html_node("table") %>%
      html_table(header = TRUE) %>%
      dplyr::rename_all(fix_names) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(fix_abbreviations) %>%
      dplyr::mutate_at(4:9, parse_number) %>%
      dplyr::mutate(jurisdiction = gsub("Dept.$", "Department",
                                        jurisdiction),
                    month = str_replace_all(month, month_fix)) %>%
      dplyr::bind_rows(facility)

    num_cols <- c(4:14, 16:34)
    if (i >= 8) num_cols <- c(4:14, 16:36)
    county_monthly <-
      read_html(county_monthly_files[i]) %>%
      html_node("table") %>%
      html_table(header = TRUE) %>%
      dplyr::rename_all(fix_names) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(fix_abbreviations) %>%
      dplyr::mutate_at(num_cols, parse_number) %>%
      dplyr::mutate(jurisdiction         = gsub("Dept.$", "Department",
                                        jurisdiction),
                    day_of_highest_count = lubridate::mdy(day_of_highest_count),
                    day_of_highest_count = as.character(day_of_highest_count),
                    month                = parse_number(month),
                    month = str_replace_all(month, month_fix)) %>%
      dplyr::bind_rows(county_monthly)

    county_quarterly <-
      read_html(county_quarterly_files[i]) %>%
      html_node("table") %>%
      html_table(header = TRUE) %>%
      dplyr::rename_all(fix_names) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(fix_abbreviations) %>%
      dplyr::mutate_at(3:9, parse_number) %>%
      dplyr::mutate(jurisdiction = gsub("Dept.$", "Department",
                                        jurisdiction)) %>%
      dplyr::bind_rows(county_quarterly)

    message(i)
  }


  facility <-
    facility %>%
    right_join(county_match) %>%
    dplyr::select(jurisdiction,
                  year,
                  month,
                  census_county_name,
                  fips_state_code,
                  fips_county_code,
                  fips_state_county_code,
                  everything()) %>%
    dplyr::arrange(desc(year),
                   desc(month),
                   census_county_name)
  county_monthly <-
    county_monthly %>%
    right_join(county_match) %>%
    dplyr::select(jurisdiction,
                  year,
                  month,
                  census_county_name,
                  fips_state_code,
                  fips_county_code,
                  fips_state_county_code,
                  everything()) %>%
    dplyr::arrange(desc(year),
                   desc(month),
                   census_county_name)

  county_quarterly <-
    county_quarterly %>%
    right_join(county_match) %>%
    dplyr::select(jurisdiction,
                  year,
                  quarter,
                  census_county_name,
                  fips_state_code,
                  fips_county_code,
                  fips_state_county_code,
                  everything()) %>%
    dplyr::arrange(desc(year),
                   desc(quarter),
                   census_county_name)



  jails <- list(facility, county_monthly, county_quarterly)
  return(jails)
}


