library(tidyverse)
library(rvest)
library(lubridate)

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
  col <- gsub("^D|U$", NA, col, ignore.case = TRUE)
  return(col)
}

name_changes <- c(
  "^number_of_inmates_classified_as_3rd_strike$"                  = "num_inmate_are_3rd_striker",
  "^number_of_inmates_classified_as_2nd_strike$"                  = "num_inmate_are_2nd_striker",
  "^number_of_unserved_felony_warrants_in_your_county$"           = "num_unserved_felony_warrants",
  "^number_of_unserved_misdemeanor_warrants_in_your_county$"      = "num_unserved_misdemean_warrants",
  "^percentage_of_your_inmates_believed_to_be_illegal_aliens$"    = "percent_inmates_illegal_alien",
  "^number_of_inmate_assaults_on_staff_during_this_quarter$"      = "num_inmate_assaults_on_staff",
  "^money_spent_on_medication_during_previous_quarter$"           = "money_spent_medication_last_qtr",
  "^money_spent_on_psych_medication_during_previous_quarter$"     = "money_spent_psych_med_last_qtr",
  "^avg_number_of_felony_inmates_unsentenced$"                    = "avg_felony_inmate_unsentenced",
  "^avg_number_of_felony_inmates_sentenced$"                      = "avg_felony_inmate_sentenced",
  "^avg_number_of_felony_inmates_total$"                          = "avg_felony_inmate_total",
  "^avg_number_of_misdemeanor_inmates_unsentenced$"               = "avg_misdemean_inmate_unsentenced",
  "^avg_number_of_misdemeanor_inmates_sentenced$"                 = "avg_misdemean_inmate_sentenced",
  "^avg_number_of_misdemeanor_inmates_total$"                     = "avg_misdemean_inmate_total",
  "^highest_one_day_population_for_this_month_occurred_on$"       = "day_of_highest_count",
  "^the_highest_count_was$"                                       = "highest_inmate_count",
  "^mental_health_cases_opened_last_day_of_the_month$"            = "mental_heath_case_open_end_month",
  "^new_mental_health_cases_opened_during_this_month$"            = "num_new_mental_health_cases",
  "^inmates_last_day_of_the_month_receiving_psych_medication$"    = "num_inmate_get_psych_meds",
  "^inmates_assigned_to_mental_health_beds_last_day_of_month$"    = "num_inmate_get_mental_heath_bed",
  "^inmates_that_were_seen_at_inmate_sick_call_this_month$"       = "num_inmates_seen_sick_call",
  "^physician_practitioner_occurrences_during_this_month$"        = "num_doctor_occurrences",
  "^off_site_medical_appointments_during_this_month$"             = "num_offsite_medical_appointment",
  "^dental_encounters_during_this_month$"                         = "num_dental_encounters",
  "^inmates_assigned_to_medical_beds_last_day_of_the_month$"      = "avg_inmate_get_sick_bed",
  "^avg_of_inmates_not_assigned_to_housing$"                      = "avg_inmate_not_assign_housing",
  "^avg_of_your_inmates_in_contract_beds_in_other_jurisdictions$" = "avg_own_inmate_housed_elsewhere",
  "^avg_of_federal_inmates_housed_in_your_system_on_contract$"    = "avg_fed_inmate_housed_contract",
  "^avg_of_state_inmates_housed_in_your_system_on_contract$"      = "avg_state_inmate_housed_contract",
  "^avg_of_inmates_from_other_co_in_your_system_on_contract$"     = "avg_local_inmate_housed_contract",
  "^avg_of_inmates_sent_and_awaiting_transport_to_state_prison$"  = "avg_inmate_wait_transport_prison",
  "^avg_of_inmates_in_hospitals_outside_of_your_jail_facilities$" = "avg_inmate_in_hospital",
  "^total_of_persons_booked_this_month$"                          = "total_num_persons_booked",
  "^total_of_pretrial_released_due_to_lack_of_housing_capacity$"  = "tot_pretrial_release_lack_bed",
  "^total_of_sent_released_due_to_lack_of_housing_capacity$"      = "tot_sentenced_release_lack_bed",
  "^total_of_juveniles_in_custody_this_month_per_707_w_i_code$"   = "total_juv_in_custody",
  "^avg_of_inmates_assigned_to_medical_beds$"                     = "avg_inmates_get_medical_bed",
  "^avg_of_inmates_assigned_to_mental_health_beds$"               = "avg_inmates_get_mental_heath_bed",
  "^avg_of_inmates_requiring_regular_medical_attention$"          = "avg_inmate_need_reg_med_attent",
  "^avg_of_inmates_requiring_regular_mental_health_attention$"    = "avg_inmates_need_reg_ment_health",
  "^all_releases_from_your_system_als$"                           = "avg_length_stay_all_releases",
  "^pre_trial_releases_als$"                                      = "avg_length_stay_pretrial_release",
  "^sentenced_releases_als$"                                      = "avg_length_stay_sentnced_release",
  "^total_facility_adp$"                                          = "avg_daily_pop_total_facility",
  "^unsentenced_males$"                                           = "avg_daily_pop_unsentenced_male",
  "^sentenced_males$"                                             = "avg_daily_pop_sentenced_male",
  "^unsentenced_females$"                                         = "avg_daily_pop_unsentenced_female",
  "^sentenced_females$"                                           = "avg_daily_pop_sentenced_female",
  "^adp_totals_unsentenced_males$"                                = "avg_daily_pop_unsentenced_male",
  "^adp_totals_unsentenced_females$"                              = "avg_daily_pop_unsentenced_female",
  "^adp_totals_sentenced_males$"                                  = "avg_daily_pop_sentenced_male",
  "^adp_totals_sentenced_females$"                                = "avg_daily_pop_sentenced_female",
  "^adp_totals_jurisdiction$"                                     = "avg_daily_pop_total_jurisdiction"
)

county_match <- data.frame(jurisdiction = c("Alameda Sheriff's Department",
                                            "Amador Sheriff's Department",
                                            "Butte Sheriff's Department",
                                            "Calaveras Sheriff's Department",
                                            "Colusa Sheriff's Department",
                                            "Contra Costa Sheriff's Department",
                                            "Del Norte Sheriff's Department",
                                            "El Dorado Sheriff's Department",
                                            "Fresno Sheriff's Department",
                                            "Glenn Sheriff's Department",
                                            "Humboldt Sheriff's Department",
                                            "Imperial Sheriff's Department",
                                            "Inyo Sheriff's Department",
                                            "Kern Sheriff's Department",
                                            "Kings Sheriff's Department",
                                            "Lake Sheriff's Department",
                                            "Lassen Sheriff's Department",
                                            "Los Angeles Sheriff's Department",
                                            "Madera Corrections Department",
                                            "Marin Sheriff's Department",
                                            "Mariposa Sheriff's Department",
                                            "Mendocino Sheriff's Department",
                                            "Merced Sheriff's Department",
                                            "Modoc Sheriff's Department",
                                            "Mono Sheriff's Department",
                                            "Monterey Sheriff's Department",
                                            "Napa Corrections Department",
                                            "Nevada Sheriff's Department",
                                            "Orange Sheriff's Department",
                                            "Placer Sheriff's Department",
                                            "Plumas Sheriff's Department",
                                            "Riverside Sheriff's Department",
                                            "Sacramento Sheriff's Department",
                                            "San Benito Sheriff's Department",
                                            "San Bernardino Sheriff's Department",
                                            "San Diego Sheriff's Department",
                                            "San Diego Work Furlough",
                                            "San Francisco Sheriff's Department",
                                            "San Joaquin Sheriff's Department",
                                            "San Luis Obispo Sheriff's Department",
                                            "San Mateo Sheriff's Department",
                                            "Santa Barbara Sheriff's Department",
                                            "Santa Clara Corrections Department",
                                            "Santa Cruz Sheriff's Department",
                                            "Shasta Sheriff's Department",
                                            "Siskiyou Sheriff's Department",
                                            "Solano Sheriff's Department",
                                            "Sonoma Sheriff's Department",
                                            "Stanislaus Sheriff's Department",
                                            "Sutter Sheriff's Department",
                                            "Tehama Sheriff's Department",
                                            "Trinity Sheriff's Department",
                                            "Tulare Sheriff's Department",
                                            "Tuolumne Sheriff's Department",
                                            "Ventura Sheriff's Department",
                                            "Yolo Sheriff's Department",
                                            "Yuba Sheriff's Department",
                                            "Santa Ana Police Department",
                                            "Sierra Sheriff's Department",
                                            "Ventura Work Furlough",
                                            "Santa Clara Probation Department",
                                            "Oakland Police Department",
                                            "Scapular House",
                                            "San Diego Probation Department"),
                           census_county_name = c("Alameda County",
                                                  "Amador County",
                                                  "Butte County",
                                                  "Calaveras County",
                                                  "Colusa County",
                                                  "Contra Costa County",
                                                  "Del Norte County",
                                                  "El Dorado County",
                                                  "Fresno County",
                                                  "Glenn County",
                                                  "Humboldt County",
                                                  "Imperial County",
                                                  "Inyo County",
                                                  "Kern County",
                                                  "Kings County",
                                                  "Lake County",
                                                  "Lassen County",
                                                  "Los Angeles County",
                                                  "Madera County",
                                                  "Marin County",
                                                  "Mariposa County",
                                                  "Mendocino County",
                                                  "Merced County",
                                                  "Modoc County",
                                                  "Mono County",
                                                  "Monterey County",
                                                  "Napa County",
                                                  "Nevada County",
                                                  "Orange County",
                                                  "Placer County",
                                                  "Plumas County",
                                                  "Riverside County",
                                                  "Sacramento County",
                                                  "San Benito County",
                                                  "San Bernardino County",
                                                  "San Diego County",
                                                  "San Diego County",
                                                  "San Francisco County",
                                                  "San Joaquin County",
                                                  "San Luis Obispo County",
                                                  "San Mateo County",
                                                  "Santa Barbara County",
                                                  "Santa Clara County",
                                                  "Santa Cruz County",
                                                  "Shasta County",
                                                  "Siskiyou County",
                                                  "Solano County",
                                                  "Sonoma County",
                                                  "Stanislaus County",
                                                  "Sutter County",
                                                  "Tehama County",
                                                  "Trinity County",
                                                  "Tulare County",
                                                  "Tuolumne County",
                                                  "Ventura County",
                                                  "Yolo County",
                                                  "Yuba County",
                                                  "Orange County",
                                                  "Sierra County",
                                                  "Ventura County",
                                                  "Santa Clara County",
                                                  "Alameda County",
                                                  "Los Angeles County",
                                                  "San Diego County"),
                           fips_state_code  = rep("06", 64),
                           fips_county_code = c("001",
                                                "005",
                                                "007",
                                                "009",
                                                "011",
                                                "013",
                                                "015",
                                                "017",
                                                "019",
                                                "021",
                                                "023",
                                                "025",
                                                "027",
                                                "029",
                                                "031",
                                                "033",
                                                "035",
                                                "037",
                                                "039",
                                                "041",
                                                "043",
                                                "045",
                                                "047",
                                                "049",
                                                "051",
                                                "053",
                                                "055",
                                                "057",
                                                "059",
                                                "061",
                                                "063",
                                                "065",
                                                "067",
                                                "069",
                                                "071",
                                                "073",
                                                "073",
                                                "075",
                                                "077",
                                                "079",
                                                "081",
                                                "083",
                                                "085",
                                                "087",
                                                "089",
                                                "093",
                                                "095",
                                                "097",
                                                "099",
                                                "101",
                                                "103",
                                                "105",
                                                "107",
                                                "109",
                                                "111",
                                                "113",
                                                "115",
                                                "059",
                                                "091",
                                                "111",
                                                "085",
                                                "001",
                                                "037",
                                                "073"),
                           stringsAsFactors = FALSE)
county_match$fips_state_county_code = paste0(county_match$fips_state_code,
                                             county_match$fips_county_code)



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

extraCapabilities <- list(
  chromeOptions =
    list(prefs = list(
      "download.default_directory" =
        "C:/Users/user/Dropbox/R_project/california_jails/raw_data")))

selenium_clicker <- function(remDr, using, value) {
  webElem <- remDr$findElement(using = using,
                               value)
  webElem$clickElement()
}


start_rsDriver <- function(extraCapabilities = NULL) {
  count <- 1
  catcher <- NULL
  while(is.null(catcher) & count < 100) {
    catcher <- tryCatch({
      if (!is.null(extraCapabilities)) {
        rD <- rsDriver(verbose = FALSE, port = as.integer(Sys.Date() + count),
                       extraCapabilities = extraCapabilities)
      } else {
        rD <- rsDriver(verbose = FALSE, port = as.integer(Sys.Date() + count))
      }
    },
    error = function(e){})
    count <- count + 1
    message(count)
  }
  return(rD)
}

