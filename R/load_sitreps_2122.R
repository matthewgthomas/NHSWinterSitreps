#' Load timeseries of winter situation reports for 2021-22
#'
#' The returned tibble contains:
#' - General & Acute beds
#' - Adult critical care
#'
#' @param sitrep_url URL of the timeseries file
#'
#' @importFrom magrittr "%>%"
#'
load_sitreps_2122 = function(sitrep_url) {
  ##
  ## load sitrep timeseries
  ##
  # download sitrep file
  tmp_sitrep = tempfile()
  download.file(sitrep_url, tmp_sitrep, mode = "wb")

  # custom function to replicate how `tibble` used to rename duplicate column names
  my_repair_names = function(x) vctrs::vec_as_names_legacy(x, sep = "__")

  # load sitrep data
  sitrep_beds_adult = readxl::read_excel(tmp_sitrep, sheet = "Adult G&A beds", skip = 14, .name_repair = my_repair_names)             # general and acute beds
  sitrep_beds_paed  = readxl::read_excel(tmp_sitrep, sheet = "Paediatric G&A beds", skip = 14, .name_repair = my_repair_names)             # general and acute beds
  sitrep_critical   = readxl::read_excel(tmp_sitrep, sheet = "Adult critical care", skip = 14, .name_repair = my_repair_names)  # adult critical care
  sitrep_beds_long  = readxl::read_excel(tmp_sitrep, sheet = "Beds Occ by long stay patients", skip = 14, .name_repair = my_repair_names)
  sitrep_beds_flu   = readxl::read_excel(tmp_sitrep, sheet = "Flu", skip = 14, .name_repair = my_repair_names)
  sitrep_ambo       = readxl::read_excel(tmp_sitrep, sheet = "Ambulance Arrivals and Delays", skip = 14, .name_repair = my_repair_names)
  sitrep_closures   = readxl::read_excel(tmp_sitrep, sheet = "A&E Closures", skip = 14, .name_repair = my_repair_names)
  sitrep_diverts    = readxl::read_excel(tmp_sitrep, sheet = "A&E Diverts", skip = 14, .name_repair = my_repair_names)

  ##
  ## sitrep date range
  ##
  sitrep_dates = readxl::read_excel(tmp_sitrep, sheet = "Adult G&A beds", skip = 12, n_max = 1) %>%
    janitor::remove_empty("cols")

  # clean up dates
  sitrep_dates = t(sitrep_dates) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Date = V1)

  # add columns for lookups to bed occupancy and long stay patients columns
  num_cols = length( names(sitrep_beds_adult)[ grep("Adult G&A beds occ'd", names(sitrep_beds_adult)) ] ) - 1  # how many time series columns are there?

  sitrep_dates$Occupancy_adult_open = c("Adult G&A Beds Open",
                                        paste("Adult G&A Beds Open", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_adult_occ = c("Adult G&A beds occ'd",
                                       paste("Adult G&A beds occ'd", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_paeds_open = c("Paeds G&A Beds Open",
                                        paste("Paeds G&A Beds Open", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_paeds_occ = c("Paeds G&A beds occ'd",
                                       paste("Paeds G&A beds occ'd", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy = c("Occupancy rate",
                             paste("Occupancy rate", seq(1:num_cols), sep="__"))

  sitrep_dates$Flu_GA = c("G&A flu beds",
                          paste("G&A flu beds", seq(1:num_cols), sep="__"))

  sitrep_dates$Flu_CC = c("CC flu beds",
                          paste("CC flu beds", seq(1:num_cols), sep="__"))

  # - Dates for long-stay patients started later -
  sitrep_dates_long = readxl::read_excel(tmp_sitrep, sheet = "Beds Occ by long stay patients", skip = 12, n_max = 1) %>%
    janitor::remove_empty("cols")

  # clean up dates
  sitrep_dates_long = t(sitrep_dates_long) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Date = V1)

  # add columns for lookups to bed occupancy and long stay patients columns
  num_cols = length( names(sitrep_beds_long)[ grep("> 21 days", names(sitrep_beds_long)) ] ) - 1  # how many time series columns are there?

  sitrep_dates_long$LongStay = c("> 21 days",
                                 paste("> 21 days", seq(1:num_cols), sep="__"))

  sitrep_dates_long$LongStay_7 = c("> 7 days",
                                   paste("> 7 days", seq(1:num_cols), sep="__"))

  sitrep_dates = sitrep_dates %>%
    dplyr::left_join(sitrep_dates_long, by = "Date")

  sitrep_dates$Ambo30 = c("Delay 30-60 mins",
                          paste("Delay 30-60 mins", seq(1:num_cols), sep="__"))

  sitrep_dates$Ambo60 = c("Delay >60 mins",
                          paste("Delay >60 mins", seq(1:num_cols), sep="__"))

  # Convert to long format for joining with bed occupancy data in the next section
  sitrep_dates_long = sitrep_dates %>%
    tidyr::pivot_longer(cols = -Date) %>%
    dplyr::select(-name)


  ######################################################################################################
  ## Bed occupancy
  ## - this winter, bed occupancy has been split into adult and paediatric: we'll combine them here
  ##
  # Adult
  sitrep_beds_adult = sitrep_beds_adult %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Adult")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(-c(Code, Name), as.integer))

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  # sitrep_beds_adult$`Occupancy rate` = as.numeric(sitrep_beds_adult$`Adult G&A beds occ'd`) / as.numeric(sitrep_beds_adult$`Adult G&A Beds Open`)

  #... now loop over the rest of the columns, calculating occupancy rates
  # num_cols = length( names(sitrep_beds_adult)[ grep("Adult G&A beds occ'd", names(sitrep_beds_adult)) ] ) - 1  # how many time series columns are there?

  # for (i in 1:num_cols) {
  #   # get current pair of available/occupied columns
  #   tmp_cols = sitrep_beds_adult[, grep( paste0("ADULT G&A BEDS.*__", i, "$"), toupper(colnames(sitrep_beds_adult))) ]
  #   # calculate occupancy rate (needs unlist() otherwise as.numeric() doesn't work)
  #   sitrep_beds_adult$rate_tmp = as.numeric(unlist(tmp_cols[,2])) /  as.numeric(unlist(tmp_cols[,1]))
  #   # rename column to include "__i"
  #   names(sitrep_beds_adult)[ names(sitrep_beds_adult) == "rate_tmp" ] = paste0("Occupancy rate__", i)
  # }

  # keep only new occupancy rate columns
  # sitrep_beds = sitrep_beds %>%
  #   dplyr::select(Code, Name, dplyr::starts_with("Occupancy rate"))

  # Paediatrics
  sitrep_beds_paed = sitrep_beds_paed %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Paeds")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(-c(Code, Name), as.integer))

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  # sitrep_beds_paed$`Occupancy rate` = as.numeric(sitrep_beds_paed$`Paeds G&A beds occ'd`) / as.numeric(sitrep_beds_paed$`Paeds G&A Beds Open`)

  #... now loop over the rest of the columns, calculating occupancy rates
  # num_cols = length( names(sitrep_beds_paed)[ grep("Paeds G&A beds occ'd", names(sitrep_beds_paed)) ] ) - 1  # how many time series columns are there?

  # for (i in 1:num_cols) {
  #   # get current pair of available/occupied columns
  #   tmp_cols = sitrep_beds_paed[, grep( paste0("PAEDS G&A BEDS.*__", i, "$"), toupper(colnames(sitrep_beds_paed))) ]
  #   # calculate occupancy rate (needs unlist() otherwise as.numeric() doesn't work)
  #   sitrep_beds_paed$rate_tmp = as.numeric(unlist(tmp_cols[,2])) /  as.numeric(unlist(tmp_cols[,1]))
  #   # rename column to include "__i"
  #   names(sitrep_beds_paed)[ names(sitrep_beds_paed) == "rate_tmp" ] = paste0("Occupancy rate__", i)
  # }

  # combine adult and paediatrics and convert to long format
  sitrep_beds = dplyr::bind_rows(
    sitrep_beds_adult %>%
      tidyr::gather(Occupancy, `Occupancy rate`, -Code, -Name) %>%
      dplyr::left_join(sitrep_dates_long, by = c("Occupancy" = "value")),
      # dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Occupancy), by = "Occupancy"),  # merge in dates that correspond to column names

    sitrep_beds_paed %>%
      tidyr::gather(Occupancy, `Occupancy rate`, -Code, -Name) %>%
      dplyr::left_join(sitrep_dates_long, by = c("Occupancy" = "value"))
      # dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Occupancy), by = "Occupancy"),  # merge in dates that correspond to column names
  )

  # sum adult and paed beds into a single indicator
  sitrep_beds = sitrep_beds %>%
    dplyr::mutate(Occupancy = stringr::str_remove(Occupancy, "Adult ")) %>%
    dplyr::mutate(Occupancy = stringr::str_remove(Occupancy, "Paeds "))

  sitrep_beds = sitrep_beds %>%
    dplyr::group_by(Code, Name, Occupancy, Date) %>%
    dplyr::summarise(`Occupancy rate` = sum(`Occupancy rate`, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Elongate data so there are columsn for G&A beds open and occupied
  sitrep_beds = sitrep_beds %>%
    dplyr::mutate(Occupancy = stringr::str_remove(Occupancy, "__[0-9]+")) %>%
    tidyr::pivot_wider(names_from = Occupancy, values_from = `Occupancy rate`)

  # Calculate occupancy rates
  sitrep_beds = sitrep_beds %>%
    dplyr::mutate(`Occupancy rate` = `G&A beds occ'd` / `G&A Beds Open`)

  # variable conversions
  sitrep_beds = sitrep_beds %>%
    # dplyr::select(-Occupancy) %>%
    # dplyr::mutate(`Occupancy rate` = dplyr::na_if(`Occupancy rate`, "-")) %>%
    dplyr::mutate(
      # `Occupancy rate` = as.numeric(`Occupancy rate`),
      Date = as.POSIXct(Date)
    )


  #########################################################################################
  ## Adult critical care bed occupancy rate
  ##
  # tidy up data
  sitrep_critical = sitrep_critical %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("CC")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows")

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  sitrep_critical$`Occupancy rate` = as.numeric(sitrep_critical$`CC Adult Occ`) / as.numeric(sitrep_critical$`CC Adult Open`)

  #... now loop over the rest of the columns, calculating occupancy rates
  num_cols = length( names(sitrep_critical)[ grep("CC Adult Occ", names(sitrep_critical)) ] ) - 1  # how many time series columns are there?

  for (i in 1:num_cols) {
    # get current pair of available/occupied columns
    tmp_cols = sitrep_critical[, grep( paste0("CC Adult.*__", i, "$"), colnames(sitrep_critical)) ]
    # calculate occupancy rate (needs unlist() otherwise as.numeric() doesn't work)
    sitrep_critical$rate_tmp = as.numeric(unlist(tmp_cols[,2])) /  as.numeric(unlist(tmp_cols[,1]))
    # rename column to include "__i"
    names(sitrep_critical)[ names(sitrep_critical) == "rate_tmp" ] = paste0("Occupancy rate__", i)
  }

  # keep only new occupancy rate columns
  sitrep_critical = sitrep_critical %>%
    dplyr::select(Code, Name, dplyr::starts_with("Occupancy rate"))

  # convert to long format
  sitrep_critical = sitrep_critical %>%
    tidyr::gather(Occupancy, `Occupancy rate`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Occupancy), by = "Occupancy")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_critical = sitrep_critical %>%
    dplyr::select(-Occupancy) %>%
    dplyr::mutate(`Occupancy rate` = dplyr::na_if(`Occupancy rate`, "-")) %>%
    dplyr::mutate(`Occupancy rate` = as.numeric(`Occupancy rate`),
                  Date = as.POSIXct(Date)) %>%
    dplyr::rename(`Critical care beds occupancy rate` = `Occupancy rate`)


  ######################################################################################################
  ## Beds occupied by long-stay patients (> 7 days and > 21 days)
  ##

  ##
  ## > 7 days
  ##
  sitrep_beds_long_7 = sitrep_beds_long %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("> 7 days"))  # keep only people in for more than three weeks

  # convert to long format
  sitrep_beds_long_7 = sitrep_beds_long_7 %>%
    tidyr::gather(LongStay, `No. beds occupied by long-stay patients (> 7 days)`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, LongStay = LongStay_7), by = "LongStay")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_long_7 = sitrep_beds_long_7 %>%
    dplyr::select(-LongStay) %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  ##
  ## > 21 days
  ##
  sitrep_beds_long_21 = sitrep_beds_long %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("> 21 days"))  # keep only people in for more than three weeks

  # convert to long format
  sitrep_beds_long_21 = sitrep_beds_long_21 %>%
    tidyr::gather(LongStay, `No. beds occupied by long-stay patients (> 21 days)`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, LongStay), by = "LongStay")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_long_21 = sitrep_beds_long_21 %>%
    dplyr::select(-LongStay) %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  ######################################################################################################
  ## Beds occupied by flu patients (General & Acute, and Critical Care)
  ##

  ##
  ## General & Acute
  ##
  sitrep_flu_GA = sitrep_beds_flu %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("G&A"))

  # convert to long format
  sitrep_flu_GA = sitrep_flu_GA %>%
    tidyr::gather(Flu, `No. beds occupied by flu patients (G&A)`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Flu = Flu_GA), by = "Flu")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_flu_GA = sitrep_flu_GA %>%
    dplyr::select(-Flu) %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  ##
  ## Critical care
  ##
  sitrep_flu_CC = sitrep_beds_flu %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("CC"))

  # convert to long format
  sitrep_flu_CC = sitrep_flu_CC %>%
    tidyr::gather(Flu, `No. beds occupied by flu patients (Critical Care)`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Flu = Flu_CC), by = "Flu")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_flu_CC = sitrep_flu_CC %>%
    dplyr::select(-Flu) %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  ######################################################################################################
  ## Ambulance delays
  ##

  ##
  ## Delay 30-60 mins
  ##
  sitrep_ambo30 = sitrep_ambo %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Delay 30"))

  # convert to long format
  sitrep_ambo30 = sitrep_ambo30 %>%
    tidyr::gather(Ambo30, Delays, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Ambo30), by = "Ambo30")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_ambo30 = sitrep_ambo30 %>%
    dplyr::select(-Ambo30) %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  ##
  ## Delay >60 mins
  ##
  sitrep_ambo60 = sitrep_ambo %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Delay >"))

  # convert to long format
  sitrep_ambo60 = sitrep_ambo60 %>%
    tidyr::gather(Ambo60, Delays, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Ambo60), by = "Ambo60")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_ambo60 = sitrep_ambo60 %>%
    dplyr::select(-Ambo60) %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  ######################################################################################################
  ## A&E diverts
  ##
  sitrep_diverts = sitrep_diverts %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(-`NHS England Region`, -V__1)

  # convert to long format
  sitrep_diverts = sitrep_diverts %>%
    tidyr::gather(Date, `Diverts`, -Code, -Name) %>%
    dplyr::mutate(Date = janitor::excel_numeric_to_date(as.integer(Date)))  # convert numbers to dates

  # variable conversions
  sitrep_diverts = sitrep_diverts %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  ######################################################################################################
  ## A&E closures
  ##
  sitrep_closures = sitrep_closures %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(-`NHS England Region`, -V__1)

  # convert to long format
  sitrep_closures = sitrep_closures %>%
    tidyr::gather(Date, Closures, -Code, -Name) %>%
    dplyr::mutate(Date = janitor::excel_numeric_to_date(as.integer(Date)))  # convert numbers to dates

  # variable conversions
  sitrep_closures = sitrep_closures %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  ######################################################################################################
  ## Combine sitreps into one dataframe
  ##
  sitrep_ambo30 = na.omit(sitrep_ambo30)
  sitrep_ambo60 = na.omit(sitrep_ambo60)
  sitrep_beds = na.omit(sitrep_beds)
  sitrep_critical = na.omit(sitrep_critical)
  sitrep_beds_long_7 = na.omit(sitrep_beds_long_7)
  sitrep_beds_long_21 = na.omit(sitrep_beds_long_21)
  sitrep_flu_GA = na.omit(sitrep_flu_GA)
  sitrep_flu_CC = na.omit(sitrep_flu_CC)
  sitrep_closures = na.omit(sitrep_closures)
  sitrep_diverts = na.omit(sitrep_diverts)

  sitrep = sitrep_beds %>%
    dplyr::left_join(sitrep_ambo30       %>% dplyr::select(Code, Name, Date, Delays30 = Delays), by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_ambo60       %>% dplyr::select(Code, Name, Date, Delays60 = Delays), by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_critical     %>% dplyr::select(Code, Name, Date, `Critical care beds occupancy rate`), by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_7  %>% dplyr::select(Code, Name, Date, `No. beds occupied by long-stay patients (> 7 days)`),  by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_21 %>% dplyr::select(Code, Name, Date, `No. beds occupied by long-stay patients (> 21 days)`),  by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_flu_GA       %>% dplyr::select(Code, Name, Date, `No. beds occupied by flu patients (G&A)`),  by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_flu_CC       %>% dplyr::select(Code, Name, Date, `No. beds occupied by flu patients (Critical Care)`),  by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_closures     %>% dplyr::select(Code, Name, Date, Closures), by = c("Code", "Name", "Date")) %>%
    dplyr::left_join(sitrep_diverts      %>% dplyr::select(Code, Name, Date, Diverts), by = c("Code", "Name", "Date"))

  # re-order columns
  sitrep = sitrep %>% dplyr::select(Code, Name, Date, dplyr::everything())

  # remove temp file
  unlink(tmp_sitrep)

  # return sitrep
  sitrep
}
