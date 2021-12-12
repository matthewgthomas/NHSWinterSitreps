#' Load timeseries of winter situation reports in its most up-to-date format (from 2017-18 to 2019-20)
#'
#' The returned tibble contains:
#' - A&E diverts
#' - A&E closures
#' - Ambulance delays
#' - Bed occupancy
#' - Beds occupied by long-stay patients (> 7 days and > 21 days)
#' - Beds closed due to diarrhoea, vomiting, norovirus
#'
#' @param sitrep_url URL of the timeseries file
#' @param closures_sheet_name Name of the 'A&E Closures' worksheet (it can differ from winter to winter)
#' @param diverts_sheet_name Name of the 'A&E Diverts' worksheet (it can differ from winter to winter)
#'
#' @importFrom magrittr "%>%"
#'
load_sitreps_generic = function(sitrep_url,
                                closures_sheet_name = "A&E Closures",
                                diverts_sheet_name = "A&E Diverts") {
  ##
  ## load sitrep timeseries
  ##
  # download sitrep file
  tmp_sitrep = tempfile()
  download.file(sitrep_url, tmp_sitrep, mode = "wb")

  # custom function to replicate how `tibble` used to rename duplicate column names
  my_repair_names = function(x) vctrs::vec_as_names_legacy(x, sep = "__")

  # load sitrep data
  sitrep_beds      = readxl::read_excel(tmp_sitrep, sheet = "G&A beds", skip = 14, .name_repair = my_repair_names)        # general and acute beds
  sitrep_critical  = readxl::read_excel(tmp_sitrep, sheet = "Adult critical care", skip = 14, .name_repair = my_repair_names)  # adult critical care
  sitrep_closures  = readxl::read_excel(tmp_sitrep, sheet = closures_sheet_name, skip = 14, .name_repair = my_repair_names)
  sitrep_diverts   = readxl::read_excel(tmp_sitrep, sheet = diverts_sheet_name, skip = 14, .name_repair = my_repair_names)
  sitrep_beds_long = readxl::read_excel(tmp_sitrep, sheet = "Beds Occ by long stay patients", skip = 14, .name_repair = my_repair_names)
  sitrep_beds_noro = readxl::read_excel(tmp_sitrep, sheet = "D&V, Norovirus", skip = 14, .name_repair = my_repair_names)
  sitrep_ambo      = readxl::read_excel(tmp_sitrep, sheet = "Ambulance Arrivals and Delays", skip = 14, .name_repair = my_repair_names)

  ##
  ## sitrep date range
  ##
  sitrep_dates = readxl::read_excel(tmp_sitrep, sheet = "G&A beds", skip = 12, n_max = 1) %>%
    janitor::remove_empty("cols")

  # clean up dates
  sitrep_dates = t(sitrep_dates) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Date = V1)

  # add columns for lookups to bed occupancy and long stay patients columns
  num_cols = length( names(sitrep_beds)[ grep("Occupancy", names(sitrep_beds)) ] ) - 1  # how many time series columns are there?

  sitrep_dates$Occupancy = c("Occupancy rate",
                             paste("Occupancy rate", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_open = c("Total Beds Open",
                                  paste("Total Beds Open", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_occ = c("Total beds occ'd",
                                 paste("Total beds occ'd", seq(1:num_cols), sep="__"))

  sitrep_dates$LongStay = c("> 21 days",
                            paste("> 21 days", seq(1:num_cols), sep="__"))

  sitrep_dates$LongStay_7 = c("> 7 days",
                              paste("> 7 days", seq(1:num_cols), sep="__"))

  sitrep_dates$BedsClosed = c("Beds closed",
                              paste("Beds closed", seq(1:num_cols), sep="__"))

  sitrep_dates$BedsClosedUnoc = c("Beds closed unocc",
                                  paste("Beds closed unocc", seq(1:num_cols), sep="__"))

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
  ##
  sitrep_beds = sitrep_beds %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Total"), dplyr::starts_with("Occupancy rate")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(-c(Code, Name), as.integer))

  # convert to long format
  sitrep_beds = sitrep_beds %>%
    tidyr::gather(Occupancy, `Occupancy rate`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates_long, by = c("Occupancy" = "value"))
    # dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Occupancy), by = "Occupancy")  # merge in dates that correspond to column names

  # Elongate data so there are columsn for G&A beds open and occupied
  sitrep_beds = sitrep_beds %>%
    dplyr::mutate(Occupancy = stringr::str_remove(Occupancy, "__[0-9]+")) %>%
    tidyr::pivot_wider(names_from = Occupancy, values_from = `Occupancy rate`)

  sitrep_beds = sitrep_beds %>%
    dplyr::rename(
      `G&A beds occ'd` = `Total beds occ'd`,
      `G&A Beds Open` = `Total Beds Open`
    )

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
    dplyr::slice(-c(2))   # skip blank line

  if (length( names(sitrep_critical)[ grep("Occupancy", names(sitrep_critical)) ] ) > 0) {
    sitrep_critical = sitrep_critical %>%
      dplyr::select(Code, Name, dplyr::starts_with("Occupancy rate")) %>%   # keep only bed occupancy rates
      janitor::remove_empty("rows")

  } else {
    # data doesn't contain a pre-calculated occupancy rate, so calculate one
    sitrep_critical = sitrep_critical %>%
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
  ## A&E diverts
  ##
  sitrep_diverts = sitrep_diverts %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
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
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(-`NHS England Region`, -V__1)

  # convert to long format
  sitrep_closures = sitrep_closures %>%
    tidyr::gather(Date, Closures, -Code, -Name) %>%
    dplyr::mutate(Date = janitor::excel_numeric_to_date(as.integer(Date)))  # convert numbers to dates

  # variable conversions
  sitrep_closures = sitrep_closures %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  ######################################################################################################
  ## Beds occupied by long-stay patients (> 7 days and > 21 days)
  ##

  ##
  ## > 7 days
  ##
  sitrep_beds_long_7 = sitrep_beds_long %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
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
    dplyr::slice(-c(2)) %>%   # skip blank line
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
  ## Beds closed due to diarrhoea, vomiting, norovirus
  ##

  ##
  ## beds closed
  ##
  sitrep_beds_closed = sitrep_beds_noro %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, `Beds closed`, dplyr::starts_with("Beds closed__"))

  # convert to long format
  sitrep_beds_closed = sitrep_beds_closed %>%
    tidyr::gather(BedsClosed, `No. beds closed due to norovirus etc.`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, BedsClosed), by = "BedsClosed")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_closed = sitrep_beds_closed %>%
    dplyr::select(-BedsClosed) %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  ##
  ## beds closed unoccupied
  ##
  sitrep_beds_closed_unoc = sitrep_beds_noro %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, `Beds closed unocc`, dplyr::starts_with("Beds closed unocc__"))

  # convert to long format
  sitrep_beds_closed_unoc = sitrep_beds_closed_unoc %>%
    tidyr::gather(BedsClosedUnoc, `No. unoccupied beds closed due to norovirus etc.`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, BedsClosedUnoc), by = "BedsClosedUnoc")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_closed_unoc = sitrep_beds_closed_unoc %>%
    dplyr::select(-BedsClosedUnoc) %>%
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
  ## Combine sitreps into one dataframe
  ##

  ##
  ## Make master list of Trusts and dates
  ##
  sitrep_trusts = dplyr::bind_rows(
    sitrep_beds        %>% dplyr::select(Code, Name, Date),
    sitrep_critical    %>% dplyr::select(Code, Name, Date),
    sitrep_closures    %>% dplyr::select(Code, Name, Date),
    sitrep_diverts     %>% dplyr::select(Code, Name, Date),
    sitrep_beds_long_7 %>% dplyr::select(Code, Name, Date),
    sitrep_beds_closed %>% dplyr::select(Code, Name, Date),
    sitrep_ambo30      %>% dplyr::select(Code, Name, Date)
  ) %>%
    dplyr::distinct() %>%
    na.omit()

  sitrep_ambo30 = na.omit(sitrep_ambo30)
  sitrep_ambo60 = na.omit(sitrep_ambo60)
  sitrep_beds = na.omit(sitrep_beds)
  sitrep_critical = na.omit(sitrep_critical)
  sitrep_closures = na.omit(sitrep_closures)
  sitrep_diverts = na.omit(sitrep_diverts)
  sitrep_beds_closed = na.omit(sitrep_beds_closed)
  sitrep_beds_closed_unoc = na.omit(sitrep_beds_closed_unoc)
  sitrep_beds_long_7 = na.omit(sitrep_beds_long_7)
  sitrep_beds_long_21 = na.omit(sitrep_beds_long_21)

  sitrep = sitrep_trusts %>%
    dplyr::left_join(sitrep_ambo30           %>% dplyr::select(Code, Date, Delays30 = Delays),                                      by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_ambo60           %>% dplyr::select(Code, Date, Delays60 = Delays),                                      by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds             %>% dplyr::select(Code, Date, `Occupancy rate`),                                       by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_critical         %>% dplyr::select(Code, Date, `Critical care beds occupancy rate`),                    by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_closed      %>% dplyr::select(Code, Date, `No. beds closed due to norovirus etc.`),                by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_closed_unoc %>% dplyr::select(Code, Date, `No. unoccupied beds closed due to norovirus etc.`),     by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_7      %>% dplyr::select(Code, Date, `No. beds occupied by long-stay patients (> 7 days)`),   by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_21     %>% dplyr::select(Code, Date, `No. beds occupied by long-stay patients (> 21 days)`),  by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_closures         %>% dplyr::select(Code, Date, Closures),                                               by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_diverts          %>% dplyr::select(Code, Date, Diverts),                                                by = c("Code", "Date"))

  # re-order columns
  sitrep = sitrep %>% dplyr::select(Code, Name, Date, dplyr::everything())

  # remove temp file
  unlink(tmp_sitrep)

  # return sitrep
  sitrep
}
