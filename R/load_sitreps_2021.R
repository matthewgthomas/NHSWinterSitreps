#' Load timeseries of winter situation reports for 2020-21
#'
#' The returned tibble contains:
#' - General & Acute beds
#' - Adult critical care
#'
#' @param sitrep_url URL of the timeseries file
#'
#' @importFrom magrittr "%>%"
#'
load_sitreps_2021 = function(sitrep_url) {
  ##
  ## load sitrep timeseries
  ##
  # download sitrep file
  tmp_sitrep = tempfile()
  download.file(sitrep_url, tmp_sitrep, mode = "wb")

  # custom function to replicate how `tibble` used to rename duplicate column names
  my_repair_names = function(x) vctrs::vec_as_names_legacy(x, sep = "__")

  # load sitrep data
  sitrep_beds      = readxl::read_excel(tmp_sitrep, sheet = "G&A beds", skip = 14, .name_repair = my_repair_names)             # general and acute beds
  sitrep_critical  = readxl::read_excel(tmp_sitrep, sheet = "Adult critical care", skip = 14, .name_repair = my_repair_names)  # adult critical care
  sitrep_beds_long = readxl::read_excel(tmp_sitrep, sheet = "Beds Occ by long stay patients", skip = 14, .name_repair = my_repair_names)

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
  num_cols = length( names(sitrep_beds)[ grep("Total beds occ'd", names(sitrep_beds)) ] ) - 1  # how many time series columns are there?

  sitrep_dates$Occupancy = c("Occupancy rate",
                             paste("Occupancy rate", seq(1:num_cols), sep="__"))

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


  ######################################################################################################
  ## Bed occupancy
  ##
  sitrep_beds = sitrep_beds %>%
    dplyr::slice(-c(3)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Total beds")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows")

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  sitrep_beds$`Occupancy rate` = as.numeric(sitrep_beds$`Total beds occ'd`) / as.numeric(sitrep_beds$`Total Beds Open`)

  #... now loop over the rest of the columns, calculating occupancy rates
  num_cols = length( names(sitrep_beds)[ grep("Total beds occ'd", names(sitrep_beds)) ] ) - 1  # how many time series columns are there?

  for (i in 1:num_cols) {
    # get current pair of available/occupied columns
    tmp_cols = sitrep_beds[, grep( paste0("TOTAL BEDS.*__", i, "$"), toupper(colnames(sitrep_beds))) ]
    # calculate occupancy rate (needs unlist() otherwise as.numeric() doesn't work)
    sitrep_beds$rate_tmp = as.numeric(unlist(tmp_cols[,2])) /  as.numeric(unlist(tmp_cols[,1]))
    # rename column to include "__i"
    names(sitrep_beds)[ names(sitrep_beds) == "rate_tmp" ] = paste0("Occupancy rate__", i)
  }

  # keep only new occupancy rate columns
  sitrep_beds = sitrep_beds %>%
    dplyr::select(Code, Name, dplyr::starts_with("Occupancy rate"))

  # convert to long format
  sitrep_beds = sitrep_beds %>%
    tidyr::gather(Occupancy, `Occupancy rate`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, Occupancy), by = "Occupancy")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds = sitrep_beds %>%
    dplyr::select(-Occupancy) %>%
    dplyr::mutate(`Occupancy rate` = dplyr::na_if(`Occupancy rate`, "-")) %>%
    dplyr::mutate(`Occupancy rate` = as.numeric(`Occupancy rate`),
                  Date = as.POSIXct(Date))


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
  ## Combine sitreps into one dataframe
  ##
  sitrep_beds = na.omit(sitrep_beds)
  sitrep_critical = na.omit(sitrep_critical)
  sitrep_beds_long_7 = na.omit(sitrep_beds_long_7)
  sitrep_beds_long_21 = na.omit(sitrep_beds_long_21)

  sitrep = sitrep_beds %>%
    dplyr::left_join(sitrep_critical     %>% dplyr::select(Code, Date, `Critical care beds occupancy rate`), by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_7  %>% dplyr::select(Code, Date, `No. beds occupied by long-stay patients (> 7 days)`),  by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_long_21 %>% dplyr::select(Code, Date, `No. beds occupied by long-stay patients (> 21 days)`),  by = c("Code", "Date"))

  # re-order columns
  sitrep = sitrep %>% dplyr::select(Code, Name, Date, dplyr::everything())

  # remove temp file
  unlink(tmp_sitrep)

  # return sitrep
  sitrep
}
