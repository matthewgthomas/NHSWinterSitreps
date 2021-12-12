#' Load timeseries of winter situation reports from 2012-13
#'
#' Downloads and cleans the "Daily SR – Timeseries 6 November 2012 – 28 February 2013.xls" data from https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-sitrep-data-2012-13/
#'
#' The returned tibble contains:
#' - A&E diverts
#' - A&E closures
#' - Bed occupancy
#' - Beds closed due to diarrhoea, vomiting, norovirus
#'
#' @param sitrep_url URL of the timeseries file
#'
#' @importFrom magrittr "%>%"
load_sitreps_1213 = function(sitrep_url = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Daily-SR-Timeseries-.xls") {
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
  sitrep_closures  = readxl::read_excel(tmp_sitrep, sheet = "A&E closures", skip = 14, .name_repair = my_repair_names)
  sitrep_diverts   = readxl::read_excel(tmp_sitrep, sheet = "A&E diverts", skip = 14, .name_repair = my_repair_names)
  sitrep_critical  = readxl::read_excel(tmp_sitrep, sheet = "Adult critical care", skip = 14, .name_repair = my_repair_names)
  sitrep_beds_noro = readxl::read_excel(tmp_sitrep, sheet = "D&V, Norovirus",  skip = 14, .name_repair = my_repair_names)

  ##
  ## sitrep date range
  ##
  sitrep_dates = readxl::read_excel(tmp_sitrep, sheet = "G&A beds", skip = 12, n_max = 1, col_types = "text") %>%
    janitor::remove_empty("cols")

  # clean up dates
  sitrep_dates = t(sitrep_dates) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Date_txt = V1)

  # some dates are ranges not single days - e.g. 11-13-Dec-15 - choose the most recent day in the range
  sitrep_dates$Date_txt = stringr::str_replace(sitrep_dates$Date_txt, "([0-9]+)-[0-9]+", "\\1")
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "9 Nov 12 to 11 Nov 12", "", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "16 Nov 12 to 18-Nov-2012", "18-Nov-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "23 Nov 12 to 25-Nov-2012", "25-Nov-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "30 Nov 12 to 02 Dec 2012", "02-Dec-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "07 Dec 12 to 09 Dec 2012", "09-Dec-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "14-16-Dec 12", "16-Dec-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "21-26/12/2012", "26-Dec-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "28-30/12/2012", "30-Dec-12", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "31/12/2012 -01 /1/13", "01-Jan-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "04-06/01/2013", "06-Jan-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "11-13/01/2013", "13-Jan-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "18-20 Jan-13", "20-Jan-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "25-27/1/13", "27-Jan-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "1-3/2/13", "3-Feb-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "8-10-Feb-13", "10-Feb-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "15-17-02-13", "17-Feb-13", sitrep_dates$Date_txt)
  sitrep_dates$Date_txt = ifelse(sitrep_dates$Date_txt == "22-24 Feb 13", "24-Feb-13", sitrep_dates$Date_txt)

  # convert to Date format - need to do this row by row using sapply()
  sitrep_dates$Date = as.Date( sapply(1:nrow(sitrep_dates), function(i)
    ifelse( Hmisc::all.is.numeric(sitrep_dates$Date_txt[i]),   # check if this row contains a number or a string
            janitor::excel_numeric_to_date(as.numeric(sitrep_dates$Date_txt[i])),  # if a number, use janitor::excel...() to convert it
            as.Date(sitrep_dates$Date_txt[i], format = "%d-%b-%y") )      # if a string, convert directly to Date
  ), origin = "1970-01-01" )

  sitrep_dates$Date_txt = NULL  # don't need this column anymore

  # add columns for lookups to bed occupancy and long stay patients columns
  num_cols = length( names(sitrep_beds)[ grep("Total beds occ", names(sitrep_beds)) ] ) - 1  # how many time series columns are there?

  sitrep_dates$Occupancy = c("Occupancy rate",
                             paste("Occupancy rate", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_open = c("Total beds avail",
                                  paste("Total beds avail", seq(1:num_cols), sep="__"))

  sitrep_dates$Occupancy_occ = c("Total beds occ'd",
                                 paste("Total beds occ'd", seq(1:num_cols), sep="__"))

  sitrep_dates$BedsClosed = c("Beds closed norovirus",
                              paste("Beds closed norovirus", seq(1:num_cols), sep="__"))

  sitrep_dates$BedsClosedUnoc = c("Beds closed unocc",
                                  paste("Beds closed unocc", seq(1:num_cols), sep="__"))

  # Convert to long format for joining with bed occupancy data in the next section
  sitrep_dates_long = sitrep_dates %>%
    tidyr::pivot_longer(cols = -Date) %>%
    dplyr::select(-name) %>%
    na.omit()


  ######################################################################################################
  ## Bed occupancy
  ##
  sitrep_beds = sitrep_beds %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("Total beds")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(-c(Code, Name), as.integer))

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  # sitrep_beds$`Occupancy rate` = as.numeric(sitrep_beds$`Total beds occ'd`) / as.numeric(sitrep_beds$`Total beds avail`)

  #... now loop over the rest of the columns, calculating occupancy rates
  # num_cols = length( names(sitrep_beds)[ grep("Total beds occ", names(sitrep_beds)) ] ) - 1  # how many time series columns are there?

  # for (i in 1:num_cols) {
  #   # get current pair of available/occupied columns
  #   tmp_cols = sitrep_beds[, grep( paste0("Total beds.*__", i, "$"), colnames(sitrep_beds)) ]
  #   # calculate occupancy rate (needs unlist() otherwise as.numeric() doesn't work)
  #   sitrep_beds$rate_tmp = as.numeric(unlist(tmp_cols[,2])) /  as.numeric(unlist(tmp_cols[,1]))
  #   # rename column to include "__i"
  #   names(sitrep_beds)[ names(sitrep_beds) == "rate_tmp" ] = paste0("Occupancy rate__", i)
  # }

  # keep only new occupancy rate columns
  # sitrep_beds = sitrep_beds %>%
  #   dplyr::select(Code, Name, dplyr::starts_with("Occupancy rate"))

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
      `G&A Beds Open` = `Total beds avail`
    )

  # Calculate occupancy rates
  sitrep_beds = sitrep_beds %>%
    dplyr::mutate(`Occupancy rate` = `G&A beds occ'd` / `G&A Beds Open`)

  # variable conversions
  sitrep_beds = sitrep_beds %>%
    # dplyr::select(-Occupancy) %>%
    dplyr::mutate(Date = as.POSIXct(Date))


  #########################################################################################
  ## Adult critical care bed occupancy rate
  ##
  # tidy up data
  sitrep_critical = sitrep_critical %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, dplyr::starts_with("CC")) %>%   # keep only bed occupancy rates
    janitor::remove_empty("rows")

  # calculate bed occupancy rates for each pair of "Total beds available/occupied" columns
  #... do the first pair manually
  sitrep_critical$`Occupancy rate` = as.numeric(sitrep_critical$`CC Adult Occ`) / as.numeric(sitrep_critical$`CC Adult avail`)

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
                  Date = as.POSIXct(Date))


  ######################################################################################################
  ## A&E diverts
  ##
  sitrep_diverts = sitrep_diverts %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(-`SHA`) %>%
    janitor::remove_empty(c("rows", "cols"))

  # convert to long format
  sitrep_diverts = sitrep_diverts %>%
    tidyr::gather(Date_txt, `Diverts`, -Code, -Name)

  # some dates are ranges not single days - e.g. 11-13-Dec-15 - choose the most recent day in the range
  sitrep_diverts$Date_txt = stringr::str_replace(sitrep_diverts$Date_txt, "([0-9]+)-[0-9]+", "\\1")
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "9 Nov 12 to 11 Nov 12", "", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "16 Nov 12 to 18-Nov-2012", "18-Nov-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "23 Nov 12 to 25-Nov-2012", "25-Nov-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "30 Nov 12 to 02 Dec 2012", "02-Dec-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "07 Dec 12 to 09 Dec 2012", "09-Dec-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "14-16-Dec 12", "16-Dec-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "21-26/12/2012", "26-Dec-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "28-30/12/2012", "30-Dec-12", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "31/12/2012 -01 /1/13", "01-Jan-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "04-06/01/2013", "06-Jan-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "11-13/01/2013", "13-Jan-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "18-20 Jan-13", "20-Jan-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "25-27/1/13", "27-Jan-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "1-3/2/13", "3-Feb-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "8-10-Feb-13", "10-Feb-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "15-17-02-13", "17-Feb-13", sitrep_diverts$Date_txt)
  sitrep_diverts$Date_txt = ifelse(sitrep_diverts$Date_txt == "22-24 Feb 13", "24-Feb-13", sitrep_diverts$Date_txt)

  # convert to Date format - need to do this row by row using sapply()
  sitrep_diverts$Date = as.Date( sapply(1:nrow(sitrep_diverts), function(i)
    ifelse( Hmisc::all.is.numeric(sitrep_diverts$Date_txt[i]),   # check if this row contains a number or a string
            janitor::excel_numeric_to_date(as.numeric(sitrep_diverts$Date_txt[i])),  # if a number, use janitor::excel...() to convert it
            as.Date(sitrep_diverts$Date_txt[i], format = "%d-%b-%y") )      # if a string, convert directly to Date
  ), origin = "1970-01-01" )

  sitrep_diverts$Date_txt = NULL  # don't need this column anymore

  # variable conversions
  sitrep_diverts = sitrep_diverts %>%
    dplyr::mutate(Date = as.POSIXct(Date),
                  Diverts = as.integer(Diverts))


  ######################################################################################################
  ## A&E closures
  ##
  sitrep_closures = sitrep_closures %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(-`SHA`) %>%
    janitor::remove_empty(c("rows", "cols"))

  # convert to long format
  sitrep_closures = sitrep_closures %>%
    tidyr::gather(Date_txt, Closures, -Code, -Name)

  # some dates are ranges not single days - e.g. 11-13-Dec-15 - choose the most recent day in the range
  sitrep_closures$Date_txt = stringr::str_replace(sitrep_closures$Date_txt, "([0-9]+)-[0-9]+", "\\1")
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "9 Nov 12 to 11 Nov 12", "", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "16 Nov 12 to 18-Nov-2012", "18-Nov-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "23 Nov 12 to 25-Nov-2012", "25-Nov-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "30 Nov 12 to 02 Dec 2012", "02-Dec-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "07 Dec 12 to 09 Dec 2012", "09-Dec-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "14-16-Dec 12", "16-Dec-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "21-26/12/2012", "26-Dec-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "28-30/12/2012", "30-Dec-12", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "31/12/2012 -01 /1/13", "01-Jan-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "04-06/01/2013", "06-Jan-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "11-13/01/2013", "13-Jan-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "18-20 Jan-13", "20-Jan-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "25-27/1/13", "27-Jan-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "1-3/2/13", "3-Feb-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "8-10-Feb-13", "10-Feb-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "15-17-02-13", "17-Feb-13", sitrep_closures$Date_txt)
  sitrep_closures$Date_txt = ifelse(sitrep_closures$Date_txt == "22-24 Feb 13", "24-Feb-13", sitrep_closures$Date_txt)

  # convert to Date format - need to do this row by row using sapply()
  sitrep_closures$Date = as.Date( sapply(1:nrow(sitrep_closures), function(i)
    ifelse( Hmisc::all.is.numeric(sitrep_closures$Date_txt[i]),   # check if this row contains a number or a string
            janitor::excel_numeric_to_date(as.numeric(sitrep_closures$Date_txt[i])),  # if a number, use janitor::excel...() to convert it
            as.Date(sitrep_closures$Date_txt[i], format = "%d-%b-%y") )      # if a string, convert directly to Date
  ), origin = "1970-01-01" )

  sitrep_closures$Date_txt = NULL  # don't need this column anymore

  # variable conversions
  sitrep_closures = sitrep_closures %>%
    dplyr::mutate(Date = as.POSIXct(Date),
                  Closures = as.integer(Closures))


  ######################################################################################################
  ## Beds closed due to diarrhoea, vomiting, norovirus
  ##

  ##
  ## beds closed
  ##
  sitrep_beds_closed = sitrep_beds_noro %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, `Beds closed norovirus`, dplyr::starts_with("Beds closed norovirus__")) %>%
    janitor::remove_empty("rows")

  # convert to long format
  sitrep_beds_closed = sitrep_beds_closed %>%
    tidyr::gather(BedsClosed, `No. beds closed due to norovirus etc.`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, BedsClosed), by = "BedsClosed")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_closed = sitrep_beds_closed %>%
    dplyr::select(-BedsClosed) %>%
    dplyr::mutate(Date = as.POSIXct(Date),
                  `No. beds closed due to norovirus etc.` = as.integer(`No. beds closed due to norovirus etc.`))

  ##
  ## beds closed unoccupied
  ##
  sitrep_beds_closed_unoc = sitrep_beds_noro %>%
    dplyr::slice(-c(2)) %>%   # skip blank line
    dplyr::select(Code, Name, `Beds closed unocc`, dplyr::starts_with("Beds closed unocc__")) %>%
    janitor::remove_empty("rows")

  # convert to long format
  sitrep_beds_closed_unoc = sitrep_beds_closed_unoc %>%
    tidyr::gather(BedsClosedUnoc, `No. unoccupied beds closed due to norovirus etc.`, -Code, -Name) %>%
    dplyr::left_join(sitrep_dates %>% dplyr::select(Date, BedsClosedUnoc), by = "BedsClosedUnoc")  # merge in dates that correspond to column names

  # variable conversions
  sitrep_beds_closed_unoc = sitrep_beds_closed_unoc %>%
    dplyr::select(-BedsClosedUnoc) %>%
    dplyr::mutate(Date = as.POSIXct(Date),
                  `No. unoccupied beds closed due to norovirus etc.` = as.integer(`No. unoccupied beds closed due to norovirus etc.`))


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
    sitrep_beds_closed %>% dplyr::select(Code, Name, Date)
  ) %>%
    dplyr::distinct() %>%
    na.omit()

  sitrep_beds = na.omit(sitrep_beds)
  sitrep_closures = na.omit(sitrep_closures)
  sitrep_critical = na.omit(sitrep_critical)
  sitrep_diverts = na.omit(sitrep_diverts)
  sitrep_beds_closed = na.omit(sitrep_beds_closed)
  sitrep_beds_closed_unoc = na.omit(sitrep_beds_closed_unoc)

  sitrep = sitrep_trusts %>%
    dplyr::left_join(sitrep_diverts          %>% dplyr::select(Code, Date, Diverts),                                      by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_closures         %>% dplyr::select(Code, Date, Closures),                                           by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds             %>% dplyr::select(Code, Date, `Occupancy rate`),                                   by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_critical         %>% dplyr::select(Code, Date, `Critial beds occupancy rate` = `Occupancy rate`),   by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_closed      %>% dplyr::select(Code, Date, `No. beds closed due to norovirus etc.`),            by = c("Code", "Date")) %>%
    dplyr::left_join(sitrep_beds_closed_unoc %>% dplyr::select(Code, Date, `No. unoccupied beds closed due to norovirus etc.`), by = c("Code", "Date"))

  # re-order columns
  sitrep = sitrep %>% dplyr::select(Code, Name, Date, dplyr::everything())

  # remove temp file
  unlink(tmp_sitrep)

  # return sitrep
  sitrep
}
