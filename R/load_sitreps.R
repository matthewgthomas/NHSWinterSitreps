#' Load timeseries of winter situation reports from the specified winter
#' @param winter The winter you want to fetch data for (can be "2019-20", "2018-19", "2017-18", "2016-17")
#' @example sitrep_1819 = load_sitreps("2018-19")
#' @export
load_sitreps = function(winter) {

  if (missing(winter)) stop("You must specify a winter")
  if (!winter %in% c("2019-20", "2018-19", "2017-18", "2016-17")) stop("`winter` must be one of '2019-20', '2018-19', '2017-18', '2016-17'")

  sitrep = tibble::tibble()

  if (winter == "2019-20") {
    # "Winter SitRep: Acute Time series 2 December 2019 – 8 December 2019" data from https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-daily-sitrep-2018-19-data/
    sitrep = load_sitreps_generic("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/12/Winter-data-timeseries.xlsx")

  } else if (winter == "2018-19") {
    # "Winter SitRep: Acute Time series 3 December 2018 to 3 March 2019" data from https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-daily-sitrep-2018-19-data/
    sitrep = load_sitreps_generic("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/03/Winter-data-timeseries-20190307.xlsx")

  } else if (winter == "2017-18") {
    # "Winter Sitrep: Acute Time series 20 November 2017 to 4 March 2018" data from https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-daily-sitrep-2017-18-data/
    sitrep = load_sitreps_generic("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/03/Winter-data-Timeseries-20180304.xlsx",
                                  closures_sheet_name = "A&E closures", diverts_sheet_name = "A&E diverts")  # this winter's file has closures and diverts with lowercase 'c' and 'd'

  } else if (winter == "2016-17") {
    # "Winter SitRep Part A: Acute Time Series 1 December 2016 to 12 March 2017" data from https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-daily-sitrep-2016-17-data/
    sitrep = load_sitreps_1617()
  }

  sitrep
}
