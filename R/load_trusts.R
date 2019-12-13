#' Load list of NHS Trust locations
#'
#' Uses NHS England's data on Trusts and Sites from https://data.england.nhs.uk/dataset/ods-nhs-trusts-and-sites
#'
#' @param url Location of the NHS Trusts and Sites .csv file
#' @export
load_trusts = function(url = "https://nhsenglandfilestore.s3.amazonaws.com/ods/etr.csv") {
  # column names come from the data dictionary published with the data
  # GOR code stands for 'Government Office Region Code Linked Geographically'
  readr::read_csv(url,
                  col_names = c("Organisation code", "Name", "National grouping", "High level health geography",
                                "Address line 1", "Address line 2", "Address line 3", "Address line 4", "Address line 5", "Postcode",
                                "Open date", "Close date", "Null 1", "Null 2", "Null 3", "Null 4", "Null 5",
                                "Contact telephone number", "Null 6", "Null 7", "Null 8", "Amended record indicator",
                                "Null 9", "GOR code", "Null 10", "Null 11", "Null 12"))
}
