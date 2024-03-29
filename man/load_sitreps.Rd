% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_sitreps.R
\name{load_sitreps}
\alias{load_sitreps}
\title{Load timeseries of winter situation reports from the specified winter}
\usage{
load_sitreps(winter)
}
\arguments{
\item{winter}{The winter you want to fetch data for (can be "2022-23", "2021-22 ,"2020-21", "2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13")}
}
\description{
Load timeseries of winter situation reports from the specified winter
}
\examples{
sitrep_1819 = load_sitreps("2018-19")
}
