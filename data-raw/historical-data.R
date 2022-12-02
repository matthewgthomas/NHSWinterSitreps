# Download and save all historical data in one go
library(devtools)

load_all(".")

sitrep2122 <- load_sitreps("2021-22")
sitrep2021 <- load_sitreps("2020-21")
sitrep1920 <- load_sitreps("2019-20")
sitrep1819 <- load_sitreps("2018-19")
sitrep1718 <- load_sitreps("2017-18")
sitrep1617 <- load_sitreps("2016-17")
sitrep1516 <- load_sitreps("2015-16")

usethis::use_data(sitrep2122, overwrite = TRUE)
usethis::use_data(sitrep2021, overwrite = TRUE)
usethis::use_data(sitrep1920, overwrite = TRUE)
usethis::use_data(sitrep1819, overwrite = TRUE)
usethis::use_data(sitrep1718, overwrite = TRUE)
usethis::use_data(sitrep1617, overwrite = TRUE)
usethis::use_data(sitrep1516, overwrite = TRUE)
