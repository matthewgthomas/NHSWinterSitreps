# Download and save the current winter's data
library(devtools)

load_all(".")

sitrep2223 <- load_sitreps("2022-23")
usethis::use_data(sitrep2223, overwrite = TRUE)
