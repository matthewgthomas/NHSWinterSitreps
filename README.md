# NHSWinterSitreps
The `NHSWinterSitreps` R package provides functions for downloading and cleaning NHS England's [winter situation reports](https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/). You can access data for every winter from 2012-13 to 2019-20.

To install:

```
devtools::install_github("matthewgthomas\NHSWinterSitreps")
```

To use:

```
library(NHSWinterSitreps)
sitrep = load_sitreps("2019-20")  # this can be one of "2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14" or "2012-13"
```

The situation report will contain the following data:

- A&E diverts
- A&E closures
- Ambulance delays (only for 2017-18 onward)
- A&E attendance (only for 2015-16)
- Bed occupancy
- Beds occupied by long-stay patients (> 7 days and > 21 days) (only for 2017-18 onward)
- Beds closed due to diarrhoea, vomiting, norovirus
