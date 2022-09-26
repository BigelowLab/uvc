library(namtools)
library(namforecast)

NAMPATH <- namtools::nam_path("nwa", "day")
NAMDB   <- namtools::read_database(NAMPATH)



FCASTPATH <- namforecast::nam_path("nwa", "forecast")
FCASTDB <- namforecast::read_database(FCASTPATH)
