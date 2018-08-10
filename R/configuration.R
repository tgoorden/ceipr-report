#
# Configuration for ceipr data. Normally you don't need to touch this.
#
library(readr)

data_directory <- 'webdab1.umweltbundesamt.at/download/gridding2018/'
years <- c(2000:2016)
sectors <- c("Public Power","Industry","Other Stationary Comb","Fugitive","Solvents","Road Transport","Shipping","Aviation","Offroad","Waste")
names(sectors) <- c("A","B","C","D","E","F","G","H","I","J")
csv_column_names <- c("iso2", "year", "sector",
                      "pollutant", "longitude",
                      "latitude", "unit", "emission")
csv_col_types <- cols(
  iso2 = col_character(),
  year = col_integer(),
  sector = col_character(),
  pollutant = col_character(),
  longitude = col_number(),
  latitude = col_number(),
  unit = col_character(),
  emission = col_double()
)
csv_delim <- ';'
csv_comment <- '#'
csv_locale <- locale(decimal_mark = '.',grouping_mark = ',')
