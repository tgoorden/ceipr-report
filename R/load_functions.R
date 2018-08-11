#
# All sorts of helper functions for loading and displaying ceipr data.
#

# Note: not 100% we need/use all of these...
library(tidyverse)
library(stringr)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)
library(maptools)

# Name of zip file
zip_file <- function(year,pollutant) {
  str_c(data_directory,year,'/',pollutant,'_2018_GRID_',year,'.zip')
}

# Name of the file inside the zip file
data_file <- function(year,pollutant,sector) {
  str_c(pollutant,'_',sector,'_',str_replace_all(sectors[sector]," ", ""),'_2018_GRID_',year,'.txt')
}

# combines the zip and internal file name
file_names <- function(year,pollutant,sector) {
  s <- c(zip_file(pollutant,year),data_file(pollutant,sector,year))
  names(s) <- c("zip_file","data_file")
  return(s)
}

# read internal file from a zip file
read_file_data <- function(zip,filename) {
  return(read_delim(
        unz(zip,filename),
        comment=csv_comment,
        col_names=csv_column_names,
        col_types = csv_col_types,
        delim = csv_delim,
        locale=csv_locale
      ))
  closeAllConnections() # explicitely closed to avoid warnings due to time-out
}

# Load data for specified pollutant, for a specific sector and year
# If parameters are missing, the whole series will be loaded as a default
read_pollution_data <- function(year=years,pollutant=pollutants,sector=names(sectors)) {
    bind_rows(map(year,function(y) {
      bind_rows(map(pollutant, function(p) {
        bind_rows(map(sector,function(s) {
          read_file_data(
            zip_file(y,p),
            data_file(y,p,s)
          )
        }))
      }))
    }))
}



# DISPLAY FUNCTIONS:

# Show the trends for a specific polutant and sector.
#
# Example country_filter:
# country_filter <- function(tbl) filter(tbl,iso2=="BE"|iso2=="NL"|iso2=="DE"|iso2=="GB")




# Show trends for all sectors.
pollution_trend_all_sectors <- function(pollutant,country_filter) {
  names(sectors) %>%
    map(function(sector) pollution_trend(sector,pollutant,country_filter))
}





