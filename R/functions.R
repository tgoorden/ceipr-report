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
  read_delim(
    unz(zip,filename),
    comment=csv_comment,
    col_names=csv_column_names,
    col_types = csv_col_types,
    delim = csv_delim,
    locale=csv_locale
  )
}

# load data for specified pollutant, for a specific sector and year
read_pollution_data <- function(year,pollutant,sector) {
  read_file_data(
    zip_file(year,pollutant),
    data_file(year,pollutant,sector)
  )
}

# reads data for all years (2000-2016) for a specific pollutant and sector
read_all_pollution_data <- function(pollutant,sector) {
  years %>%
    map(function(year) read_pollution_data(year,pollutant,sector))
}

# DISPLAY FUNCTIONS:

# Show the trends for a specific polutant and sector.
#
# Example country_filter:
# country_filter <- function(tbl) filter(tbl,iso2=="BE"|iso2=="NL"|iso2=="DE"|iso2=="GB")
pollution_trend <- function(sector,pollutant,country_filter=function(tbl) return(tbl)) {
  pollution_totals <- read_all_pollution_data(pollutant,sector) %>%
    bind_rows %>%
    country_filter %>%
    group_by(iso2,year) %>%
    summarise(total = sum(emission))
  ggplot(pollution_totals) +
    geom_line(mapping= aes(x=year,y=total,color=iso2)) +
    labs(
      title=paste(pollutant,sector,sectors[sector],sep = " - "),
      x="Jaartal",
      y="Uitstoot (ton)",
      color="Land")
}

# Show trends for all sectors.
pollution_trend_all_sectors <- function(pollutant,country_filter) {
  names(sectors) %>%
    map(function(sector) pollution_trend(sector,pollutant,country_filter))
}


# converts a set of data to a raster
convert_to_raster <- function(df) {
  sp::coordinates(df) <- ~longitude+latitude
  # create the grid as defined here:
  # http://webdab1.umweltbundesamt.at/download/01GridData/\
  # EMEP_gridding_system_documentation.pdf

  # first create an arbitrary WGS84 grid
  r <- raster::raster(ncols = 1200, nrows = 520)

  # reassign the extent
  raster::extent(r) <- c(-30,90,30,82)

  # fill with the corresponding values
  # from the spatial data frame (rasterize)
  r <- raster::rasterize(df, r, field = "emission")
  return(r)
}

# Creates a layered geotiff with
save_to_geotiff <- function(pollutant,sector) {
  rasters <- years %>%
    map(function(year) read_pollution_data(year,pollutant,sector)) %>%
    map(function(df) filter(df,iso2=="BE")) %>%
    map(convert_to_raster)
  names(raster) <- paste(pollutant,sector,years, sep = "_")
  raster::writeRaster(raster::stack(rasters),str_c(pollutant,'_',sector,'.tif'),overwrite = TRUE)
}
