library(ggplot2)

# Use this to view trends in emission data
view_trends <- function(ceipr_data,group="iso2",group_label="country",subject="",title=paste(subject,"trends per",group_label,sep = " ")) {
  pollution_totals <- dplyr::group_by(ceipr_data,.dots=group,year) %>%
    dplyr::summarise(total = sum(emission))
  ggplot(pollution_totals) +
    geom_line(mapping= aes_string(x="year",y="total",color=group)) +
    labs(
      title=title,
      x="Year",
      y="Emissions (ton)",
      color=group_label)
}

# Use this to compare emissions (typically for a specific year)
view_comparisons <- function(ceipr_data,group="iso2",group_label="country",subject="",title=paste(subject,"comparison per",group_label,sep = " ")) {
  pollution_totals <- dplyr::group_by(ceipr_data,.dots=group,year) %>%
    dplyr::summarise(total = sum(emission))
  ggplot(pollution_totals) +
    geom_bar(mapping= aes_string(x="year",y="total",fill=group),stat="identity",position="dodge") +
    labs(
      title=title,
      x="Year",
      y="Emissions (ton)",
      color=group_label)
}

# converts a set of data to a raster
convert_to_raster <- function(df) {
  sp::coordinates(df) <- ~longitude+latitude
  # create the grid as defined here:
  # http://webdab1.umweltbundesamt.at/download/01GridData/EMEP_gridding_system_documentation.pdf

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
save_to_geotiff <- function(ceipr_data,pollutant,title="",filename="example") {
  rasters <- years %>%
    map(function(y) filter(ceipr_data,year==y)) %>%
    map(convert_to_raster)
  names(raster) <- paste(title,years, sep = "_")
  raster::writeRaster(raster::stack(rasters),str_c(filename,'.tif'),overwrite = TRUE)
}

