library("eurostat")

# Load the data set for EU population data
# Warning: only available from 2007!
EU_population_data <- get_eurostat("tps00001")
# convert the timestamp to a year, for better referencing
EU_population_data$year <- with(EU_population_data,strtoi(format(time,'%Y')))
EU_population_data <- dplyr::rename(EU_population_data,population=values,iso2=geo)

EU_population_data %>%
  count(iso2,year) %>%
  filter(n > 1)

# attach population data to a ceipr dataframe
add_population <- function(ceipr) {
  return(
    left_join(ceipr,EU_population_data, by = c("iso2","year"))
  )
}

add_emissions_per_person <- function(ceipr) {
  ceipr$emission_per_person <- with(ceipr,emission/population)
}
