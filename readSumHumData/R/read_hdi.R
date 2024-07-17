#' Read HDI Data
#'
#' This function reads an HDI data file for a specified country, processes the data,
#' and returns an object of class 'hdi_data'. It ensures that columns are displayed nicely
#' and variables have the correct class.
#' @param country_code The name of the country for which to load the data, used to construct the file name.
#' @return A data frame of class 'hdi_data' with properly formatted columns and data types.
#' @export
#' @examples
#' hdi_data <- read_hdi("irl")
read_hdi <- function(country_code) {
  file_name <- sprintf("hdro_indicators_%s.csv", country_code)

  # Import and Remove redundant first row
  data <- data.table::fread(file_name)[-1,]

  # Set the appropriate classes for each column
  data$country_code <- as.character(data$country_code)
  data$country_name <- as.character(data$country_name)

  data$indicator_id <- as.factor(data$indicator_id)
  data$indicator_name <- as.factor(data$indicator_name)
  data$index_id <- as.factor(data$index_id)
  data$index_name <- as.factor(data$index_name)

  data$value <- as.numeric(data$value)
  data$year <- as.integer(data$year)

  # Rename columns to be more descriptive
  names(data) <- c("country_code", "country_name", "indicator_id",
                   "indicator_name", "index_id", "index_name", "value", "year")

  # Set class for methods
  class(data) <- c("hdiData", class(data))
  return(data)
}

