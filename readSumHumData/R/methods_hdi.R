#' Print Method for HDI Data
#'
#' Prints the HDI data in a user-friendly format.
#' @param x An 'hdiData' object
#' @export
print_hdiData <- function(x) {
  cat("HDI Data Object\n")
  cat("Country: ", unique(x$country_name), "\n")
  cat("Data Points: ", nrow(x), "\n")
  cat("Columns: ", toString(names(x)), "\n")
  cat("\nFirst few rows of the dataset:\n")
  head(x)

}

#' Summary Method for HDI Data
#'
#' Provides a statistical summary of the HDI data.
#' @param x An 'hdiData' object
#' @return A summary of the HDI data values.
#' @export
summary_hdiData <- function(x) {
  # Summary for numeric data
  cat("Summary for HDI Data Object:\n")

  cat("\nValue Summary:\n")
  print(summary(x$value))

  # Counts for categorical data
  cat("\nCountry Distribution:\n")
  print(as.data.frame(table(x$country_name)))
  cat("\nIndicator Distribution:\n")
  print(as.data.frame(table(x$indicator_name)))
  cat("\nIndex Distribution:\n")
  print(as.data.frame(table(x$index_name)))

  # Include year range
  cat("\nYear Range: ", range(x$year)[1], " to ", range(x$year)[2], "\n")
}

#' Plot Method for HDI Data
#'
#' Generates 4 plots for value, country, index and year range.
#' @param x An 'hdiData object
#' @export
plot_hdiData <- function(x) {

  # Setting up the plotting area
  par(mfrow = c(2, 2))

  # Plot
  hist(x$value, main = "Distribution of Values", xlab = "Values", col = "blue")
  barplot(table(x$country_name), main = "Distribution of Country Names", xlab = "Country", ylab = "Frequency", col = "red")
  barplot(table(x$index_name), main = "Distribution of Index Names", xlab = "Index", ylab = "Frequency", col = "green")
  plot(table(x$year), type = "o", main = "Trend Over Years", xlab = "Year", ylab = "Frequency", col = "orange")

  # Resetting the plotting area
  par(mfrow = c(1, 1))

}
