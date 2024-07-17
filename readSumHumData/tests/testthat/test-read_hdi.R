library(testthat)
library(readSumHumData)

# Test the read_hdi function
test_that("read_hdi returns correct data structure", {
  # Assert to have the data file named 'hdro_indicators_Ireland.csv' in tests/testthat
  test_path <- system.file("extdata", "hdro_indicators_irl.csv", package = "readSumHumData")

  # Load the data with irl data file
  data <- read_hdi("irl")

  # Check if the data has the correct class
  expect_true(is.data.frame(data))
  expect_true(inherits(data, "hdiData"))

  # Check for correct column names and types
  expected_cols <- c("country_code", "country_name", "indicator_id",
                     "indicator_name", "index_id", "index_name", "value", "year")
  expect_equal(names(data), expected_cols)
  expect_type(data$year, "integer")
  expect_type(data$value, "double")
  expect_type(data$country_code, "character")
  expect_type(data$country_name, "character")
  expect_s3_class(data$indicator_id, "factor")
  expect_s3_class(data$indicator_name, "factor")
  expect_s3_class(data$index_id, "factor")
  expect_s3_class(data$index_name, "factor")
})
