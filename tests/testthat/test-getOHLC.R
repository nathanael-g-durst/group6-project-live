# Positive Test Cases

## Test for Default Behavior
test_that("getOHLC returns data frame with OHLC data for a valid pair", {
  result <- getOHLC("ADAEUR")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Time", "Open", "High", "Low", "Close", "VWAP", "Volume",
                    "Count") %in% colnames(result)))
})

## Test for Specific Interval
test_that("getOHLC returns correct data for specified interval", {
  result <- getOHLC("ADAEUR", interval = "1h")
  expect_true(all(c("Time", "Open", "High", "Low", "Close")
                  %in% colnames(result)))
})

## Test for Since Parameter
test_that("getOHLC returns data since a specified timestamp", {
  result <- getOHLC("ADAEUR", interval = "1h", since = "2024-10-01 00:00:00")
  expect_true(all(result$Time >= anytime::anytime("2024-10-01 00:00:00")))
})

## Test for User-Friendly Interval Input
test_that("getOHLC handles user-friendly intervals like '1h' and '1d'", {
  result_1h <- getOHLC("ADAEUR", interval = "1h")
  result_1d <- getOHLC("ADAEUR", interval = "1d")
  expect_s3_class(result_1h, "data.frame")
  expect_s3_class(result_1d, "data.frame")
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getOHLC throws error for invalid pair input", {
  expect_error(getOHLC(123),
               "Invalid input: 'pair' must be a single character string.")
})

## Test for Invalid Interval Input
test_that("getOHLC throws error for invalid interval format", {
  expect_error(getOHLC("ADAEUR", interval = "invalid_interval"),
               "Invalid interval format")
})

## Test for Invalid Since Parameter
test_that("getOHLC throws error for invalid 'since' format", {
  expect_error(getOHLC("ADAEUR", since = "invalid_date"),
               "Invalid 'since' format.")
})

## Test for API Error Handling
test_that("getOHLC handles API errors correctly", {
  expect_error(getOHLC("INVALID_PAIR"), "API returned the following error")
})
