# Positive tests

## Test for Correct Return Type
test_that("getSpreads returns a data frame by default", {
  result <- getSpreads("XTZUSD")
  expect_s3_class(result, "data.frame")
})

## Test for Correct Return Type (With Timestamp)
test_that("getSpreads returns a list with data frame and
          timestamp when requested", {
  result <- getSpreads("XTZUSD", timestamp = TRUE)
  expect_type(result, "list")
  expect_true("spreads" %in% names(result))
  expect_true("last" %in% names(result))
  expect_s3_class(result$spreads, "data.frame")
  expect_type(result$last, "integer")
})

## Test for Correct Return Types When Timestamp is Included
test_that("getSpreads returns correct timestamp type when requested", {
  result <- getSpreads("XTZUSD", timestamp = TRUE)
  expect_type(result$last, "integer")
})

## Test for Column Names and Data Types
test_that("getSpreads returns data frame with correct columns", {
  result <- getSpreads("XTZUSD")
  expect_true(all(c("Time", "Bid", "Ask") %in% colnames(result)))
  expect_type(result$Time, "double")
  expect_type(result$Bid, "double")
  expect_type(result$Ask, "double")
})

## Test for Data Returned with Correct Values
test_that("getSpreads returns valid Bid and Ask values", {
  result <- getSpreads("XTZUSD")
  expect_false(any(is.na(result$Bid)))
  expect_false(any(is.na(result$Ask)))
})

## Test for since Parameter Behavior
test_that("getSpreads returns data since specified timestamp", {
  # Use a known timestamp for testing
  result <- getSpreads("XTZUSD", since = "2024-10-01 00:00:00")
  expect_true(all(result$Time >= anytime::anytime("2024-10-01 00:00:00")))
})

## Test for Large Dataset
test_that("getSpreads handles large datasets", {
  result <- getSpreads("XTZUSD", timestamp = TRUE)
  expect_gte(nrow(result$spreads), 100)  # Assume large dataset has 100+ rows
})


# Negative tests

## Test for Pair Input Length
test_that("getSpreads throws error for multiple pairs in input", {
  expect_error(getSpreads(c("XTZUSD", "ADAEUR")),
               "Invalid input: 'pair' must be a single character string.")
})


## Test for Invalid Input Handling (Non-Character Pair)
test_that("getSpreads throws error for non-character pair input", {
  expect_error(getSpreads(123),
               "Invalid input: 'pair' must be a single character string.")
})

## Test for Invalid Input Handling (Invalid Timestamp Format)
test_that("getSpreads throws error for invalid 'since' format", {
  expect_error(getSpreads("XTZUSD", since = "invalid_date"),
               "Invalid 'since' format.")
})

## Test for Correct API Error Handling
test_that("getSpreads handles API errors correctly", {
  # Mocking an invalid pair to simulate an API error
  expect_error(getSpreads("INVALID_PAIR"), "API returned the following error")
})
