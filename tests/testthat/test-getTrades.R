# Positive Test Cases

## Test for Default Behavior
test_that("getTrades returns data frame for a valid pair", {
  result <- getTrades("XTZUSD")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Price", "Volume", "Time", "Order_Type", "Execution_Type",
                    "Miscellaneous", "Trade_ID") %in% colnames(result)))
})

## Test for Specific 'since' Parameter
test_that("getTrades returns data for trades since a specified timestamp", {
  result <- getTrades("XTZUSD", since = "2024-10-01 00:00:00")
  expect_true(all(result$Time >= anytime::anytime("2024-10-01 00:00:00")))
})

## Test for Specific 'count' Parameter
test_that("getTrades returns correct number
          of trades when count is specified", {
  result <- getTrades("XTZUSD", count = 10)
  expect_true(nrow(result) <= 10)
})

## Test for Correct Numeric Columns
test_that("getTrades returns numeric values for price and volume", {
  result <- getTrades("XTZUSD")
  expect_type(result$Price, "double")
  expect_type(result$Volume, "double")
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getTrades throws error for invalid pair input", {
  expect_error(getTrades(123),
               "Invalid input: 'pair' must be a single character string.")
})

## Test for Invalid 'since' Parameter
test_that("getTrades throws error for invalid 'since' format", {
  expect_error(getTrades("XTZUSD", since = "invalid_date"),
               "Invalid 'since' format.")
})

## Test for Invalid 'count' Parameter
test_that("getTrades throws error for invalid 'count' parameter", {
  expect_error(getTrades("XTZUSD", count = 2000),
               "Invalid input: 'count' must be a number between 1 and 1000.")
})

## Test for API Error Handling
test_that("getTrades handles API errors correctly", {
  expect_error(getTrades("INVALID_PAIR"), "API returned the following error")
})
