# Positive Test Cases

## Test for Default Behavior (All Pairs)
test_that("getTickers returns data frame with all pairs by default", {
  result <- getTickers()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

## Test for Specific Pairs
test_that("getTickers returns data for specified pairs", {
  result <- getTickers(c("ADAEUR", "ADAUSD"))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("ADAEUR", "ADAUSD") %in% result$PairID))
})

## Test for Correct Columns
test_that("getTickers returns data frame with correct columns", {
  result <- getTickers("ADAEUR")
  expect_true(all(c("Ask_Price", "Bid_Price", "LastTrade_Price",
                    "Volume_Today", "VWAP_Today", "Trades_Today")
                  %in% colnames(result)))
})

## Test for Numeric Data
test_that("getTickers returns numeric values for price, volume,
          and trades columns", {
  result <- getTickers("ADAEUR")
  expect_type(result$Ask_Price, "double")
  expect_type(result$Bid_Price, "double")
  expect_type(result$LastTrade_Price, "double")
  expect_type(result$Volume_Today, "double")
  expect_type(result$VWAP_Today, "double")
  expect_type(result$Trades_Today, "double")
})

## Test for missing fields
test_that("getTickers handles missing columns in the API response", {
  result <- getTickers("ADAEUR")
  expect_false("missing_column" %in% colnames(result))
})

# Test for large datasets
test_that("getTickers handles large datasets efficiently", {
  result <- getTickers(c("ADAEUR", "BTCUSD", "ETHUSD", "LTCUSD",
                         "DOTUSD", "XRPUSD", "ADAUSD"))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getTickers throws error for invalid pair input", {
  expect_error(getTickers(123),
               paste("Invalid input: 'pairs' must be a character vector",
                     "with at least one element."))
})

## Test for API Error Handling
test_that("getTickers handles API errors correctly", {
  expect_error(getTickers("INVALID_PAIR"), "API returned the following error")
})
