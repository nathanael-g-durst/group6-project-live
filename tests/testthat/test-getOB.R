# Positive Test Cases

## Test for Default Behavior
test_that("getOB returns data frame with bids and asks", {
  result <- getOB("ADAEUR")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Bid_Price", "Ask_Price") %in% colnames(result)))
})

## Test for Specific Count Parameter
test_that("getOB returns correct number of orders when count is specified", {
  result <- getOB("ADAEUR", count = 100)
  expect_true(nrow(result) == 200)
})

## Test for Correct Columns and Data Types
test_that("getOB returns data frame with correct columns and types", {
  result <- getOB("ADAEUR")
  expect_true(all(c("Bid_Price", "Bid_Volume", "Bid_Timestamp", "Ask_Price",
                    "Ask_Volume", "Ask_Timestamp") %in% colnames(result)))
  expect_type(result$Bid_Price, "double")
  expect_type(result$Ask_Price, "double")
})

## Test for Sorted Bids and Asks
test_that("getOB returns sorted bids and asks", {
  result <- getOB("ADAEUR")
  expect_true(all(diff(result$Bid_Price[result$Order_Type == "Bid"]) >= 0))
  expect_true(all(diff(result$Ask_Price[result$Order_Type == "Ask"]) >= 0))
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getOB throws error for invalid pair input", {
  expect_error(getOB(123),
               "Invalid input: 'pair' must be a single character string.")
})

## Test for Invalid Count Parameter
test_that("getOB throws error for invalid count parameter", {
  expect_error(getOB("ADAEUR", count = 600),
               "Invalid input: 'count' must be a number between 1 and 500.")
})

## Test for API Error Handling
test_that("getOB handles API errors correctly", {
  expect_error(getOB("INVALID_PAIR"), "API returned the following error")
})
