# Positive Test Cases

## Test for Default Behavior
test_that("getPairs returns data frame with all pairs by default", {
  result <- getPairs()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

## Test for Specific Pair
test_that("getPairs returns data for specified pairs", {
  result <- getPairs(c("ADAEUR", "ADAUSD"))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("ADAEUR", "ADAUSD") %in% result$PairID))
})

## Test for Info Parameter
test_that("getPairs returns data with different info levels", {
  result <- getPairs("ADAEUR", info = "fees")
  expect_s3_class(result, "data.frame")
})

## Test for Country Code
test_that("getPairs handles country_code parameter correctly", {
  result <- getPairs("ADAEUR", country_code = "CH")
  expect_s3_class(result, "data.frame")
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getPairs throws error for invalid pair input", {
  expect_error(getPairs(123),
               paste("Invalid input: 'pairs' must be a character vector",
                                    "with at least one element."))
})

## Test for Invalid Info Parameter
test_that("getPairs throws error for invalid info parameter", {
  expect_error(getPairs("ADAEUR", info = "invalid_info"),
               "Invalid 'info' parameter.")
})

## Test for Invalid Country Code
test_that("getPairs throws error for invalid country code input", {
  expect_error(getPairs("ADAEUR", country_code = 123),
               paste("Invalid input: 'country_code' must be a",
                     "single character string or NULL."))
})

## Test for API Error Handling
test_that("getPairs handles API errors correctly", {
  expect_error(getPairs("INVALID_PAIR"), "API returned the following error")
})
