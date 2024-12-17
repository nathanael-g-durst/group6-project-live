# Positive Test Cases

## Test for Default Behavior (All Assets)
test_that("getAssets returns data frame with all assets by default", {
  result <- getAssets()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

## Test for Specific Assets
test_that("getAssets returns data frame with specified assets", {
  result <- getAssets(c("BTC", "ETH"))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("BTC", "ETH") %in% result$Asset))
})

## Test for Correct Columns and Data Types
test_that("getAssets returns data frame with correct columns", {
  result <- getAssets("BTC")
  expect_true(all(c("AssetID", "Asset", "aclass", "altname",
                    "decimals", "status") %in% colnames(result)))
  expect_type(result$Asset, "character")
  expect_type(result$decimals, "integer")
})

# Negative Test Cases

## Test for Invalid Asset Input
test_that("getAssets throws error for invalid asset input", {
  expect_error(getAssets(123),
               paste("Invalid input: 'assets' must be a character vector",
                     "with at least one element."))
})

## Test for API Error Handling
test_that("getAssets handles API errors correctly", {
  # Mocking an invalid request to trigger an API error
  expect_error(getAssets("INVALID_ASSET"), "API returned the following error")
})

## Test for Empty Asset Vector
test_that("getAssets throws error for empty asset vector", {
  expect_error(getAssets(c()),
               paste("Invalid input: 'assets' must be a character vector with",
                                    "at least one element."))
})
