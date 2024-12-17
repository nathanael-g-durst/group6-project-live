# Positive Test Cases

## Test for Default Behavior
test_that("getStatus returns both status and timestamp by default", {
  result <- getStatus()
  expect_type(result, "character")
  expect_named(result, c("status", "timestamp"))
})

## Test for Status Only
test_that("getStatus returns only the status when specified", {
  result <- getStatus("status")
  expect_type(result, "character")
  expect_false("timestamp" %in% names(result))
})

## Test for Timestamp Only
test_that("getStatus returns only the timestamp when specified", {
  result <- getStatus("timestamp")
  expect_type(result, "character")
  expect_false("status" %in% names(result))
})

# Negative Test Cases

## Test for Invalid Data Parameter
test_that("getStatus throws error for invalid 'data' parameter", {
  expect_error(getStatus("invalid_data"), "Invalid value for 'data' parameter")
})
