#' Retrieve the Current System Status of the Kraken Exchange
#'
#' This function fetches the current system status from the Kraken API,
#' including both the operational status and the timestamp.
#'
#' @param data A character string specifying the type of data to return.
#'              Use "status" for the system status, "timestamp" for
#'              the timestamp, or "both" to retrieve both status and timestamp.
#'              The default is "both".
#'
#' @return A character string if `data = "status"` or `data = "timestamp"`.
#'          A named vector if `data = "both"`.
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' getStatus("both")
#' getStatus("status")
#' getStatus("timestamp")


getStatus <- function(data = "both") {

  # Kraken API
  url <- "https://api.kraken.com/0/public/SystemStatus"

  # Fetch data
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Check for errors in the API response
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ",
         paste(jsonFile[["error"]], collapse = ", "))
  }

  # Check that the result exists
  if (is.null(jsonFile[["result"]]) || length(jsonFile[["result"]]) == 0) {
    stop("No system status data returned from the API.")
  }

  # Extract and format the result based on the argument
  if (data == "status") {
    result <- jsonFile[["result"]][["status"]]
    if (is.null(result)) {
      stop("Status data is not available in the result.")
    }

  } else if (data == "timestamp") {
    result <- jsonFile[["result"]][["timestamp"]]
    if (is.null(result)) {
      stop("Timestamp data is not available in the result.")
    }

  } else if (data == "both") {
    status <- jsonFile[["result"]][["status"]]
    timestamp <- jsonFile[["result"]][["timestamp"]]

    if (is.null(status) || is.null(timestamp)) {
      stop("Both status and timestamp data are not available in the result.")
    }

    result <- c(status = status, timestamp = timestamp)

  } else {
    stop("Invalid value for 'data' parameter. Use 'status',
         'timestamp', or 'both'.")
  }

  # Return the results
  return(result)
}
