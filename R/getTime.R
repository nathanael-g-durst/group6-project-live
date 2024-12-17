#' Retrieve the Current Kraken Server Time
#'
#' This function fetches the current server time from the Kraken API in
#' either UNIX timestamp or RFC 1123 format.
#'
#' @param format A character string specifying the time format to return.
#'                Use "unix" for UNIX timestamp or "rfc" for RFC 1123 format.
#'                The default is "unix".
#'
#' @return A POSIXct object if `format = "unix"` or
#'          a character string in RFC 1123 format if `format = "rfc"`.
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' getTime("unix")
#' getTime("rfc")



getTime <- function(format = "unix") {

  # Kraken API
  url <- "https://api.kraken.com/0/public/Time"

  # Get data with error handling
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Error check from the API response
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ",
         paste(jsonFile[["error"]], collapse = ", "))
  }

  # Check that the result element exists
  if (is.null(jsonFile[["result"]])) {
    stop("No result data returned from the API.")
  }

  # Format the result based on the specified format
  if (format == "unix") {
    date <- jsonFile[["result"]][["unixtime"]]
    if (is.null(date)) {
      stop("Unix time data is not available in the result.")
    }
    result <- as.POSIXct(date, origin = "1970-01-01", tz = "UTC")

  } else if (format == "rfc") {
    result <- jsonFile[["result"]][["rfc1123"]]
    if (is.null(result)) {
      stop("RFC 1123 time data is not available in the result.")
    }

  } else {
    stop("Invalid format specified. Use 'unix' or 'rfc'.")
  }

  # Return results
  return(result)
}
