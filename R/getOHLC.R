#' Retrieve OHLC Data from Kraken Exchange
#'
#' This function fetches OHLC (Open, High, Low, Close) data from the Kraken API
#' for a specified trading pair.
#'
#' @param pair A character string specifying the trading pair (e.g., "ADAEUR").
#'              This is a required parameter.
#' @param interval A time frame interval in minutes or in a
#'                  user-friendly format (e.g., "1h" for 1 hour,
#'                  "1d" for 1 day, etc.). Possible values are "1m", "5m",
#'                  "15m", "30m", "1h", "4h", "1d", "1w", "2w",
#'                  or numeric values in minutes. Default is 1 minute.
#' @param since A character string for a human-readable date-time
#'              (e.g., "2024-10-01 12:00:00") or a Unix timestamp.
#'              Default is NULL (returns all available data).
#'
#' @return A data frame containing OHLC data for the requested trading pair.
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @importFrom anytime anytime
#' @importFrom rlang sym
#'
#' @examples
#' getOHLC("ADAEUR")
#' getOHLC("ADAEUR", interval = "4h")
#' getOHLC("ADAEUR", interval = 1440, since = "2024-01-01 00:00:00")
#' getOHLC("ADAEUR", interval = "2w", since = 1704063600)


getOHLC <- function(pair, interval = 1, since = NULL) {

  # Validate the pair input
  if (!is.character(pair) || length(pair) != 1) {
    stop("Invalid input: 'pair' must be a single character string.")
  }

  # Convert user-friendly intervals into minutes if needed
  if (is.character(interval)) {
    interval_map <- list(
      "1m" = 1, "5m" = 5, "15m" = 15, "30m" = 30, "1h" = 60, "4h" = 240,
      "1d" = 1440, "1w" = 10080, "2w" = 21600
    )
    interval <- interval_map[[interval]]
    if (is.null(interval)) {
      stop(paste("Invalid interval format. Please use '1m', '5m', '15m',",
                 "'30m', '1h', '4h', '1d', '1w', '2w',",
                 "or the corresponding amount of time in minutes",
                 "(e.g., '60' for 1 hour)."))
    }
  } else if (!is.numeric(interval)) {
    stop(paste("Interval must be a numeric value representing",
               "minutes or a valid time format string (e.g., '1h')."))
  }

  # Convert human-readable date-time into Unix timestamp
  if (!is.null(since) && !is.numeric(since)) {
    since <- as.numeric(anytime::anytime(since))
    if (is.na(since)) {
      stop(paste("Invalid 'since' format. Please provide",
                 "a valid date-time string or a Unix timestamp."))
    }
  }

  # Build the URL for the OHLC data request
  url <- paste0("https://api.kraken.com/0/public/OHLC?pair=",
                pair, "&interval=", interval)
  if (!is.null(since)) {
    url <- paste0(url, "&since=", since)
  }

  # Fetch data from the Kraken API
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Check for API errors
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ",
         paste(jsonFile[["error"]], collapse = ", "))
  }

  # Extract the OHLC data
  ohlc_data <- jsonFile[["result"]][[pair]]
  if (is.null(ohlc_data)) {
    stop("No OHLC data returned for the given pair.")
  }

  # Convert OHLC data into a data frame and label columns
  ohlc_df <- as.data.frame(ohlc_data, stringsAsFactors = FALSE)
  colnames(ohlc_df) <- c("Time", "Open", "High", "Low", "Close",
                         "VWAP", "Volume", "Count")

  # Convert numeric columns to numeric format using Standard Evaluation (SE)
  ohlc_df <- dplyr::mutate(ohlc_df,
                           !!rlang::sym("Time") :=
                             anytime::anytime(as.numeric(ohlc_df$Time)),
                           !!rlang::sym("Open") :=
                             as.numeric(ohlc_df$Open),
                           !!rlang::sym("High") :=
                             as.numeric(ohlc_df$High),
                           !!rlang::sym("Low") :=
                             as.numeric(ohlc_df$Low),
                           !!rlang::sym("Close") :=
                             as.numeric(ohlc_df$Close),
                           !!rlang::sym("VWAP") :=
                             as.numeric(ohlc_df$VWAP),
                           !!rlang::sym("Volume") :=
                             as.numeric(ohlc_df$Volume),
                           !!rlang::sym("Count") :=
                             as.numeric(ohlc_df$Count))

  return(ohlc_df)
}
