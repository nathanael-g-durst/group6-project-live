#' Retrieve Recent Spreads Data from Kraken Exchange
#'
#' This function fetches recent spread data from the Kraken API for
#' a specified trading pair.
#'
#' @param pair A character string specifying the trading pair (e.g., "XTZUSD",
#'              "ADAEUR"). This is a required parameter.
#' @param since A character string for a human-readable date-time
#'              (e.g., "2024-10-01 12:00:00") or a Unix timestamp.
#'              Default is NULL.
#' @param timestamp A logical value. If TRUE, the function returns both the
#'                  spreads data frame and the last timestamp for future
#'                  polling. If FALSE (default), it returns only the data frame.
#'
#' @return A data frame containing the spread data or a list containing
#'          the data frame and the last timestamp.
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom anytime anytime
#' @importFrom rlang sym
#'
#' @examples
#' getSpreads("XTZUSD")
#' getSpreads("ADAEUR", since = "2024-10-01 00:00:00", timestamp = TRUE)


getSpreads <- function(pair, since = NULL, timestamp = FALSE) {

  # Validate the pair input
  if (!is.character(pair) || length(pair) != 1) {
    stop("Invalid input: 'pair' must be a single character string.")
  }

  # Convert human-readable date-time into Unix timestamp if 'since' is provided
  if (!is.null(since) && !is.numeric(since)) {
    since <- as.numeric(anytime::anytime(since))
    if (is.na(since)) {
      stop(paste("Invalid 'since' format. Please provide",
                 "a valid date-time string or a Unix timestamp."))
    }
  }

  # Build the URL for the spreads data request
  url <- paste0("https://api.kraken.com/0/public/Spread?pair=", pair)
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

  # Extract the spreads data
  spreads_data <- jsonFile[["result"]][[pair]]
  if (is.null(spreads_data)) {
    stop("No spread data returned for the given pair.")
  }

  # Extract the 'last' timestamp for future polling
  last_timestamp <- jsonFile[["result"]][["last"]]

  # Convert spreads_data directly to a data frame and rename columns
  spreads_df <- as.data.frame(spreads_data, stringsAsFactors = FALSE)
  colnames(spreads_df) <- c("Time", "Bid", "Ask")

  # Convert the Time, Bid, and Ask columns using Standard Evaluation
  spreads_df <- dplyr::mutate(spreads_df,
                              !!rlang::sym("Time") :=
                                anytime::anytime(
                                  as.numeric(!!rlang::sym("Time"))),
                              !!rlang::sym("Bid") :=
                                as.numeric(!!rlang::sym("Bid")),
                              !!rlang::sym("Ask") :=
                                as.numeric(!!rlang::sym("Ask")))

  # Return based on the 'timestamp' flag
  if (timestamp) {
    return(list(spreads = spreads_df, last = last_timestamp))
  } else {
    return(spreads_df)
  }
}
