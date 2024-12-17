#' Retrieve Ticker Information from Kraken Exchange
#'
#' This function fetches detailed ticker information from the Kraken API
#' for specified trading pairs or all available pairs.
#'
#' @param pairs A character vector specifying the pairs to retrieve ticker
#'                information for. Use "All" to fetch data for all pairs.
#'                For specific pairs, provide their abbreviations
#'                (e.g., "ADAEUR" or c("ADAEUR", "BTCUSD")). Default is "All".
#'
#' @return A data frame containing detailed ticker information for the
#'          requested pairs with renamed and split columns, where applicable,
#'          and all numbers treated as numeric.
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#'
#' @examples
#' getTickers()
#' getTickers("ADAEUR")
#' getTickers(c("ADAEUR", "BTCUSD"))


getTickers <- function(pairs = "All") {

  # Validate pairs input
  if (!is.character(pairs) || length(pairs) < 1) {
    stop(paste("Invalid input: 'pairs' must be a character vector",
               "with at least one element."))
  }

  # Build the base URL
  url <- "https://api.kraken.com/0/public/Ticker"

  # Add the pairs or set to all pairs
  if (pairs[1] != "All") {
    pairsList <- paste(pairs, collapse = ",")
    url <- paste0(url, "?pair=", pairsList)
  }

  # Fetch data from the Kraken API
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

  # Check if the 'result' element exists
  if (is.null(jsonFile[["result"]]) || length(jsonFile[["result"]]) == 0) {
    stop("No ticker data returned from the API.")
  }

  # Extract the result list
  tickerList <- jsonFile[["result"]]

  # Initialize an empty list to store data frames
  result <- list()

  # Process each ticker pair
  for (pair in names(tickerList)) {
    ticker_data <- tryCatch({
      as.data.frame(t(tickerList[[pair]]), stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Failed to process ticker pair:",
                    pair, "with error:", e$message))
      return(NULL)  # Skip to the next pair in case of an error
    })

    # Add the pair name to the data
    if (!is.null(ticker_data)) {
      ticker_data$Pair <- pair
      result[[pair]] <- ticker_data
    }
  }

  # Check if any data was processed
  if (length(result) == 0) {
    stop("No valid ticker data could be processed.")
  }

  # Combine all results into a single data frame
  finalResult <- tryCatch({
    dplyr::bind_rows(result, .id = "PairID")
  }, error = function(e) {
    stop("Error combining the result data frames: ", e$message)
  })

  # Rename the columns to be more descriptive
  colnames(finalResult) <- c("PairID", "Ask", "Bid", "LastTradeClosed",
                             "Volume", "VWAP", "Trades", "Low", "High",
                             "Open", "Pair")

  # Split columns with multiple values and convert
  if ("Ask" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Ask_Price") :=
                                   vapply(!!rlang::sym("Ask"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("Ask_WholeLotVolume") :=
                                   vapply(!!rlang::sym("Ask"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1)),
                                 !!rlang::sym("Ask_LotVolume") :=
                                   vapply(!!rlang::sym("Ask"),
                                          function(x) as.numeric(x[3]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Ask"))
  }

  if ("Bid" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Bid_Price") :=
                                   vapply(!!rlang::sym("Bid"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("Bid_WholeLotVolume") :=
                                   vapply(!!rlang::sym("Bid"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1)),
                                 !!rlang::sym("Bid_LotVolume") :=
                                   vapply(!!rlang::sym("Bid"),
                                          function(x) as.numeric(x[3]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Bid"))
  }

  if ("LastTradeClosed" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("LastTrade_Price") :=
                                   vapply(!!rlang::sym("LastTradeClosed"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("LastTrade_LotVolume") :=
                                   vapply(!!rlang::sym("LastTradeClosed"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("LastTradeClosed"))
  }

  if ("Volume" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Volume_Today") :=
                                   vapply(!!rlang::sym("Volume"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("Volume_24h") :=
                                   vapply(!!rlang::sym("Volume"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Volume"))
  }

  if ("VWAP" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("VWAP_Today") :=
                                   vapply(!!rlang::sym("VWAP"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("VWAP_24h") :=
                                   vapply(!!rlang::sym("VWAP"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("VWAP"))
  }

  if ("Trades" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Trades_Today") :=
                                   vapply(!!rlang::sym("Trades"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("Trades_24h") :=
                                   vapply(!!rlang::sym("Trades"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Trades"))
  }

  if ("Low" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Low_Today") :=
                                   vapply(!!rlang::sym("Low"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("Low_24h") :=
                                   vapply(!!rlang::sym("Low"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Low"))
  }

  if ("High" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("High_Today") :=
                                   vapply(!!rlang::sym("High"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1)),
                                 !!rlang::sym("High_24h") :=
                                   vapply(!!rlang::sym("High"),
                                          function(x) as.numeric(x[2]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("High"))
  }

  if ("Open" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 !!rlang::sym("Open_Price") :=
                                   vapply(!!rlang::sym("Open"),
                                          function(x) as.numeric(x[1]),
                                          numeric(1))
    ) %>% dplyr::select(-!!rlang::sym("Open"))
  }

  # Convert to numeric
  numeric_columns <- c("Ask_Price", "Ask_WholeLotVolume", "Ask_LotVolume",
                       "Bid_Price", "Bid_WholeLotVolume", "Bid_LotVolume",
                       "LastTrade_Price", "LastTrade_LotVolume",
                       "Volume_Today", "Volume_24h", "VWAP_Today", "VWAP_24h",
                       "Trades_Today", "Trades_24h", "Low_Today", "Low_24h",
                       "High_Today", "High_24h", "Open_Price")

  finalResult[numeric_columns] <- lapply(finalResult[numeric_columns],
                                         as.numeric)

  # Return the cleaned data frame
  return(finalResult)
}
