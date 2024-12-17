#' Retrieve Order Book Data from Kraken Exchange
#'
#' This function fetches order book data from the Kraken API
#' for a specified trading pair.
#'
#' @param pair A character string specifying the trading pair (e.g., "ADAEUR").
#'              This is a required parameter.
#' @param count An optional integer between 1 and 500 specifying the number
#'              of orders to retrieve. Default is NULL.
#'
#' @return A data frame containing the order book data for
#'         the requested trading pair,
#'         with bid orders appearing first (sorted by price ascending)
#'         followed by ask orders (sorted by price ascending).
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate arrange
#' @importFrom anytime anytime
#' @importFrom rlang :=
#'
#' @examples
#' getOB("ADAEUR")
#' getOB("ADAEUR", count = 100)


getOB <- function(pair, count = NULL) {

  # Validate the pair input
  if (!is.character(pair) || length(pair) != 1) {
    stop("Invalid input: 'pair' must be a single character string.")
  }

  # Validate the count input if provided
  if (!is.null(count)) {
    if (!is.numeric(count) || count < 1 || count > 500) {
      stop("Invalid input: 'count' must be a number between 1 and 500.")
    }
  }

  # Build the URL for the order book data request
  url <- paste0("https://api.kraken.com/0/public/Depth?pair=", pair)
  if (!is.null(count)) {
    url <- paste0(url, "&count=", count)
  }

  # Fetch data from the Kraken API
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Check for API errors
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ", paste(jsonFile[["error"]],
                                                        collapse = ", "))
  }

  # Extract the order book data
  order_book <- jsonFile[["result"]][[pair]]
  if (is.null(order_book)) {
    stop("No order book data returned for the given pair.")
  }

  # Extract asks and bids
  asks <- as.data.frame(order_book$asks, stringsAsFactors = FALSE)
  bids <- as.data.frame(order_book$bids, stringsAsFactors = FALSE)

  # Name the columns for asks and bids
  colnames(asks) <- c("Ask_Price", "Ask_Volume", "Ask_Timestamp")
  colnames(bids) <- c("Bid_Price", "Bid_Volume", "Bid_Timestamp")

  # Convert numeric columns to the proper format using Standard Evaluation (SE)
  asks <- dplyr::mutate(asks,
                        !!dplyr::sym("Ask_Price") :=
                          as.numeric(asks$Ask_Price),
                        !!dplyr::sym("Ask_Volume") :=
                          as.numeric(asks$Ask_Volume),
                        !!dplyr::sym("Ask_Timestamp") :=
                          anytime::anytime(as.numeric(asks$Ask_Timestamp)))

  bids <- dplyr::mutate(bids,
                        !!dplyr::sym("Bid_Price") :=
                          as.numeric(bids$Bid_Price),
                        !!dplyr::sym("Bid_Volume") :=
                          as.numeric(bids$Bid_Volume),
                        !!dplyr::sym("Bid_Timestamp") :=
                          anytime::anytime(as.numeric(bids$Bid_Timestamp)))

  # Sort bids (lowest to highest) and asks (lowest to highest)
  sorted_bids <- dplyr::arrange(bids, !!dplyr::sym("Bid_Price"))
  sorted_asks <- dplyr::arrange(asks, !!dplyr::sym("Ask_Price"))

  # Combine bids and asks into a single data frame
  result <- dplyr::bind_rows(
    dplyr::mutate(sorted_bids, !!dplyr::sym("Order_Type") := "Bid"),
    dplyr::mutate(sorted_asks, !!dplyr::sym("Order_Type") := "Ask")
  )

  return(result)
}
