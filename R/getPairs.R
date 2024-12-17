#' Retrieve Tradable Asset Pairs Information from Kraken Exchange
#'
#' This function fetches detailed information about tradable asset pairs from
#' the Kraken API, either for all pairs or a specified subset.
#'
#' @param pairs A character vector specifying the pairs to retrieve. Use "All"
#'                to fetch data for all pairs. For specific pairs, provide
#'                their abbreviations (e.g., "ADAEUR" or c("ADAEUR", "BTCUSD")).
#'                Default is "All".
#' @param info A character vector to specify the level of detail
#'              ("info", "leverage", "fees", "margin"). Default is "info".
#' @param country_code A single character string specifying the country code to
#'                      filter the pairs. Use NULL if not needed.
#'                      Default is NULL.
#'
#' @return A data frame containing detailed information about the requested
#'          asset pairs with nested lists for columns with multiple values.
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
#'
#' @examples
#' getPairs()
#' getPairs("ADAEUR")
#' getPairs(c("ADAEUR", "BTCUSD"), info = "fees")
#' getPairs("ADAEUR", country_code = "CH")
#' getPairs(country_code = "US:TX")


getPairs <- function(pairs = "All", info = "info", country_code = NULL) {

  # Validate pairs input
  if (!is.character(pairs) || length(pairs) < 1) {
    stop(paste("Invalid input: 'pairs' must be a character vector",
         "with at least one element."))
  }

  # Validate info parameter
  valid_info_options <- c("info", "leverage", "fees", "margin")
  if (!(info %in% valid_info_options)) {
    stop("Invalid 'info' parameter. Must be one of: ",
         paste(valid_info_options, collapse = ", "))
  }

  # Validate country_code parameter
  if (!is.null(country_code)) {
    if (!is.character(country_code) || length(country_code) != 1) {
      stop(paste("Invalid input: 'country_code' must be a",
                 "single character string or NULL."))
    }
  }

  # Build the base URL
  url <- "https://api.kraken.com/0/public/AssetPairs?"

  # Add the pairs or set to all
  if (pairs[1] == "All") {
    url <- paste0(url, "info=", info)
  } else {
    pairsList <- paste(pairs, collapse = ",")
    url <- paste0(url, "pair=", pairsList, "&info=", info)
  }

  # Append country_code parameter if provided
  if (!is.null(country_code)) {
    url <- paste0(url, "&country=", country_code)
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
    stop("No asset pair data returned from the API.")
  }

  # Extract the result list
  pairsList <- jsonFile[["result"]]

  # Initialize an empty list to store data frames
  result <- list()

  # Process each pair
  for (pair in names(pairsList)) {
    pair_data <- tryCatch({
      as.data.frame(t(pairsList[[pair]]), stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Failed to process pair:", pair, "with error:", e$message))
      return(NULL)  # Skip to the next pair in case of an error
    })

    # Add the pair name to the data
    if (!is.null(pair_data)) {
      pair_data$Pair <- pair
      result[[pair]] <- pair_data
    }
  }

  # Check if any data was processed
  if (length(result) == 0) {
    stop("No valid asset pair data could be processed.")
  }

  # Combine all results into a single data frame
  finalResult <- tryCatch({
    dplyr::bind_rows(result, .id = "PairID")
  }, error = function(e) {
    stop("Error combining the result data frames: ", e$message)
  })

  # Dynamically process columns
  for (col in colnames(finalResult)) {
    if (is.list(finalResult[[col]])) {
      # Check the lengths of elements in the column
      element_lengths <- lengths(finalResult[[col]])

      if (all(element_lengths == 1)) {
        # If all elements have length 1, unlist them
        finalResult[[col]] <- unlist(finalResult[[col]])
      } else {
        # Keep columns with multiple values as nested lists
        finalResult[[col]] <- lapply(finalResult[[col]],
                                     function(x) if (is.null(x)) NA else x)
      }
    }
  }

  # Return the cleaned data frame with nested lists for multi-value columns
  return(finalResult)
}
