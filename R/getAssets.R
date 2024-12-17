#' Retrieve Asset Information from Kraken Exchange
#'
#' This function fetches detailed asset information from the Kraken API,
#' either for all available assets or a specified subset.
#'
#' @param assets A character vector specifying the assets to retrieve.
#'                Use "All" to fetch data for all assets.
#'                For specific assets, provide their abbreviations
#'                (e.g., "ADA" or c("BTC", "ETH")). Default is "All".
#'
#' @return A data frame containing detailed information
#'          about the requested assets.
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
#'
#' @examples
#' getAssets()
#' getAssets("ETH")
#' getAssets(c("BTC", "AAVE", "ADA"))



getAssets <- function(assets = "All") {

  # Variables definition
  result <- list()

  # Check assets parameter
  if (!is.character(assets) || length(assets) < 1) {
    stop(paste("Invalid input: 'assets' must be a character vector",
               "with at least one element."))
  }

  # Determine if fetching all assets or specific ones
  if (assets[1] == "All") {
    # Kraken API
    url <- "https://api.kraken.com/0/public/Assets"
  } else {
    # Edit input for query
    assetsList <- paste(assets, collapse = ",")
    url <- paste0("https://api.kraken.com/0/public/Assets?asset=", assetsList)
  }

  # Fetch data
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

  # Check if the 'result' element exists
  if (is.null(jsonFile[["result"]]) || length(jsonFile[["result"]]) == 0) {
    stop("No asset data returned from the API.")
  }

  # Extract the result list
  assetsList <- jsonFile[["result"]]

  # Process each element in the list
  for (asset in names(assetsList)) {
    asset_data <- tryCatch({
      as.data.frame(t(assetsList[[asset]]), stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Failed to process asset:", asset, "with error:",
                    e$message))
      return(NULL)  # Skip to the next asset in case of an error
    })

    # Add the asset name to the data
    if (!is.null(asset_data)) {
      asset_data$Asset <- asset
      result[[asset]] <- asset_data
    }
  }

  # Check if any data was processed
  if (length(result) == 0) {
    stop("No valid asset data could be processed.")
  }

  # Combine all results into a single data frame
  finalResult <- tryCatch({
    dplyr::bind_rows(result, .id = "AssetID")
  }, error = function(e) {
    stop("Error combining the result data frames: ", e$message)
  })

  # Attempt to unnest columns if needed
  finalResult <- tryCatch({
    tidyr::unnest(finalResult, cols = c("aclass", "altname", "decimals",
                                        "display_decimals", "collateral_value",
                                        "status"))
  }, error = function(e) {
    stop("Error unnesting the final result: ", e$message)
  })

  # Return the clean data frame
  return(finalResult)
}
