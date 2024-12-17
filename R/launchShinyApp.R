#' Launch the KrakenR Shiny Dashboard for Crypto Assets
#'
#' This function launches a Shiny dashboard, \strong{KrakenR}, that allows users
#' to explore crypto asset data directly from their R environment. The dashboard
#' includes various views and visualizations of cryptocurrency data fetched
#' through the Kraken API.
#'
#' @return Launches a Shiny application in the default browser.
#'         No return value.
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#' # Launch the KrakenR Shiny dashboard
#' if (interactive()) {
#'   launchShinyApp()
#' }


launchShinyApp <- function() {
  appDir <- system.file("shinyapp", package = "KrakenR")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
