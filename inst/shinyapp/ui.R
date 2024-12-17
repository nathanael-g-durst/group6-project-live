######## [START] Packages requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c(
  "dplyr", "knitr", "tidyr", "shiny", "shinydashboard",
  "shinyjs", "fresh", "reactable", "gridExtra", "plotly",
  "ggplot2", "jsonlite", "purrr", "TTR", "zoo", "DT",
  "lubridate", "KrakenR"
)
################################################################################

# For the package
for (packageName in packagesNeeded) {
  if (!requireNamespace(packageName, quietly = TRUE)) {
    stop(paste("The package", packageName, "is required but not installed. Please install it."))
  }
}

library(TTR)
library(zoo)
library(dplyr)
library(knitr)
library(tidyr)
library(purrr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(fresh)
library(reactable)
library(gridExtra)
library(plotly)
library(ggplot2)
library(jsonlite)
library(DT)
library(lubridate)
library(KrakenR)

######## [END] Packages requirements ########

######## [START] Set working directory ########

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######## [END] Set working directory ########

######## [START] Custom Theme ########

kraken <- create_theme(
  adminlte_color(
    light_blue = "#434C5E",
    blue = "#2E3440"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#2E3440",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

######## [END] Custom Theme ########

######## [START] Shiny UI ########

dashboardPage(
  ## Header content
  dashboardHeader(
    title = "KrakenR",

    # Use dropdownMenu for notifications or other elements
    tags$li(
      class = "dropdown",
      actionButton(
        "setup_alert_new",
        label = "Setup an Alert",
        icon = icon("bell"),
        style = "margin-top: 8px; margin-left: 10px; margin-right: 10px;"
      )
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        inputId = "launch_vignette",
        label = NULL,
        icon = icon("circle-question"),
        style = "margin-top: 8px; margin-right: 10px;"
      )
    )
  ),

  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("News", tabName = "news", icon = icon("newspaper")),
      menuItem("Chat Assistant", tabName = "chatgpt", icon = icon("robot"))
    )
  ),

  ## Body content
  dashboardBody(
    use_theme(kraken),
    useShinyjs(),

    # CSS for loading screen
    tags$head(
      tags$style(HTML("
        #loading-screen {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: #2E3440;
          color: #ffffff;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          z-index: 1000;
        }
      "))
    ),

    # Loading screen
    div(id = "loading-screen", "Loading, please wait..."),
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("titleBox", width = 2),
          valueBoxOutput("lastpriceBox", width = 3),
          valueBoxOutput("changeBox", width = 2),
          valueBoxOutput("volumeBox", width = 3),
          box(
            title = NULL, width = 2, solidHeader = FALSE,
            selectizeInput("pair_select", "Choose a Trading Pair",
              choices = NULL,
              options = list(
                placeholder = "Type to search for a pair",
                allowEmptyOption = FALSE
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Order Book", width = 4, solidHeader = TRUE, status = "primary", background = "black",
            selectInput("interval_size", NULL, choices = NULL, selected = NULL, width = "auto"),
            htmlOutput("html_table")
          ),
          box(
            title = "Market Chart", width = 8, solidHeader = TRUE, status = "primary",
            fluidRow(
              column(4, selectInput("chart_interval", "Interval", choices = c("1m", "5m", "15m", "30m", "1h", "4h", "1d", "1w", "2w"), selected = "2w")),
              column(4, selectInput("chart_type", "Chart Type", choices = c("Candlestick", "Line", "Area"), selected = "Candlestick")),
              column(4, selectInput("stat_method", "Statistical Method", choices = c("None", "Moving Average", "Bollinger Bands", "Exponential Moving Average"), selected = "None"))
            ),
            fluidRow(
              column(12, plotlyOutput("market_chart", height = "35vh"))
            )
          ),
          box(
            title = "Last Trades", width = 8, solidHeader = TRUE, status = "primary", style = "height: 25vh; overflow: hidden;",
            fluidRow(
              column(4, selectInput("order_type", "Order Type", choices = c("All", "buy", "sell"), selected = "All")),
              column(4, selectInput("execution_type", "Execution Type", choices = c("All", "market", "limit"), selected = "All")),
              column(2, actionButton("refresh_trades", "Refresh Trades", icon = icon("sync")))
            ),
            DTOutput("trades_table")
          ),
          box(
            title = "Active Alerts",
            width = 8,
            solidHeader = TRUE,
            status = "primary",
            style = "height: 25vh; overflow: hidden;",
            DTOutput("alerts_table"),
            actionButton("remove_selected_alert", "Remove Selected Alert", icon = icon("trash"))
          )
        )
      ),

      # Second tab content
      tabItem(
        tabName = "news",
        uiOutput("pagination_controls"),
        fluidRow(
          column(width = 12, div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px;", uiOutput("news_feed")))
        )
      ),
      # Thirds tab content
      tabItem(
        tabName = "chatgpt",
        fluidRow(
          column(
            12,
            h3("Ask ChatGPT Assistant", style = "color: #ffffff; font-weight: bold;"),

            # Text input for the query
            textInput("chat_input", "Type your question here:", placeholder = "Enter your query..."),

            # Button to submit the query
            actionButton("chat_submit", "Get Response",
              icon = icon("paper-plane"),
              style = "margin-top: 10px; background-color: #81A1C1; color: #2E3440; font-weight: bold;"
            ),

            # Response display area
            div(
              style = "margin-top: 20px; background-color: #434C5E; padding: 10px; border-radius: 8px; color: #ffffff; height: 400px; overflow-y: auto;",
              uiOutput("chat_response")
            )
          )
        )
      )
    )
  )
)
######## [END] Shiny UI ########
