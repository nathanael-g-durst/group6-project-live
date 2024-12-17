######## [START] Custom functions ########

############ [START] Crypto News Feed #############

getNews <- function(filter = NULL, currencies = NULL, regions = NULL, kind = NULL, public = TRUE) {
  # Define valid values for each parameter
  valid_filters <- c("rising", "hot", "bullish", "bearish", "important", "saved", "lol")
  valid_regions <- c("en", "de", "nl", "es", "fr", "it", "pt", "ru", "tr", "ar", "cn", "jp", "ko")
  valid_kind <- c("news", "media")

  # CRYPTO PANIC API
  base_url <- "https://cryptopanic.com"
  api_call <- paste0(base_url, "/api/v1/posts/?auth_token=", Sys.getenv("CRYPTOPANIC_KEY"))

  # Add public parameter
  if (public) {
    api_call <- paste0(api_call, "&public=true")
  }

  # Validate and append the filter parameter
  if (!is.null(filter)) {
    if (filter %in% valid_filters) {
      api_call <- paste0(api_call, "&filter=", filter)
    } else {
      stop("Invalid filter value. Valid options are: ", paste(valid_filters, collapse = ", "))
    }
  }

  # Validate and append the currencies parameter
  if (!is.null(currencies)) {
    currency_list <- unlist(strsplit(currencies, ","))
    if (length(currency_list) <= 50) {
      api_call <- paste0(api_call, "&currencies=", currencies)
    } else {
      stop("You can specify a maximum of 50 currencies.")
    }
  }

  # Validate and append the regions parameter
  if (!is.null(regions)) {
    region_list <- unlist(strsplit(regions, ","))
    if (all(region_list %in% valid_regions)) {
      api_call <- paste0(api_call, "&regions=", regions)
    } else {
      stop("Invalid region value. Valid options are: ", paste(valid_regions, collapse = ", "))
    }
  }

  # Validate and append the kind parameter
  if (!is.null(kind)) {
    if (kind %in% valid_kind) {
      api_call <- paste0(api_call, "&kind=", kind)
    } else {
      stop("Invalid kind value. Valid options are: ", paste(valid_kind, collapse = ", "))
    }
  }

  # Fetch data with error handling
  response <- tryCatch(
    {
      jsonlite::fromJSON(api_call)
    },
    error = function(e) {
      stop("Error fetching data from the CryptoPanic API: ", e$message)
    }
  )

  # Check and process the 'results' portion
  if (!is.null(response$results) && length(response$results) > 0) {
    # Wrangle the 'results' data
    results_df <- response$results %>%
      mutate(
        source_title = source$title,
        source_region = source$region,
        source_domain = source$domain,
        source_type = source$type,
        # Use map functions to handle NULLs and lists in 'currencies'
        currency_code = map_chr(currencies, ~ if (!is.null(.x) && !is.null(.x$code)) paste(.x$code, collapse = ", ") else NA),
        currency_title = map_chr(currencies, ~ if (!is.null(.x) && !is.null(.x$title)) paste(.x$title, collapse = ", ") else NA)
      ) %>%
      select(
        kind,
        source_title,
        source_region,
        source_domain,
        source_type,
        title,
        published_at,
        url,
        id,
        currency_code,
        currency_title
      )
  } else {
    stop("No results found in the API response.")
  }

  return(results_df)
}

############ [END] Crypto News Feed ############

############ [START] API Rate Limit #############

# Track the time of the last API call
last_call_time <- Sys.time() - 2 # Initialize to more than 1 second ago

rate_limited_call <- function(api_call, timeout = 1) {
  # Calculate time since the last call
  time_since_last_call <- Sys.time() - last_call_time

  # If the last call was less than 1 second ago, wait
  if (time_since_last_call < timeout) {
    Sys.sleep(timeout - as.numeric(time_since_last_call, units = "secs"))
    message("Timeout: waiting before next API call.")
  }

  # Proceed with the API call
  tryCatch(
    {
      # Update the timestamp of the last call
      last_call_time <<- Sys.time()

      # Execute the API call
      data <- api_call()
      message("API call successful.")
      return(data)
    },
    error = function(e) {
      message("API call error: ", e$message)
      return(NULL)
    }
  )
}

############ [END] API Rate Limit ############

############ [START] Colour change ############

# Function to determine the color based on old and current prices
get_color <- function(now, old, current_trend) {
  # If there's no previous price, start with green
  if (is.null(old) || is.na(old)) {
    return("green")
  }

  # Determine the color based on the price trend
  if (now < old) {
    return("red") # Continue showing red if the price decreased
  } else if (now > old) {
    return("green") # Show green if the price increased
  } else {
    return(current_trend) # Keep the previous color if the price is unchanged
  }
}

############ [END] Colour change ############

############ [START] Adaptative scale ############

get_scaled_intervals <- function(median_price) {
  # Base intervals in Kraken's style
  base_intervals <- c(0.1, 0.5, 1, 2.5, 5, 10, 25, 50, 100)

  # Calculate the order of magnitude for the median price
  magnitude <- floor(log10(median_price))

  # Adjust base intervals to match the magnitude of the price
  scaling_factor <- 10^(magnitude - 4) # Adjust magnitude relative to a reference point

  # Scale intervals based on magnitude
  scaled_intervals <- base_intervals * scaling_factor

  # Ensure intervals are unique and sorted for the selectInput
  return(sort(unique(scaled_intervals)))
}

############ [END] Adaptative scale ############

############ [START] HTML Table ############

# Function to create an HTML row with a full-width bar effect
create_html_row <- function(price, quantity, cumulative_quantity, max_cumulative, bg_color, text_color) {
  width_percent <- (cumulative_quantity / max_cumulative) * 100

  # Single cell with a div inside to control the width of the background bar
  sprintf(
    "<tr style='height: 40px;'>
      <td colspan='3' style='padding: 0; position: relative;'>
        <!-- Background color div overlay -->
        <div style='background: %s; width: %.2f%%; height: 100%%; position: absolute; top: 0; left: 0; z-index: 1;'></div>

        <!-- Data table with z-index to overlay on top of background -->
        <table style='width: 100%%; position: relative; z-index: 2; border-collapse: collapse;'>
          <tr>
            <td style='width: 33%%; padding: 5px; color: %s; text-align: center; font-size: 14px; font-weight: bold;'>%.4f</td>
            <td style='width: 33%%; padding: 5px; color: %s; text-align: center; font-size: 14px; font-weight: bold;'>%.4f</td>
            <td style='width: 33%%; padding: 5px; color: %s; text-align: center; font-size: 14px; font-weight: bold;'>%.2f</td>
          </tr>
        </table>
      </td>
    </tr>",
    bg_color, width_percent,
    text_color, price,
    text_color, quantity,
    text_color, cumulative_quantity
  )
}

# Generate the full HTML table as a string with alignment tweaks
generate_html_table <- function(order_book_data, bg_color = "darkred", text_color = "white") {
  if ("Bid_Price" %in% colnames(order_book_data)) {
    price_col <- "Bid_Price"
    quantity_col <- "Bid_Volume"
  } else if ("Ask_Price" %in% colnames(order_book_data)) {
    price_col <- "Ask_Price"
    quantity_col <- "Ask_Volume"
  } else {
    return("<p>Invalid data format: missing required bid or ask columns.</p>")
  }

  # Calculate the maximum cumulative quantity for bar width calculation
  max_cumulative <- max(order_book_data[["cumulative_quantity"]], na.rm = TRUE)

  # Generate table rows with mapply
  rows <- paste(
    mapply(create_html_row,
      order_book_data[[price_col]],
      order_book_data[[quantity_col]],
      order_book_data[["cumulative_quantity"]],
      MoreArgs = list(max_cumulative = max_cumulative, bg_color = bg_color, text_color = text_color)
    ),
    collapse = ""
  )

  # Construct and return the full HTML table with headers aligned
  paste0(
    "<table style='width: 100%; border-collapse: collapse;'>",
    "<tr><th style='width: 33%; padding: 10px; background-color: #434C5E; color: white; text-align: left;'>Price</th>
        <th style='width: 33%; padding: 10px; background-color: #434C5E; color: white; text-align: left;'>Quantity</th>
        <th style='width: 33%; padding: 10px; background-color: #434C5E; color: white; text-align: left;'>Cumulative Quantity</th></tr>",
    rows,
    "</table>"
  )
}

############ [END] HTML Table ############

######## [END] Custom functions ########

######## [START] Server Logic ########

server <- function(input, output, session) {
  ############ [START] Loading Screen #############

  delay(12, hide("loading-screen", anim = TRUE, animType = "fade"))

  # # Reactive value to check if the app is ready
  # app_ready <- reactiveVal(FALSE)
  #
  # # Perform initial data loading
  # observe({
  #   # Example API calls or data loading
  #   data_pairs <- rate_limited_call(getPairs)
  #   data_tickers <- rate_limited_call(getTickers)
  #
  #   # Ensure data is loaded before continuing
  #   if (!is.null(data_pairs) && !is.null(data_tickers)) {
  #     # Set loaded data in reactive values
  #     getpairs_data(data_pairs)
  #     gettickers_data(data_tickers)
  #
  #     # Indicate that loading is complete
  #     app_ready(TRUE)
  #   }
  # })
  #
  # # Hide loading screen once the app is ready
  # observe({
  #   if (app_ready()) {
  #     hide("loading-screen", anim = TRUE, animType = "fade")
  #   }
  # })

  ############ [END] Loading Screen #############

  ############ [START] Generic API Calls with Throttling ############

  # getNews - Updates every seconds - Generic
  getnews_data <- reactiveVal()
  observe({
    invalidateLater(86400000, session) # Refresh every 24 hours
    data <- rate_limited_call(getPairs)
    if (!is.null(data)) {
      getpairs_data(data) # Update the reactive value with the new data
      message("getPairs data updated successfully.")
    } else {
      message("getPairs call failed.")
    }
  })

  # getPairs() - Updates every 24h - Generic
  getpairs_data <- reactiveVal()
  observe({
    invalidateLater(86400000, session) # Refresh every 24 hours
    data <- rate_limited_call(getPairs)
    if (!is.null(data)) {
      getpairs_data(data) # Update the reactive value with the new data
      message("getPairs data updated successfully.")
    } else {
      message("getPairs call failed.")
    }
  })

  # getTickers() - Updates every 5s - Generic
  gettickers_data <- reactiveVal()
  observe({
    invalidateLater(3000, session)
    data <- rate_limited_call(getTickers)
    if (!is.null(data)) {
      gettickers_data(data) # Update the reactive value with the new data
      message("getTickers data updated successfully.")
    } else {
      message("getTickers call failed.")
    }
  })

  ############ [END] Generic API Calls with Throttling ############

  ############ [START] Trading Pair Picker / Title ############

  ## Pairs names
  selected_pair <- reactiveVal()

  observe({
    # Extract wsname
    pair_names <- getpairs_data()$wsname

    # Update dropdown choices
    updateSelectInput(session, "pair_select",
      choices = pair_names,
      selected = "ADA/USD"
    )
  })

  # Update titleBox based on last valid selection in selected_pair
  observeEvent(input$pair_select, {
    if (!is.null(input$pair_select) && input$pair_select != "") {
      selected_pair(input$pair_select) # Update only on valid selection
    }
  })

  # Display selected trading pair in titleBox
  output$titleBox <- renderValueBox({
    valueBox(selected_pair(), "TRADING PAIR", color = "light-blue")
  })

  ############ [END] Trading Pair Picker / Title ############

  ############ [START] Vignette Button ############

  observeEvent(input$launch_vignette, {
    showModal(modalDialog(
      title = tags$div(
        "Shiny-App Documentation",
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "modal",
          `aria-label` = "Close",
          tags$span(HTML("&times;"), `aria-hidden` = "true"),
          style = "font-size: 1.5rem; color: #000; border: none; background: transparent; cursor: pointer;"
        )
      ),
      size = "l", # Large modal
      easyClose = TRUE,
      footer = NULL,
      tags$iframe(
        src = "Shiny-App.html", # Direct path to the HTML file in www folder
        style = "width:100%; height:600px; border:none;"
      )
    ))
  })

  ############ [END] Vignette Button ############

  ############ [START] Specific API Calls with Throttling ############

  # getOHLC() - Updates every 10m - Specific
  getohlc_data <- reactiveVal()
  observe({
    invalidateLater(300000, session) # Refresh every 10 minutes
    data <- rate_limited_call(function() getOHLC(last_valid_pair_id(), interval = "5m", since = as.numeric(Sys.time()) - (24 * 60 * 60)))
    if (!is.null(data)) {
      getohlc_data(data) # Update reactive value with the new data
      message("getOHLC data updated successfully.")
    } else {
      message("getOHLC call failed.")
    }
  })

  # getTrades() - Manual update - Specific
  gettrades_data <- reactiveVal()
  # Observe changes in last_valid_pair_id() to fetch trades data automatically
  observeEvent(last_valid_pair_id(), {
    req(last_valid_pair_id()) # Ensure the pair ID is valid
    data <- rate_limited_call(function() getTrades(last_valid_pair_id()))
    if (!is.null(data)) {
      gettrades_data(data) # Update reactive value
      message("Trades data updated successfully on pair change.")
    } else {
      message("Trades call failed on pair change.")
    }
  })
  # Observe the "Refresh Trades" button to manually refresh the trades data
  observeEvent(input$refresh_trades, {
    req(last_valid_pair_id()) # Ensure the pair ID is valid
    data <- rate_limited_call(function() getTrades(last_valid_pair_id()))
    if (!is.null(data)) {
      gettrades_data(data) # Update reactive value
      message("Trades data updated successfully on button click.")
    } else {
      message("Trades call failed on button click.")
    }
  })

  # getOB() - Updates every 3s - Specific
  getob_data <- reactiveVal()
  observe({
    invalidateLater(3000, session)
    data <- rate_limited_call(function() getOB(last_valid_pair_id(), count = 500))
    if (!is.null(data)) {
      getob_data(data) # Update reactive value with the new data
      message("getOB data updated successfully.")
    } else {
      message("getOB call failed.")
    }
  })

  ############ [END] Specific API Calls with Throttling ############

  ######## [START] Select Trading Pair ########

  # Reactive values for selected currency and pair ID
  selected_currency <- reactiveVal("USD")
  selected_crypto <- reactiveVal("ADA")
  last_valid_pair_id <- reactiveVal("ADAUSD")

  # Update selected currency and pair ID based on the dropdown selection
  observeEvent(input$pair_select, {
    if (!is.null(input$pair_select) && input$pair_select != "") {
      # Extract currencies from the selected pair
      currencies <- strsplit(input$pair_select, "/")[[1]]
      selected_currency(if (length(currencies) > 1) currencies[2] else "")
      selected_crypto(if (length(currencies) > 1) currencies[1] else "")

      # Get the PairID based on selected pair without an additional API call
      new_pair_id <- as.character(getpairs_data()$PairID[getpairs_data()$wsname == input$pair_select])
      if (!is.null(new_pair_id) && new_pair_id != "") {
        last_valid_pair_id(new_pair_id)
      }
    }
  })

  ######## [END] Select Trading Pair ########

  ######## [START] getTickers() / Last Price ########

  # Previous values for comparison
  previous_price <- NULL
  previous_color <- NULL

  # Retrieve the relevant ticker data for the selected pair from gettickers_data
  ticker_data <- reactive({
    req(last_valid_pair_id()) # Ensure the pair ID is available
    gettickers_data()[gettickers_data()$PairID == last_valid_pair_id(), ]
  })

  # Display the last price for the selected pair with periodic refresh
  output$lastpriceBox <- renderValueBox({
    # Get the current Ask_Price from ticker_data
    current_price <- if (nrow(ticker_data()) > 0) as.numeric(ticker_data()$Ask_Price) else NA

    # Display the last price with dynamic color
    box <- valueBox(
      paste0(if (!is.na(current_price)) current_price else "N/A", " ", selected_currency()),
      "LAST PRICE",
      icon = icon("coins"),
      color = get_color(now = current_price, old = previous_price, current_trend = previous_color)
    )

    previous_color <<- get_color(now = current_price, old = previous_price, current_trend = previous_color)
    previous_price <<- current_price

    return(box)
  })

  ######## [END] getTickers() / Last Price ########

  ######## [START] getOHLC() / 24H Change ########
  ########
  ############ Some discrepancies can be seen between our result and Kraken's 24h change due to the calculation method
  ########

  # Output the calculated 24-hour change directly tied to getohlc_data()
  output$changeBox <- renderValueBox({
    req(last_valid_pair_id())
    req(getohlc_data())

    # Ensure OHLC data has enough rows to calculate 24-hour change
    if (nrow(getohlc_data()) >= 2) {
      # Use the first row as the price exactly 24 hours ago
      close_24h_ago <- getohlc_data()$Close[1]
      # Most recent close price
      current_close <- tail(getohlc_data()$Close, 1)

      # Calculate percentage change
      change <- if (!is.na(current_close) && !is.na(close_24h_ago) && close_24h_ago != 0) {
        round(((current_close - close_24h_ago) / close_24h_ago) * 100, 2)
      } else {
        NA
      }
    } else {
      change <- NA
    }

    valueBox(
      paste0(if (!is.na(change)) change else "N/A", " %"),
      "24H CHANGE",
      icon = icon("percent"),
      color = if (!is.na(change) && change >= 0) "green" else "red"
    )
  })

  ######## [END] getOHLC() / 24H Change ########

  ######## [START] getOHLC() / 24H Volume ########
  ########
  ############ Some discrepancies can be seen between our result and Kraken's 24h volume due to the calculation method
  ########

  # Reactive value to store the 24-hour change percentage
  selected_volume_24h <- reactiveVal("N/A")

  # Calculate the 24-hour change based on the updated OHLC data
  observe({
    # Ensure OHLC data is available
    if (!is.null(getohlc_data()) && nrow(getohlc_data()) > 0) {
      # Calculate the total 24-hour volume
      volume_24h <- sum(getohlc_data()$Volume, na.rm = TRUE)

      # Update `selected_volume_24h` with the calculated volume
      selected_volume_24h(if (!is.na(volume_24h)) format(volume_24h, big.mark = ",") else "N/A")
    } else {
      message("Insufficient data for 24-hour volume calculation.")
    }
  })

  # Output the calculated 24-hour volume
  output$volumeBox <- renderValueBox({
    valueBox(
      paste(selected_volume_24h(), selected_crypto()),
      "24H VOLUME",
      icon = icon("chart-line"),
      color = "light-blue"
    )
  })

  ######## [END] getOHLC() / 24H Change ########

  ######## [Start] getOB() / Bid spread ########

  # Reactive value to store the current interval selection
  current_interval <- reactiveVal(NULL) # Start as NULL and set later when intervals are populated

  # Observe updates to getob_data and update the interval choices without changing the selection
  observe({
    req(getob_data())

    # Calculate the median price to determine the scale
    median_price <- median(c(getob_data()$Ask_Price, getob_data()$Bid_Price), na.rm = TRUE)

    # Get scaled intervals based on the median price
    interval_choices <- get_scaled_intervals(median_price)

    # Format interval choices to avoid scientific notation
    interval_choices_formatted <- format(interval_choices, scientific = FALSE, trim = TRUE)

    # Check if current_interval is still valid; if not, set it to a default from formatted choices
    if (!current_interval() %in% interval_choices_formatted || is.null(current_interval())) {
      current_interval(interval_choices_formatted[1]) # Reset to the first available choice
    }

    # Update the select input with formatted intervals, keeping the valid current selection visible
    updateSelectInput(session, "interval_size",
      choices = interval_choices_formatted,
      selected = current_interval()
    )
  })

  # Update current_interval when the user selects a different interval
  observeEvent(input$interval_size, {
    current_interval(input$interval_size)
  })

  output$html_table <- renderUI({
    # Ensure getob_data() has data before proceeding
    req(last_valid_pair_id())
    req(getob_data())

    # Filter and prepare bid and ask data separately
    ask_data <- getob_data() %>%
      select(Ask_Price, Ask_Volume) %>% # Select only ask-related columns
      filter(!is.na(Ask_Price)) %>%
      arrange(Ask_Price) %>%
      mutate(cumulative_quantity = cumsum(Ask_Volume)) %>%
      arrange(desc(Ask_Price))

    bid_data <- getob_data() %>%
      select(Bid_Price, Bid_Volume) %>% # Select only bid-related columns
      filter(!is.na(Bid_Price)) %>%
      arrange(desc(Bid_Price)) %>%
      mutate(cumulative_quantity = cumsum(Bid_Volume))

    # Ensure that both ask_data and bid_data are non-empty
    req(nrow(ask_data) > 0, nrow(bid_data) > 0)

    # Get the selected interval size
    interval_size <- as.numeric(input$interval_size)

    # Group ask data by the interval size
    ask_data <- ask_data %>%
      mutate(Price_Group = floor(Ask_Price / interval_size) * interval_size) %>% # Apply grouping
      group_by(Price_Group) %>%
      summarize(
        Ask_Price = first(Price_Group), # Use the grouped price as the label
        Ask_Volume = sum(Ask_Volume, na.rm = TRUE),
        cumulative_quantity = last(cumulative_quantity) # Last cumulative quantity within group
      ) %>%
      arrange(desc(Ask_Price)) %>%
      tail(7) # Show only 7 rows

    # Group bid data by the interval size
    bid_data <- bid_data %>%
      mutate(Price_Group = floor(Bid_Price / interval_size) * interval_size) %>% # Apply grouping
      group_by(Price_Group) %>%
      summarize(
        Bid_Price = first(Price_Group), # Use the grouped price as the label
        Bid_Volume = sum(Bid_Volume, na.rm = TRUE),
        cumulative_quantity = last(cumulative_quantity) # Last cumulative quantity within group
      ) %>%
      arrange(desc(Bid_Price)) %>%
      head(7) # Show only 7 rows

    # Ensure processed ask_data and bid_data are non-empty after grouping
    req(nrow(ask_data) > 0, nrow(bid_data) > 0)

    # Generate the HTML tables for bids and asks
    ask_html <- generate_html_table(ask_data, bg_color = "darkred", text_color = "white")
    bid_html <- generate_html_table(bid_data, bg_color = "darkgreen", text_color = "white")

    # Calculate the lowest Ask Price and highest Bid Price for spread calculation
    lowest_ask_price <- min(ask_data$Ask_Price, na.rm = TRUE)
    highest_bid_price <- max(bid_data$Bid_Price, na.rm = TRUE)

    # Calculate spread and spread percentage
    spread <- lowest_ask_price - highest_bid_price
    spread_percentage <- (spread / lowest_ask_price) * 100

    # Ensure spread is valid (not NaN)
    req(!is.na(spread), !is.nan(spread))

    # Create spread row as HTML
    spread_html <- sprintf(
      "<table style='width: 100%%; border-collapse: collapse;'>
     <tr>
       <td colspan='3' style='padding: 7px; background-color: #000; color: #ffffff; text-align: center;
                              font-size: 16px; font-weight: bold; border-top: 2px solid #444444; border-bottom: 2px solid #444444;'>
         Spread: %.4f (%.2f%%)
       </td>
     </tr>
   </table>",
      spread, spread_percentage
    )

    # Combine both tables into a single HTML output
    HTML(paste(ask_html, spread_html, bid_html, sep = ""))
  })

  ######## [END] getOB() / Bid spread ########

  ############ [START] Apply statistical method on the graph #############

  # Calculate statistical method overlay
  stat_overlay <- reactive({
    req(getplot_data())
    data <- getplot_data()
    method <- input$stat_method

    if (method == "Moving Average") {
      data$MA <- zoo::rollmean(data$Close, k = 10, fill = NA) # Moving Average with window size 10
    } else if (method == "Bollinger Bands") {
      bb <- TTR::BBands(data$Close, n = 20) # Bollinger Bands with default parameters
      data <- cbind(data, bb)
    } else if (method == "Exponential Moving Average") {
      data$EMA <- TTR::EMA(data$Close, n = 10) # Exponential Moving Average with period 10
    }

    return(data)
  })

  ############ [End] Apply statistical method on the graph #############

  ######## [START] Show Alert Modal ########
  user_alerts <- reactiveValues(data = data.frame(
    pair = character(0),
    current_price = numeric(0),
    target_price = numeric(0),
    alert_triggered = logical(0), # Added this to track if alert has been triggered
    stringsAsFactors = FALSE
  ))

  # Show modal dialog when either "Setup an Alert" button is clicked
  observeEvent(input$setup_alert, {
    showAlertModal() # This will trigger the modal for alert setup
  })

  # Show modal dialog when new "Setup an Alert" button (bell icon) is clicked
  observeEvent(input$setup_alert_new, {
    showAlertModal() # This will trigger the modal for alert setup
  })

  # Function to display the alert modal
  showAlertModal <- function() {
    pairs <- getpairs_data()
    pair_choices <- if (!is.null(pairs)) pairs$wsname else c("BTC/USD", "ETH/USD", "LTC/USD") # Default fallback

    showModal(modalDialog(
      title = div(
        "Set-up Alert",
        style = "font-size: 20px; font-weight: bold; text-align: center; color: black; margin-bottom: 10px;"
      ),
      div(
        style = "background-color: #f4f4f9; padding: 20px; border-radius: 8px;",

        # Dropdown for selecting trading pair
        div(
          style = "margin-bottom: 15px;",
          tags$label("Select Pair", style = "font-size: 14px; font-weight: bold; color: black; display: block; margin-bottom: 5px;"),
          selectizeInput("alert_pair", NULL,
            choices = pair_choices,
            selected = NULL,
            options = list(
              placeholder = "Type to search for a pair",
              allowEmptyOption = FALSE
            )
          )
        ),

        # Non-editable field for displaying the current price
        div(
          style = "margin-bottom: 15px;",
          tags$label("Current Price", style = "font-size: 14px; font-weight: bold; color: black; display: block; margin-bottom: 5px;"),
          textInput("current_price", NULL, value = "Select a pair to display price", width = "100%"),
          tags$style(HTML("
          #current_price {
            color: black !important;
            font-size: 14px !important;
            font-weight: bold !important;
            background-color: #e9ecef !important;
            border: 1px solid #ced4da;
          }
        "))
        ),

        # Editable target price field
        div(
          tags$label("Target Price", style = "font-size: 14px; font-weight: bold; color: black; display: block; margin-bottom: 5px;"),
          numericInput("alert_price", NULL,
            value = NULL,
            min = 0,
            step = 0.01,
            width = "100%"
          ),
          tags$style(HTML("
          #alert_price {
            color: black !important;
            font-size: 14px !important;
            font-weight: bold !important;
          }
        "))
        )
      ),
      footer = div(
        style = "display: flex; justify-content: space-between; gap: 10px; padding: 10px 20px;",
        # Cancel button styled with red background
        actionButton("cancel_button", "Cancel",
          style = "background-color: #e74c3c; color: white; border: none; padding: 8px 16px; border-radius: 5px; font-weight: bold; cursor: pointer;"
        ),
        # Confirm Alert button styled with green background
        actionButton("confirm_alert", "Set Alert",
          style = "background-color: #27ae60; color: white; border: none; padding: 8px 16px; border-radius: 5px; font-weight: bold; cursor: pointer;"
        )
      ),
      easyClose = TRUE, # This makes sure the modal can be closed by clicking the 'X'
      size = "m"
    ))
  }

  # Cancel the modal when clicked on the cancel button
  observeEvent(input$cancel_button, {
    removeModal() # This will close the modal
  })

  # Update the current price field and set the default target price when a pair is selected
  observeEvent(input$alert_pair, {
    req(input$alert_pair) # Ensure the pair is selected

    # Get the current price of the selected pair
    ticker_data <- tryCatch(
      {
        getTickers(input$alert_pair) # Fetch the ticker data for the selected pair
      },
      error = function(e) {
        message("Error fetching ticker data: ", e$message)
        NULL
      }
    )

    # Extract the Ask_Price for the current pair
    selected_price <- if (!is.null(ticker_data) && "Ask_Price" %in% names(ticker_data)) {
      as.numeric(ticker_data$Ask_Price[1]) # Use the first row's Ask_Price
    } else {
      NA
    }

    # Update the Current Price field and Target Price field
    if (!is.na(selected_price)) {
      updateTextInput(session, "current_price", value = as.character(selected_price))
      updateNumericInput(session, "alert_price", value = selected_price)
    } else {
      updateTextInput(session, "current_price", value = "Price not available")
    }
  })

  # When the user clicks "Set Alert", save the alert and show a notification when the price reaches the target
  observeEvent(input$confirm_alert, {
    req(input$alert_pair, input$alert_price) # Ensure inputs are not NULL

    # Add the alert to a reactive values data frame
    user_alerts$data <- rbind(user_alerts$data, data.frame(
      pair = input$alert_pair,
      current_price = as.numeric(input$current_price),
      target_price = input$alert_price,
      alert_triggered = FALSE, # Initially set the alert as not triggered
      stringsAsFactors = FALSE
    ))

    showNotification("Alert has been set successfully!", type = "message")
    removeModal()
  })

  # Periodically check if any alert's target price is reached and update the current price for each alert
  observe({
    invalidateLater(5000, session) # Check every 5 seconds

    # Loop through all alerts and fetch the current price for each one
    for (i in 1:nrow(user_alerts$data)) {
      alert <- user_alerts$data[i, ]

      # Simulate fetching the latest price from getTickers
      current_price <- tryCatch(
        {
          ticker_data <- getTickers(alert$pair) # Fetch the ticker data for the selected pair
          as.numeric(ticker_data$Ask_Price[1]) # Use the first row's Ask_Price
        },
        error = function(e) {
          message("Error fetching ticker data: ", e$message)
          NA
        }
      )

      # Update the Current Price in the table if the price has changed
      if (!is.na(current_price)) {
        user_alerts$data$current_price[i] <- current_price
      }

      # Check if the price threshold has been met
      if (!is.na(current_price) && current_price >= alert$target_price && !alert$alert_triggered) {
        # Set alert as triggered
        user_alerts$data$alert_triggered[i] <- TRUE

        # Remove the triggered alert from the table
        user_alerts$data <- user_alerts$data[-i, ]

        # Show the alert modal with a red message and show both prices
        showModal(modalDialog(
          title = "Price Alert Reached!",
          paste(
            "The target price for", alert$pair, "has been reached.",
            "Current price:", current_price,
            "Alert set at:", alert$target_price
          ),
          footer = div(
            style = "display: flex; justify-content: center;",
            actionButton("close_alert", "Close",
              style = "background-color: #e74c3c; color: white; border: none; padding: 8px 16px; border-radius: 5px; font-weight: bold; cursor: pointer;"
            )
          ),
          easyClose = FALSE # Disable closing the modal by clicking outside
        ))
      }
    }
  })

  # When the user clicks "Close" button in alert modal, remove the modal
  observeEvent(input$close_alert, {
    removeModal() # Close the modal
  })

  output$alerts_table <- renderDT({
    # Remove the 'alert_triggered' column from the data
    alerts_data_without_triggered <- user_alerts$data[, !(names(user_alerts$data) %in% "alert_triggered")]

    # Rename columns
    colnames(alerts_data_without_triggered) <- c("Pair", "Current Price", "Target Price")

    datatable(
      alerts_data_without_triggered,
      selection = "single", # Enable single row selection
      options = list(
        pageLength = 5,
        scrollY = "9vh",
        dom = "tp",
        class = "display stripe hover compact"
      ),
      rownames = FALSE
    )
  })

  observeEvent(input$remove_selected_alert, {
    selected_row <- input$alerts_table_rows_selected
    if (length(selected_row) == 1) {
      # Remove the selected row from the user_alerts$data
      user_alerts$data <- user_alerts$data[-selected_row, ]
      showNotification("Alert removed successfully!", type = "message")
    } else {
      showNotification("No alert selected!", type = "warning")
    }
  })
  ######## [END] Show Alert Modal ########

  ######## [Start] getOHLC() / Market Chart ########

  # Reactive value to store the selected interval for the OHLC data
  interval <- reactiveVal("2w")

  # Update the interval based on user interaction
  observeEvent(input$chart_interval, {
    interval(input$chart_interval)
  })

  # getOHLC() - Updates every 10m - Specific
  getplot_data <- reactiveVal()
  observe({
    invalidateLater(60000, session) # Refresh every 1 minute
    data <- rate_limited_call(function() getOHLC(last_valid_pair_id(), interval = as.character(interval())))
    if (!is.null(data)) {
      getplot_data(data) # Update reactive value with the new data
      message("Plot data updated successfully.")
    } else {
      message("Plot call failed.")
    }
  })

  # Market chart rendering
  output$market_chart <- renderPlotly({
    req(last_valid_pair_id())
    req(stat_overlay())

    data <- stat_overlay()
    chart_type <- input$chart_type

    chart <- switch(chart_type,
      "Candlestick" = plot_ly(data,
        x = ~Time, type = "candlestick",
        open = ~Open, high = ~High, low = ~Low, close = ~Close
      ),
      "Line" = plot_ly(data, x = ~Time, y = ~Close, type = "scatter", mode = "lines"),
      "Area" = plot_ly(data, x = ~Time, y = ~Close, type = "scatter", mode = "lines", fill = "tozeroy")
    )

    # Add statistical overlays
    if (input$stat_method == "Moving Average") {
      chart <- chart %>%
        add_trace(y = ~MA, x = ~Time, type = "scatter", mode = "lines", name = "Moving Average")
    } else if (input$stat_method == "Bollinger Bands") {
      chart <- chart %>%
        add_trace(y = ~up, x = ~Time, type = "scatter", mode = "lines", name = "Upper Band") %>%
        add_trace(y = ~dn, x = ~Time, type = "scatter", mode = "lines", name = "Lower Band") %>%
        add_trace(y = ~mavg, x = ~Time, type = "scatter", mode = "lines", name = "Moving Average")
    } else if (input$stat_method == "Exponential Moving Average") {
      chart <- chart %>%
        add_trace(y = ~EMA, x = ~Time, type = "scatter", mode = "lines", name = "EMA")
    }

    chart %>%
      layout(
        xaxis = list(title = "Time"),
        yaxis = list(title = "Price"),
        showlegend = TRUE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = TRUE)
  })

  ######## [END] getOHLC() / Market Chart ########

  ############ [START] getTrades() / Trade Table ########

  # Fetch and filter trades data
  filtered_trades <- reactive({
    req(gettrades_data()) # Ensure gettrades_data() is available

    trades <- gettrades_data() # Fetch the latest trades data

    # Apply filters for Order_Type and Execution_Type
    if (input$order_type != "All") {
      trades <- trades[trades$Order_Type == input$order_type, ]
    }
    if (input$execution_type != "All") {
      trades <- trades[trades$Execution_Type == input$execution_type, ]
    }

    return(trades)
  })

  # Render the trades table
  output$trades_table <- renderDT({
    req(last_valid_pair_id())
    trades <- filtered_trades()

    # Ensure the trades table always has the expected structure
    if (nrow(trades) == 0) {
      return(
        datatable(
          trades,
          options = list(
            pageLength = input$max_trades,
            scrollY = "9vh",
            dom = "tp",
            class = "display stripe hover compact"
          ),
          rownames = FALSE
        )
      )
    }

    # Format the Time column for readability
    trades$Time <- format(
      as.POSIXct(trades$Time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      "%Y-%m-%d %H:%M:%S"
    )

    # Rename columns for better readability
    colnames(trades) <- c("Price", "Volume", "Time", "Order Type", "Execution Type", "Miscellaneous", "Trade ID")

    # Render the table with data
    datatable(
      trades[, c("Trade ID", "Time", "Price", "Volume", "Order Type", "Execution Type")],
      options = list(
        pageLength = input$max_trades,
        scrollY = "9vh",
        dom = "tp",
        class = "display stripe hover compact"
      ),
      rownames = FALSE
    )
  })

  ############ [END] getTrades() / Trade Table ########

  ######## [Start] News / Feed ########

  # Pagination variables
  items_per_page <- 8
  current_page <- reactiveVal(1)

  # Initialize a reactive value to store news articles
  all_news <- reactiveVal(data.frame())

  # Define a function to fetch and update the news articles
  update_news_feed <- function() {
    new_articles <- tryCatch(getNews(), error = function(e) {
      message("Error fetching news: ", e$message)
      return(NULL)
    })

    if (!is.null(new_articles) && "id" %in% colnames(new_articles) && "published_at" %in% colnames(new_articles)) {
      existing_articles <- all_news()

      if ("id" %in% colnames(existing_articles)) {
        new_only <- anti_join(new_articles, existing_articles, by = "id")
      } else {
        new_only <- new_articles
      }

      if (nrow(new_only) > 0) {
        combined_news <- bind_rows(new_only, existing_articles) %>%
          distinct(id, .keep_all = TRUE) %>%
          arrange(desc(as.POSIXct(published_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))

        all_news(combined_news)
        message("News data updated successfully with ", nrow(new_only), " new articles.")
      } else {
        message("No new articles found. News data remains unchanged.")
      }
    }
  }

  # Periodically call the update_news_feed function
  observe({
    invalidateLater(1000, session)
    update_news_feed()

    total_pages <- ceiling(nrow(all_news()) / items_per_page)

    output$news_feed <- renderUI({
      if (current_page() > total_pages) {
        current_page(total_pages)
      }

      news_data <- all_news()
      start <- (current_page() - 1) * items_per_page + 1
      end <- min(start + items_per_page - 1, nrow(news_data))
      paginated_data <- news_data[start:end, ]

      news_cards <- lapply(1:nrow(paginated_data), function(i) {
        news_item <- paginated_data[i, ]
        formatted_date <- format(as.POSIXct(news_item$published_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "%d/%m/%Y %H:%M")
        unique_currencies <- unique(unlist(strsplit(news_item$currency_code, ",\\s*")))
        currency_subtitle <- paste(unique_currencies, collapse = ", ")

        cleaned_title <- if (grepl(" - ", news_item$source_title)) {
          sub(".* - ", "", news_item$source_title)
        } else {
          news_item$source_title
        }

        # Directly use source_url as input$article_url
        source_url <- paste0("https://cryptopanic.com/news/click/", news_item$id, "/")

        div(
          style = "flex: 1 1 300px; max-width: 300px; background-color: #D8DEE9; padding: 20px;
                 border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); margin: 15px; cursor: pointer; display: flex; flex-direction: column; gap: 10px;",
          onclick = sprintf("Shiny.setInputValue('article_url', '%s', {priority: 'event'});", source_url),

          # Display cleaned source text and date
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(cleaned_title, style = "font-weight: bold; font-size: 14px;"),
            div(paste(formatted_date, "UTC"), style = "font-size: 12px; color: #4C566A;")
          ),

          # Centered Title
          div(
            news_item$title,
            style = "font-size: 18px; font-weight: bold; text-align: center; margin-top: 10px; color: #2E3440;"
          ),

          # Subtitle for currency codes
          div(
            currency_subtitle,
            style = "font-size: 14px; color: #4C566A; text-align: center;"
          ),

          # Kind at the bottom
          div(
            news_item$kind,
            style = "font-size: 12px; color: #81A1C1; text-align: center; margin-top: auto;"
          )
        )
      })

      div(
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; padding: 20px;",
        news_cards
      )
    })
  })

  # Pagination controls
  output$pagination_controls <- renderUI({
    div(
      style = "display: flex; justify-content: center; gap: 10px; padding: 20px;",
      actionButton("prev_page", "Previous", icon = icon("arrow-left")),
      span(paste("Page", current_page(), "of", ceiling(nrow(all_news()) / items_per_page)), style = "font-size: 16px; padding-top: 5px;"),
      actionButton("next_page", "Next", icon = icon("arrow-right"))
    )
  })

  # Handle pagination
  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })

  observeEvent(input$next_page, {
    if (current_page() < ceiling(nrow(all_news()) / items_per_page)) {
      current_page(current_page() + 1)
    }
  })

  # Open modal and load article URL in iframe with a fallback link
  observeEvent(input$article_url, {
    showModal(modalDialog(
      title = "Article",
      div(
        # Iframe with sandbox and fallback check
        tags$iframe(
          id = "article_frame",
          src = input$article_url,
          width = "100%",
          height = "600px",
          style = "border: none;",
          sandbox = "allow-scripts", # Prevent external links from opening
          onload = "if (this.contentWindow.length === 0) { document.getElementById('fallback_link').style.display = 'block'; }"
        ),
        # Fallback link if the iframe fails
        div(
          id = "fallback_link",
          style = "display: none; margin-top: 10px;",
          "This article could not be displayed here. ",
          tags$a("Click here to open it in a new tab.", href = input$article_url, target = "_blank")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  ######## [END] News / Feed ########

  ######## [START] Chat Assitent Integration ########
  # Initialize reactive values for chat history
  chat_data <- reactiveVal(data.frame(source = character(0), message = character(0), stringsAsFactors = FALSE))

  observeEvent(input$chat_submit, {
    req(input$chat_input) # Ensure the user input is not empty

    # Add user query to chat history
    new_data <- data.frame(source = "User", message = input$chat_input, stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), new_data))

    # Send the user query to your Flask API
    response <- tryCatch(
      {
        res <- httr::POST(
          url = "https://cryptochatassitent.onrender.com/chat", # Flask API URL
          body = list(query = input$chat_input),
          encode = "json"
        )
        if (httr::status_code(res) == 200) {
          httr::content(res)$response # Extract the response content
        } else {
          "Error: Unable to fetch response from the assistant."
        }
      },
      error = function(e) {
        "Error: Unable to connect to the chat service."
      }
    )

    # Append the response from the assistant to chat history
    gpt_data <- data.frame(source = "Assistant", message = response, stringsAsFactors = FALSE)
    chat_data(rbind(chat_data(), gpt_data))

    # Update the chat display dynamically
    output$chat_response <- renderUI({
      lapply(1:nrow(chat_data()), function(i) {
        div(
          class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
          HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"]))
        )
      }) %>% tagList()
    })

    # Clear the input field after submission
    updateTextInput(session, "chat_input", value = "")
  })

  observeEvent(input$clear_history, {
    # Clear chat history if "Clear History" is pressed
    chat_data(data.frame(source = character(0), message = character(0), stringsAsFactors = FALSE))
  })


  ######## [END] Chat Assitent Integration ########
}

######## [END] Server Logic ######
