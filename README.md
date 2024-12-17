<!-- badges: start -->
  [![R-CMD-check](https://github.com/nathanael-g-durst/test-group6-project/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nathanael-g-durst/test-group6-project/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# KrakenR: An R Package for Kraken API Integration

<center>

![Kraken Digital Asset Exchange](https://i0.wp.com/blog.kraken.com/wp-content/uploads/2024/09/Blog_Header_1535x700.png?w=1535&ssl=1)

</center>

## Overview

`KrakenR` is an R package that provides a seamless interface for retrieving and interacting with cryptocurrency data from Kraken, a leading cryptocurrency exchange. This package is tailored to offer essential tools for fetching asset prices, trading volumes, and other relevant data, making it easy to conduct analyses within `R`.

`KrakenR` interacts with Kraken's API, which is well-documented [here](https://docs.kraken.com/api/). Users can explore this link to better understand how the package communicates with Kraken's endpoints.

One of the key features of `KrakenR` is the inclusion of a `Shiny App` that can be invoked directly from the package. This interactive application allows users to visualize cryptocurrency data and trends using an intuitive user interface.

### Key Features:

 * Fetch market data such as asset prices, volume, and historical trends from Kraken.
 * Real-time cryptocurrency price tracking.
 * Data wrangling functions to process and clean raw Kraken data.
 * `Shiny app` integration to provide visual analytics for cryptocurrency data.
 
### Repository Structure

The repository contains additional folders that are not part of the `KrakenR` package itself but provide important context for specific features and project components:

#### 1. `render-backend`
This folder contains the backend code hosted on [Render](https://render.com), which powers the **AI model** used in the Chat Assistant feature of the Shiny App. The backend enables the app to dynamically interact with the model, providing real-time responses to user queries.

#### 2. `presentation`
This folder holds the presentation delivered on **December 19, 2024**, for the **Programming Tools for Data Science (PTDS)** class. It provides an overview of the package development, functionalities, and the integrated Shiny application.

#### 3. No `data-raw` or `data` Folder
`KrakenR` does not include a `data` or `data-raw` folder. This is because:

- **The package and app are fully dynamic**: All data is fetched in real-time directly from the **Kraken API**.
- There is **no hardcoded data** or pre-downloaded datasets. This ensures the information retrieved is always up-to-date and directly reflects the current state of the Kraken exchange.

This design choice ensures that users interact with live cryptocurrency market data, making `KrakenR` a reliable tool for dynamic analysis and visualization.

## Installation
 
There are two ways to install the `KrakenR` package:
 
### Option 1: Direct Installation from GitHub (Recommended)
 
The package can be installed directly from this repository using `devtools`. This version ensures all project requirements are met, including the ability to launch the Shiny app from the package.
 
```R
# Generate GitHub token
usethis::create_github_token()

# Install KrakenR (replace XXXXXXXXXXX with the token generated)
devtools::install_github(
    repo = "ptds2024/group6-project",
    build_vignettes = TRUE,
    auth_token = "XXXXXXXXXXX",
    dependencies = TRUE
)

```

#### Dependencies of the KrakenR Package

The **KrakenR** package relies on a robust set of dependencies to provide a seamless interface for accessing the Kraken cryptocurrency exchange REST API. To ensure all required dependencies are properly installed, it is essential to use the argument `dependencies = TRUE` when installing the package via `devtools::install_github`. This guarantees that not only the core dependencies listed under `Imports` are installed but also those under `Suggests`, which provide additional functionalities.

The complete list of dependencies is as follows:

- **Depends**:  
  - `R (>= 3.5.0)`

- **Imports** (required for core functionality):  
  - `jsonlite`, `dplyr`, `tidyr`, `anytime`, `magrittr`, `rlang`, `shiny`

- **Suggests** (optional but recommended for vignettes, testing, and enhanced features):  
  - `knitr`, `rmarkdown`, `testthat (>= 3.0.0)`, `shinydashboard`, `shinyjs`, `fresh`,  
    `reactable`, `gridExtra`, `plotly`, `ggplot2`, `purrr`, `DT`, `lubridate`, `glue`

By setting `dependencies = TRUE`, users ensure a smooth setup of the **KrakenR** package, with all the necessary components in place for advanced cryptocurrency market data retrieval, analysis, and visualization.

### Option 2: Installation from CRAN (Not Recommended)
 
Alternatively, the package can be installed from CRAN, but this version does not include the Shiny app integration. To avoid missing features, we recommend using the GitHub installation method.
 
```R
install.packages("KrakenR")

```
> **Note**: The CRAN version is a stable release, but it does not fully align with the requirements of the current project, particularly regarding the integration of the Shiny app.

## Usage

Once you have installed the `KrakenR` package, you can immediately begin exploring its features for interacting with Kraken's cryptocurrency data. The package offers a comprehensive set of functions for fetching market data, such as asset pairs, ticker information, and more. Additionally, you can visualize real-time data using the integrated Shiny app.

### Step 1: Load the Package

To get started, load the `KrakenR` package into your `R` session:

```R
# Load the package
library(KrakenR)

```

### Step 2: Explore the Vignette and Documentation

For a comprehensive overview of how the package works, explore the vignette, which provides detailed examples and explanations for each function. You can also access the full documentation for specific function references.

```R
# Access the KrakenR vignette for a guided walkthrough
vignette("KrakenR", package = "KrakenR")

# Access the package documentation and search for specific functions
??KrakenR

```

### Step 3: ???? (Start using the Package)

Now that you’ve installed and loaded the `KrakenR` package, it's time to start fetching real data from Kraken! The package offers several functions to retrieve asset pairs, ticker information, and more.

```R
# Fetch all available asset pairs from the Kraken exchange
asset_pairs <- getPairs()

# Fetch ticker information for a specific asset pair, e.g., Bitcoin to USD
ticker_info <- getTickers("XBTUSD")

# Fetch (OHLC) data for a specific asset pair, e.g., Cardano to CHF
getOHLC("ADACHF", interval = "1h", since = "2024-01-01 00:00:00")

```

### Step 4: PROFIT!!!

> **Reference**: "Step 3: ??, Step 4: Profit" is a reference to the popular meme from [Know Your Meme](https://knowyourmeme.com/memes/profit).

## Shiny Application

The `Shiny App` included in `KrakenR` is a user-friendly tool that allows users to visualize live market data fetched from the Kraken exchange. With the app, users can track price trends, view trading volumes, and analyze market movements. The app is integrated directly into the package and can be launched with a single function call.

### Features of the Shiny App:

 * Real-time cryptocurrency data updates.
 * User-friendly interface with charts and visualizations.
 * Ability to filter data based on specific assets, dates, and metrics.

### How to Launch the App:

Once the `KrakenR` package is installed, you can launch the app using the following function:

```R
# Launch the shiny app
launchShinyApp()

```

For more information, refer to the vignette using:

```R
# Access the Shiny-App vignette for a information on the App
vignette("Shiny-App", package = "KrakenR")

```

Alternatively, you can access the app's documentation directly from within the interface by clicking the (?) button located at the top-right corner of the application.
