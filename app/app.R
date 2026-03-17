devtools::install()
devtools::load_all()

library(shiny)
library(CryptoIndicator)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(zoo)

# ======================================
# LOAD BUILT-IN DEMO DATA
# ======================================
# This file should be placed in the app's directory
# Generate it once with: saveRDS(result, "demo_data.rds")

default_demo <- NULL
if (file.exists("demo_data.rds")) {
  default_demo <- readRDS("demo_data.rds")
  default_demo_date <- format(default_demo$metadata$timestamp, "%B %Y")
} else {
  default_demo_date <- "Not available"
  warning("demo_data.rds not found. Please generate it first.")
}

# Define UI
ui <- fluidPage(

  theme = shinytheme("darkly"),

  # JavaScript for button control
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('jsCode', function(message) {
        eval(message.code);
      });
    ")),
    tags$style(HTML("
      .well {
        background-color: #2c3e50;
        color: white;
      }
      .risk-GREEN { background-color: #28a745; color: white; padding: 10px; border-radius: 5px; }
      .risk-YELLOW { background-color: #ffc107; color: black; padding: 10px; border-radius: 5px; }
      .risk-RED { background-color: #dc3545; color: white; padding: 10px; border-radius: 5px; }
      .api-section {
        background-color: #34495e;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .demo-badge {
        background-color: #9b59b6;
        color: white;
        padding: 5px 15px;
        border-radius: 20px;
        font-size: 14px;
        font-weight: bold;
        margin-left: 10px;
      }
      .mode-selector {
        background-color: #2c3e50;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 20px;
        border: 2px solid #3498db;
      }
      .mode-demo {
        border-left: 5px solid #9b59b6;
      }
      .mode-live {
        border-left: 5px solid #27ae60;
      }
      .auto-load-badge {
        background-color: #27ae60;
        color: white;
        padding: 3px 10px;
        border-radius: 15px;
        font-size: 12px;
        margin-left: 10px;
      }
      .key-input {
        margin-bottom: 10px;
      }
    "))
  ),

  # App title with REACTIVE badge
  titlePanel(
    div("CryptoIndicator Dashboard",
        uiOutput("mode_badge")
    )
  ),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel
    sidebarPanel(
      width = 3,

      # Mode Selector
      div(class = "mode-selector",
          h4("App Mode", style = "color: #3498db; text-align: center;"),
          radioButtons("app_mode",
                       "Select Mode:",
                       choices = c(
                         "Demo Mode (No API keys)" = "demo",
                         "Live Mode (With API keys)" = "live"
                       ),
                       selected = "demo",
                       width = "100%")
      ),

      # ======================================
      # DEMO MODE UI
      # ======================================
      conditionalPanel(
        condition = "input.app_mode == 'demo'",
        div(class = "api-section mode-demo",
            h4("Demo Options", style = "color: #9b59b6;"),

            # Auto-loaded demo info
            div(style = "background-color: #27ae60; padding: 10px; border-radius: 8px; margin-bottom: 15px;",
                h5("✅ Auto-Loaded Demo", style = "color: white;"),
                p(strong(default_demo_date), style = "color: white;"),
                p(style = "color: #ecf0f1; font-size: 12px; margin-top: 5px;",
                  "Default demo loads automatically when app starts")
            ),

            # Option to upload custom file
            div(style = "background-color: #34495e; padding: 10px; border-radius: 8px;",
                h5("📤 Upload Custom Demo", style = "color: #3498db;"),
                fileInput("custom_demo",
                          "Choose .rds file",
                          accept = c(".rds"),
                          placeholder = "No file selected"),
                p(style = "font-size: 12px; color: #bdc3c7;",
                  "Upload your own .rds file to view different dates")
            )
        )
      ),

      # ======================================
      # LIVE MODE UI
      # ======================================
      conditionalPanel(
        condition = "input.app_mode == 'live'",
        div(class = "api-section mode-live",
            h4("Live Analysis", style = "color: #27ae60;"),

            # Asset selection
            selectInput("asset",
                        "Select Asset:",
                        choices = c("Bitcoin"),
                        selected = "Bitcoin"),

            # Date range
            dateRangeInput("dates",
                           "Date Range:",
                           start = "2020-01-01",
                           end = Sys.Date()),

            br(),

            # API Keys Section - Manual Input
            h5("🔑 API Keys (Optional)", style = "color: #3498db;"),

            # FRED API Key manual input
            textInput("fred_api_key",
                      "FRED API Key:",
                      value = "",
                      placeholder = "Enter your FRED API key"),

            tags$small(class = "text-muted",
                       "API keys are optional. Leave blank to run without them."),

            br(), br(),

            # Action button
            actionButton("run_live", "Run Live Analysis",
                         class = "btn-danger",
                         style = "width: 100%; font-weight: bold;")
        )
      ),

      br(),

      # Common elements for both modes
      conditionalPanel(
        condition = "input.app_mode == 'demo' || input.app_mode == 'live'",

        # Date range (for filtering)
        dateRangeInput("filter_dates",
                       "Filter Date Range:",
                       start = "2020-01-01",
                       end = Sys.Date()),

        br(),

        # Status
        textOutput("status"),
        br(),

        # Current data info
        div(class = "api-section",
            h5("📊 Current Data:", style = "color: #3498db;"),
            textOutput("data_source"),
            textOutput("data_date"),
            textOutput("current_price")
        )
      )
    ),

    # Main panel
    mainPanel(
      width = 9,

      # Risk level display
      fluidRow(
        column(12,
               h3("Current Risk Level"),
               uiOutput("risk_box")
        )
      ),

      # Key metrics
      fluidRow(
        column(4, h4("Short-Term"), uiOutput("short_term")),
        column(4, h4("Medium-Term"), uiOutput("medium_term")),
        column(4, h4("Long-Term"), uiOutput("long_term"))
      ),

      # Tabs for different views
      tabsetPanel(
        tabPanel("Summary",
                 br(),
                 verbatimTextOutput("summary")),

        tabPanel("On-Chain",
                 br(),
                 h4("MVRV Ratio"),
                 plotOutput("mvrv_plot", height = "250px"),
                 br(),
                 h4("Puell Multiple"),
                 plotOutput("puell_plot", height = "250px"),
                 br(),
                 h4("NVT Ratio"),
                 plotOutput("nvt_plot", height = "250px"),
                 br(),
                 tableOutput("onchain_table")),

        tabPanel("Volatility",
                 br(),
                 h4("Volatility Regimes"),
                 plotOutput("volatility_regime_plot", height = "200px"),
                 br(),
                 h4("Short-term vs Medium-term Volatility"),
                 plotOutput("volatility_compare_plot", height = "300px"),
                 br(),
                 h4("Volatility Breakout Signals"),
                 plotOutput("volatility_signals_plot", height = "200px"),
                 br(),
                 h4("Current Volatility Metrics"),
                 tableOutput("volatility_table")),

        tabPanel("Derivatives",
                 br(),
                 h4("Funding Rates"),
                 plotOutput("funding_plot", height = "250px"),
                 br(),
                 h4("Open Interest & Funding Percentiles"),
                 plotOutput("percentile_plot", height = "250px"),
                 br(),
                 h4("Crowded Trades Signal"),
                 plotOutput("crowded_plot", height = "150px"),
                 br(),
                 tableOutput("derivatives_table")),

        tabPanel("Macro",
                 br(),
                 h4("M2 Money Supply Growth"),
                 plotOutput("m2_plot", height = "250px"),
                 br(),
                 h4("US Dollar Index (FRED)"),
                 plotOutput("dxy_plot", height = "250px"),
                 br(),
                 h4("Composite Risk Score"),
                 plotOutput("composite_plot", height = "150px"),
                 br(),
                 tableOutput("macro_table")),

        tabPanel("Positioning",
                 br(),
                 h4("Whale Flows"),
                 plotOutput("whale_flow_plot", height = "200px"),
                 br(),
                 h4("Whale Concentration"),
                 plotOutput("whale_concentration_plot", height = "250px"),
                 br(),
                 h4("Top Whales"),
                 tableOutput("whale_table"),
                 br(),
                 h4("Fear & Greed Sentiment"),
                 plotOutput("sentiment_plot", height = "250px"),
                 br(),
                 tableOutput("positioning_table"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {

  # Reactive value to store results
  results <- reactiveVal(NULL)

  # Reactive value to track data source
  data_source <- reactiveVal("none")

  # ======================================
  # REACTIVE MODE BADGE
  # ======================================
  output$mode_badge <- renderUI({
    if (input$app_mode == "demo") {
      span(class = "demo-badge", "DEMO MODE")
    } else {
      span(class = "demo-badge", style = "background-color: #27ae60;", "LIVE MODE")
    }
  })

  # ======================================
  # AUTO-LOAD DEFAULT DEMO ON STARTUP
  # ======================================
  observe({
    if (!is.null(default_demo) && input$app_mode == "demo") {
      results(default_demo)
      data_source("Default Demo")
      output$status <- renderText("✅ Default demo auto-loaded")
    }
  })

  # ======================================
  # DEMO MODE HANDLER - Custom file upload
  # ======================================
  observeEvent(input$custom_demo, {
    req(input$custom_demo)

    tryCatch({
      custom_data <- readRDS(input$custom_demo$datapath)
      results(custom_data)
      data_source(paste("Custom:", input$custom_demo$name))
      output$status <- renderText(paste("✅ Loaded:", input$custom_demo$name))
    }, error = function(e) {
      output$status <- renderText(paste("❌ Error loading file:", e$message))
    })
  })

  # ======================================
  # SWITCH TO DEMO MODE WHEN SELECTED
  # ======================================
  observeEvent(input$app_mode, {
    if (input$app_mode == "demo" && !is.null(default_demo) && is.null(results())) {
      results(default_demo)
      data_source("Default Demo")
      output$status <- renderText("✅ Switched to demo mode")
    } else if (input$app_mode == "live") {
      # Clear results when switching to live mode
      results(NULL)
      data_source("none")
      output$status <- renderText("Ready to run live analysis")
    }
  })

  # ======================================
  # LIVE MODE HANDLER - WITH DEBUGGING
  # ======================================
  observeEvent(input$run_live, {

    output$status <- renderText("🔄 Running live analysis...")

    # Disable button to prevent double-clicks
    session$sendCustomMessage(type = "jsCode",
                              list(code = "$('#run_live').prop('disabled', true)"))

    tryCatch({
      print("========== LIVE MODE DEBUG ==========")
      print(paste("Start date:", input$dates[1]))
      print(paste("FRED API key provided:", input$fred_api_key != ""))

      # Run framework
      print("Calling crypto_predictive_framework...")

      res <- crypto_predictive_framework(
        assets = input$asset,
        start_date = as.character(input$dates[1]),
        fred_api_key = if(input$fred_api_key != "") input$fred_api_key else NULL
      )

      print("Framework returned successfully")
      print(paste("Class of res:", paste(class(res), collapse = ", ")))
      print(paste("Is list:", is.list(res)))
      print(paste("Has summary:", !is.null(res$summary)))

      if (!is.null(res$summary)) {
        print(paste("Risk color:", res$summary$risk$color))
        print(paste("Risk level:", res$summary$risk$level))
      }

      # Validate the result
      if (is.null(res) || !is.list(res)) {
        stop("Framework returned invalid result")
      }

      # Store results
      results(res)
      data_source(paste("Live Analysis -", Sys.Date()))
      output$status <- renderText("✅ Live analysis complete!")

      print("✅ Live mode completed successfully")
      print("======================================")

      # Show success notification
      showNotification(
        "Live analysis completed successfully!",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      print(paste("❌ ERROR in live mode:", e$message))
      print("Traceback:")
      print(traceback())

      output$status <- renderText(paste("❌ Error:", e$message))

      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = 10
      )

    }, finally = {
      # Re-enable button
      session$sendCustomMessage(type = "jsCode",
                                list(code = "$('#run_live').prop('disabled', false)"))
    })
  })

  # ======================================
  # DATA INFO DISPLAYS
  # ======================================

  output$data_source <- renderText({
    if (is.null(results())) {
      "No data loaded"
    } else {
      paste("Source:", data_source())
    }
  })

  output$data_date <- renderText({
    req(results())
    paste("Data from:", format(results()$metadata$timestamp, "%B %Y"))
  })

  output$current_price <- renderText({
    req(results())
    if (!is.null(results()$indicators$onchain)) {
      last_close <- tail(results()$indicators$onchain$close, 1)
      paste("BTC Price: $", format(round(last_close, 2), big.mark = ","))
    }
  })

  # ======================================
  # RISK AND METRICS DISPLAYS - SAFE VERSION
  # ======================================

  output$risk_box <- renderUI({
    # Debug output
    print("========== RISK BOX ==========")

    if (is.null(results())) {
      print("results() is NULL")
      return(div(class = "well",
                 h4("⏳ No Data Loaded"),
                 p("Please run analysis or load demo.")))
    }

    res <- results()
    print(paste("results class:", class(res)[1]))

    if (!is.list(res)) {
      print(paste("results is atomic vector of length:", length(res)))
      return(div(class = "well",
                 h4("⚠️ Data Error"),
                 p(paste("Expected list, got:", class(res)[1]))))
    }

    if (is.null(res$summary)) {
      print("res$summary is NULL")
      return(div(class = "well",
                 h4("⚠️ Incomplete Data"),
                 p("Summary missing from results.")))
    }

    if (is.null(res$summary$risk)) {
      print("res$summary$risk is NULL")
      return(div(class = "well",
                 h4("⚠️ Risk Data Missing"),
                 p("Risk information not available.")))
    }

    risk_color <- res$summary$risk$color %||% "UNKNOWN"
    risk_level <- res$summary$risk$level %||% "UNKNOWN"
    risk_factors <- paste(res$summary$risk$factors %||% "None", collapse = " • ")

    print(paste("Rendering with color:", risk_color, "level:", risk_level))

    div(class = paste("risk-", risk_color, sep = ""),
        h4(paste("⚠️", risk_color, "-", risk_level)),
        p(risk_factors)
    )
  })

  output$short_term <- renderUI({
    if (is.null(results())) {
      return(div(class = "well text-center", h4("No data")))
    }
    res <- results()
    signal <- tryCatch(res$summary$short_term %||% "Unknown", error = function(e) "Unknown")
    div(class = "well text-center", h4(signal))
  })

  output$medium_term <- renderUI({
    if (is.null(results())) {
      return(div(class = "well text-center", h4("No data")))
    }
    res <- results()
    signal <- tryCatch(res$summary$medium_term %||% "Unknown", error = function(e) "Unknown")
    alloc <- tryCatch(res$summary$allocation %||% "0%", error = function(e) "0%")
    div(class = "well text-center",
        h4(signal),
        p("Allocation: ", alloc)
    )
  })

  output$long_term <- renderUI({
    if (is.null(results())) {
      return(div(class = "well text-center", h4("No data")))
    }
    res <- results()
    signal <- tryCatch(res$summary$long_term %||% "Unknown", error = function(e) "Unknown")
    div(class = "well text-center", h4(signal))
  })

  output$summary <- renderPrint({
    req(results())
    print_crypto_summary(results())
  })

  # ======================================
  # HELPER FUNCTIONS
  # ======================================

  filter_by_date <- function(data, date_col, start, end) {
    if (is.null(data) || nrow(data) == 0) return(data)
    data[data[[date_col]] >= start & data[[date_col]] <= end, ]
  }

  # ======================================
  # PLOTTING FUNCTIONS (all with NULL checks)
  # ======================================

  output$mvrv_plot <- renderPlot({
    req(results())
    onchain <- results()$indicators$onchain
    if (is.null(onchain)) return(NULL)

    df <- filter_by_date(onchain, "timestamp", input$filter_dates[1], input$filter_dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = mvrv)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_hline(yintercept = c(0.7, 0.9, 1.75),
                 linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("rect", xmin = min(df$timestamp), xmax = max(df$timestamp),
               ymin = 0.7, ymax = 0.9, alpha = 0.1, fill = "orange") +
      annotate("text", x = min(df$timestamp), y = 0.65, label = "Extreme Bottom",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(df$timestamp), y = 1.8, label = "Extreme Top",
               hjust = 0, size = 3, color = "red") +
      labs(title = "MVRV Ratio",
           x = "Date", y = "MVRV") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$puell_plot <- renderPlot({
    req(results())
    onchain <- results()$indicators$onchain
    if (is.null(onchain)) return(NULL)

    df <- filter_by_date(onchain, "timestamp", input$filter_dates[1], input$filter_dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = puell_multiple)) +
      geom_line(color = "darkorange", linewidth = 1) +
      geom_hline(yintercept = c(0.7, 2),
                 linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("text", x = min(df$timestamp), y = 0.6, label = "Miner Capitulation",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(df$timestamp), y = 2.1, label = "Miner Selling",
               hjust = 0, size = 3, color = "red") +
      labs(title = "Puell Multiple",
           x = "Date", y = "Puell Multiple") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$nvt_plot <- renderPlot({
    req(results())
    onchain <- results()$indicators$onchain
    if (is.null(onchain)) return(NULL)

    df <- filter_by_date(onchain, "timestamp", input$filter_dates[1], input$filter_dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = nvts)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_hline(yintercept = c(30, 70),
                 linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("text", x = min(df$timestamp), y = 25, label = "Undervalued",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(df$timestamp), y = 75, label = "Overvalued",
               hjust = 0, size = 3, color = "red") +
      labs(title = "NVT Ratio",
           x = "Date", y = "NVT") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$onchain_table <- renderTable({
    req(results())
    oc <- results()$signals$onchain
    data.frame(
      Indicator = c("MVRV", "NVT", "Puell", "Composite"),
      Value = c(round(oc$mvrv$value, 2),
                round(oc$nvt$value, 2),
                round(oc$puell$value, 2),
                "-"),
      Regime = c(oc$mvrv$regime,
                 oc$nvt$regime,
                 oc$puell$regime,
                 oc$composite)
    )
  })

  output$volatility_regime_plot <- renderPlot({
    req(results())
    vol <- results()$indicators$volatility
    if (is.null(vol)) return(NULL)

    n <- length(vol$regime)
    last_n <- min(365, n)
    regime_data <- data.frame(
      date = tail(results()$indicators$onchain$timestamp, last_n),
      regime = tail(vol$regime, last_n)
    )

    regime_data <- regime_data[!is.na(regime_data$regime), ]

    ggplot(regime_data, aes(x = date, y = 1, fill = regime)) +
      geom_tile(height = 0.8) +
      scale_fill_manual(values = c("Extreme High" = "red",
                                   "High" = "orange",
                                   "Low" = "lightgreen",
                                   "Extreme Low" = "green")) +
      labs(title = "Volatility Regimes (Last 365 Days)",
           x = "Date", y = "", fill = "Regime") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })

  output$volatility_compare_plot <- renderPlot({
    req(results())
    vol <- results()$indicators$volatility
    onchain <- results()$indicators$onchain
    if (is.null(vol) || is.null(onchain)) return(NULL)

    n <- min(365, nrow(onchain))
    df <- data.frame(
      date = tail(onchain$timestamp, n),
      short_vol = tail(vol$volatility$short, n) * 100,
      medium_vol = tail(vol$volatility$medium, n) * 100
    )

    df <- na.omit(df)

    ggplot(df, aes(x = date)) +
      geom_line(aes(y = short_vol, color = "Short-term (7d)"), linewidth = 1) +
      geom_line(aes(y = medium_vol, color = "Medium-term (30d)"), linewidth = 1, linetype = "dashed") +
      scale_color_manual(values = c("Short-term (7d)" = "blue",
                                    "Medium-term (30d)" = "red")) +
      labs(title = "Volatility Comparison",
           x = "Date", y = "Annualized Volatility (%)",
           color = "Horizon") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  })

  output$volatility_signals_plot <- renderPlot({
    req(results())
    vol <- results()$indicators$volatility
    onchain <- results()$indicators$onchain
    if (is.null(vol) || is.null(onchain)) return(NULL)

    n <- min(365, nrow(onchain))
    df <- data.frame(
      date = tail(onchain$timestamp, n),
      breakout = tail(vol$signals$vol_breakout, n),
      compression = tail(vol$signals$vol_compression, n)
    )

    df$signal <- ifelse(df$breakout, "Breakout",
                        ifelse(df$compression, "Compression", "Normal"))

    ggplot(df, aes(x = date, y = 1, fill = signal)) +
      geom_tile(height = 0.8) +
      scale_fill_manual(values = c("Breakout" = "red",
                                   "Compression" = "green",
                                   "Normal" = "gray")) +
      labs(title = "Volatility Signals (Last 365 Days)",
           x = "Date", y = "", fill = "Signal Type") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })

  output$volatility_table <- renderTable({
    req(results())
    vol_sig <- results()$signals$volatility

    data.frame(
      Metric = c("Current Regime", "Environment", "Strategy",
                 "7d Percentile", "30d Percentile"),
      Value = c(
        vol_sig$regime,
        vol_sig$current_regime,
        vol_sig$strategy,
        paste0(round(vol_sig$percentile_short * 100, 1), "%"),
        paste0(round(vol_sig$percentile_medium * 100, 1), "%")
      )
    )
  })

  output$funding_plot <- renderPlot({
    req(results())
    deriv <- results()$raw_data$derivatives
    if (is.null(deriv) || is.null(deriv$data)) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No derivatives data") +
               theme_void())
    }

    df <- filter_by_date(deriv$data, "date", input$filter_dates[1], input$filter_dates[2])
    if (nrow(df) == 0) return(NULL)

    df$positive <- df$avg_funding_rate > 0

    ggplot(df, aes(x = date, y = avg_funding_rate * 100)) +
      geom_col(aes(fill = positive), alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_hline(yintercept = c(0.01, 0.05, -0.01),
                 linetype = "dashed", color = "gray50", alpha = 0.5) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      labs(title = "Funding Rates",
           x = "Date", y = "Funding Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
  })

  output$percentile_plot <- renderPlot({
    req(results())
    deriv <- results()$raw_data$derivatives
    if (is.null(deriv) || is.null(deriv$data)) return(NULL)

    df <- data.frame(
      date = deriv$data$date,
      oi_percentile = deriv$oi_percentile * 100,
      funding_percentile = deriv$funding_percentile * 100
    )

    df <- filter_by_date(df, "date", input$filter_dates[1], input$filter_dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = date)) +
      geom_line(aes(y = oi_percentile, color = "OI Percentile"), linewidth = 1) +
      geom_line(aes(y = funding_percentile, color = "Funding Percentile"),
                linewidth = 1, linetype = "dashed") +
      geom_hline(yintercept = c(10, 90), linetype = "dotted", color = "gray50") +
      scale_color_manual(values = c("OI Percentile" = "blue",
                                    "Funding Percentile" = "red")) +
      labs(title = "Percentile Ranks",
           x = "Date", y = "Percentile (%)",
           color = "Metric") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom")
  })

  output$crowded_plot <- renderPlot({
    req(results())
    signals <- results()$signals$derivatives

    df <- data.frame(
      signal = c("Crowded Long", "Crowded Short"),
      value = c(ifelse(signals$crowded_long, 1, 0),
                ifelse(signals$crowded_short, 1, 0))
    )

    ggplot(df, aes(x = signal, y = value, fill = signal)) +
      geom_col() +
      scale_fill_manual(values = c("Crowded Long" = "red",
                                   "Crowded Short" = "green")) +
      labs(title = "Crowded Trade Signals",
           x = "", y = "Signal (1 = Active)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  })

  output$derivatives_table <- renderTable({
    req(results())
    d <- results()$signals$derivatives
    if (!is.null(d)) {
      data.frame(
        Metric = c("Funding Regime", "OI Percentile", "Funding Percentile",
                   "OI Momentum", "Crowded Long", "Crowded Short",
                   "Data Confidence", "Interpretation"),
        Value = c(d$funding_regime,
                  paste0(round(d$oi_percentile * 100, 1), "%"),
                  paste0(round(d$funding_percentile * 100, 1), "%"),
                  paste0(round(d$oi_momentum * 100, 1), "%"),
                  ifelse(d$crowded_long, "⚠️ Yes", "No"),
                  ifelse(d$crowded_short, "⚠️ Yes", "No"),
                  paste0(d$percentile_confidence, " (", d$n_observations, " days)"),
                  d$interpretation)
      )
    }
  })

  output$m2_plot <- renderPlot({
    req(results())
    macro <- results()$raw_data$macro
    if (is.null(macro) || is.null(macro$liquidity$m2_us)) return(NULL)

    m2_data <- data.frame(
      date = macro$liquidity$m2_us$date,
      m2_yoy = macro$liquidity$m2_yoy * 100
    )

    m2_data <- filter_by_date(m2_data, "date", input$filter_dates[1], input$filter_dates[2])
    m2_data <- m2_data[!is.na(m2_data$m2_yoy), ]

    if (nrow(m2_data) == 0) return(NULL)

    ggplot(m2_data, aes(x = date, y = m2_yoy)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      geom_hline(yintercept = c(0, 5), linetype = "dashed", color = "red") +
      annotate("rect", xmin = min(m2_data$date), xmax = max(m2_data$date),
               ymin = 5, ymax = max(m2_data$m2_yoy, na.rm = TRUE),
               alpha = 0.1, fill = "green") +
      annotate("rect", xmin = min(m2_data$date), xmax = max(m2_data$date),
               ymin = min(m2_data$m2_yoy, na.rm = TRUE), ymax = 0,
               alpha = 0.1, fill = "red") +
      labs(title = "M2 Money Supply Growth",
           x = "Date", y = "YoY % Change") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$dxy_plot <- renderPlot({
    req(results())
    macro <- results()$raw_data$macro
    if (is.null(macro) || is.null(macro$dollar$dxy)) return(NULL)

    dxy <- macro$dollar$dxy
    dxy_df <- data.frame(
      date = as.Date(dxy$date),
      dxy = dxy$value
    )

    dxy_df <- filter_by_date(dxy_df, "date", input$filter_dates[1], input$filter_dates[2])
    if (nrow(dxy_df) == 0) return(NULL)

    ggplot(dxy_df, aes(x = date, y = dxy)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_hline(yintercept = c(110, 120), linetype = "dashed", color = "red") +
      annotate("rect", xmin = min(dxy_df$date), xmax = max(dxy_df$date),
               ymin = 110, ymax = 120, alpha = 0.1, fill = "orange") +
      labs(title = "US Dollar Index (FRED - DTWEXBGS)",
           x = "Date", y = "Index Value") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$composite_plot <- renderPlot({
    req(results())
    signals <- results()$signals$macro
    if (is.null(signals)) return(NULL)

    df <- data.frame(
      component = c("M2", "Rates", "Dollar", "Composite"),
      score = c(
        ifelse(signals$liquidity_regime == "Expanding", 1,
               ifelse(signals$liquidity_regime == "Contracting", -1, 0)),
        ifelse(signals$rate_regime == "Low Rates (Risk-On)", 1,
               ifelse(signals$rate_regime == "High Rates (Risk-Off)", -1, 0)),
        ifelse(signals$dollar_regime == "Weak Dollar (Bullish Crypto)", 1,
               ifelse(signals$dollar_regime == "Strong Dollar (Bearish Crypto)", -1, 0)),
        signals$composite_risk
      )
    )

    ggplot(df, aes(x = component, y = score, fill = score)) +
      geom_col() +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0) +
      labs(title = "Macro Component Scores",
           x = "", y = "Score (-1 to +1)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  })

  output$macro_table <- renderTable({
    req(results())
    m <- results()$signals$macro
    if (!is.null(m)) {
      data.frame(
        Indicator = c("Liquidity", "Rates", "Dollar", "Composite Risk", "Bias"),
        Regime = c(m$liquidity_regime, m$rate_regime, m$dollar_regime, "-", m$risk_bias),
        Value = c("-", "-", "-", round(m$composite_risk, 2), "-")
      )
    }
  })

  output$whale_flow_plot <- renderPlot({
    req(results())
    pos <- results()$signals$positioning
    if (is.null(pos) || is.null(pos$vault_flows)) {
      df <- data.frame(x = 1, y = 1, label = "No whale flow data")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void()
    } else {
      flow_color <- ifelse(grepl("Inflows", pos$vault_flows$flow_regime), "green",
                           ifelse(grepl("Outflows", pos$vault_flows$flow_regime), "red", "gray"))

      netflow <- pos$vault_flows$net_flow_pct

      ggplot() +
        annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.4,
                 fill = flow_color, alpha = 0.5) +
        annotate("text", x = 0.5, y = 0.2,
                 label = paste("Flow Regime:", pos$vault_flows$flow_regime), size = 5) +
        annotate("text", x = 0.5, y = 0.7,
                 label = paste("Net Flow:", round(netflow, 1), "%"), size = 6, fontface = "bold") +
        labs(title = "Whale Flows") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    }
  })

  output$whale_concentration_plot <- renderPlot({
    req(results())
    pos <- results()$signals$positioning
    if (is.null(pos) || is.null(pos$whale_concentration)) {
      df <- data.frame(x = 1, y = 1, label = "No whale concentration data")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void()
    } else {
      wc <- pos$whale_concentration

      df <- data.frame(
        category = c("Mega Whales (>$10M)", "Regular Whales ($1M-$10M)",
                     "Dolphins ($100k-$1M)", "Retail (<$100k)"),
        count = c(wc$mega_whales, wc$regular_whales, wc$dolphins %||% 0, wc$retail %||% 0)
      )

      hhi_color <- ifelse(wc$hhi > 2500, "red",
                          ifelse(wc$hhi > 1500, "orange", "green"))

      ggplot(df, aes(x = category, y = count, fill = category)) +
        geom_col() +
        geom_text(aes(label = count), vjust = -0.5) +
        annotate("text", x = 2, y = max(df$count, na.rm = TRUE) * 1.2,
                 label = paste("HHI:", round(wc$hhi, 1), "-", wc$hhi_interpretation),
                 color = hhi_color, size = 5, fontface = "bold") +
        labs(title = "Whale Concentration",
             x = "", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  })

  output$whale_table <- renderTable({
    req(results())
    pos <- results()$signals$positioning
    if (!is.null(pos) && !is.null(pos$whale_concentration)) {
      wc <- pos$whale_concentration
      data.frame(
        Metric = c("Total Unique Wallets", "Mega Whales (>$10M)",
                   "Regular Whales ($1M-$10M)", "HHI", "Top 5 Share", "Concentration Risk"),
        Value = c(wc$unique_whales, wc$mega_whales, wc$regular_whales,
                  round(wc$hhi, 1), paste0(round(wc$top_5_share, 1), "%"),
                  wc$concentration_risk)
      )
    } else {
      data.frame(Message = "No whale concentration data")
    }
  })

  output$sentiment_plot <- renderPlot({
    req(results())
    sent <- results()$signals$sentiment
    if (is.null(sent)) {
      df <- data.frame(x = 1, y = 1, label = "No sentiment data")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void()
    } else {
      df <- data.frame(
        xmin = c(0, 25, 45, 55, 75),
        xmax = c(25, 45, 55, 75, 100),
        ymin = 0, ymax = 1,
        zone = c("Extreme Fear", "Fear", "Neutral", "Greed", "Extreme Greed")
      )

      value <- sent$value

      ggplot() +
        geom_rect(data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone),
                  alpha = 0.3) +
        geom_vline(xintercept = value, size = 2, color = "black") +
        annotate("text", x = value, y = 1.2,
                 label = paste0(value, "/100"), size = 5, fontface = "bold") +
        annotate("text", x = 50, y = -0.2,
                 label = paste("Classification:", sent$classification), size = 4) +
        annotate("text", x = 50, y = -0.4,
                 label = paste("Contrarian:", sent$contrarian_signal), size = 4,
                 color = ifelse(sent$contrarian_signal == "Bullish (buy)", "green",
                                ifelse(sent$contrarian_signal == "Bearish (sell)", "red", "gray"))) +
        scale_fill_manual(values = c("Extreme Fear" = "darkgreen",
                                     "Fear" = "lightgreen",
                                     "Neutral" = "gray",
                                     "Greed" = "orange",
                                     "Extreme Greed" = "red")) +
        labs(title = "Fear & Greed Index",
             x = "Value", y = "") +
        xlim(0, 100) + ylim(-0.5, 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "bottom")
    }
  })

  output$positioning_table <- renderTable({
    req(results())

    if (is.null(results()$signals$positioning)) {
      data.frame(Message = "No positioning data available")
    } else {
      pos <- results()$signals$positioning

      df <- data.frame(
        Metric = c("Whale Flow", "Whale Activity", "Interpretation"),
        Value = c(pos$whale_flow, pos$whale_activity, pos$whale_interpretation)
      )

      if (!is.null(pos$whale_concentration)) {
        wc <- pos$whale_concentration
        conc_df <- data.frame(
          Metric = c("HHI", "Top 5 Share", "Concentration Risk"),
          Value = c(round(wc$hhi, 1),
                    paste0(round(wc$top_5_share, 1), "%"),
                    wc$concentration_risk)
        )
        df <- rbind(df, conc_df)
      }

      if (!is.null(results()$signals$sentiment)) {
        sent <- results()$signals$sentiment
        sent_df <- data.frame(
          Metric = c("Fear & Greed", "Sentiment Signal"),
          Value = c(paste0(sent$value, "/100 - ", sent$classification),
                    sent$contrarian_signal)
        )
        df <- rbind(df, sent_df)
      }
      df
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
