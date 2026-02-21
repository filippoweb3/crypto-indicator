library(shiny)
library(CryptoIndicator)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"),

  # Custom CSS
  tags$head(
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
      .demo-mode {
        background-color: #2ecc71;
        color: white;
        padding: 5px;
        border-radius: 5px;
        text-align: center;
        font-weight: bold;
        margin-bottom: 10px;
      }
    "))
  ),

  # App title
  titlePanel("🚀 CryptoIndicator Dashboard"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,

      # Demo mode toggle
      div(class = "api-section",
          h4("🎮 App Mode", style = "color: #3498db;"),
          radioButtons("app_mode",
                       "Select Mode:",
                       choices = c(
                         "Demo Mode (Load stored data)" = "demo",
                         "Live Mode (Run analysis)" = "live"
                       ),
                       selected = "demo")
      ),

      # Conditional UI for demo mode
      conditionalPanel(
        condition = "input.app_mode == 'demo'",
        div(class = "api-section",
            h4("📁 Demo Data", style = "color: #3498db;"),
            fileInput("result_file",
                      "Upload result.rds file",
                      accept = c(".rds")),
            tags$small(class = "text-muted",
                       "Download the result file from the live analysis and share it with interviewers.")
        )
      ),

      # Conditional UI for live mode
      conditionalPanel(
        condition = "input.app_mode == 'live'",
        div(class = "api-section",
            h4("🔑 API Keys", style = "color: #3498db;"),

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

            # FRED API Key dropdown
            selectInput("fred_key_select",
                        "FRED API Key:",
                        choices = c("None", "Use from global_variables"),
                        selected = "Use from global_variables"),

            # Whale Alert API Key dropdown
            selectInput("whale_key_select",
                        "Whale Alert API Key:",
                        choices = c("None", "Use from global_variables"),
                        selected = "Use from global_variables"),

            tags$small(class = "text-muted",
                       "Select 'Use from global_variables' to use keys from global_variables list")
        ),

        # Action button
        actionButton("run", "Run Analysis",
                     class = "btn-primary",
                     style = "width: 100%;"),

        br(), br(),

        # Status message
        textOutput("status"),
        br(),
        textOutput("last_price"),

        # API Status
        br(),
        div(class = "api-section",
            h5("API Status:", style = "color: #3498db;"),
            textOutput("fred_status"),
            textOutput("whale_status")
        )
      ),

      # Demo mode indicator
      conditionalPanel(
        condition = "input.app_mode == 'demo'",
        div(class = "demo-mode",
            "✨ Demo Mode Active - Using Stored Data ✨"
        )
      )
    ),

    # Main panel for outputs
    mainPanel(
      width = 9,

      # Risk level display
      fluidRow(
        column(12,
               h3("🎯 Current Risk Level"),
               uiOutput("risk_box")
        )
      ),

      # Key metrics
      fluidRow(
        column(4, h4("📈 Short-Term"), uiOutput("short_term")),
        column(4, h4("📊 Medium-Term"), uiOutput("medium_term")),
        column(4, h4("📉 Long-Term"), uiOutput("long_term"))
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
                 h4("US Dollar Index (DXY)"),
                 plotOutput("dxy_plot", height = "250px"),
                 br(),
                 h4("Composite Risk Score"),
                 plotOutput("composite_plot", height = "150px"),
                 br(),
                 tableOutput("macro_table")),

        tabPanel("Positioning",
                 br(),
                 h4("Whale Flow"),
                 plotOutput("whale_plot", height = "150px"),
                 br(),
                 h4("Google Trends Sentiment"),
                 plotOutput("sentiment_plot", height = "250px"),
                 br(),
                 h4("Sentiment History"),
                 plotOutput("sentiment_history_plot", height = "250px"),
                 br(),
                 tableOutput("positioning_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive value to store results
  results <- reactiveVal(NULL)

  # Global variables list
  global_variables <- reactive({
    if (exists("global_variables", envir = .GlobalEnv)) {
      get("global_variables", envir = .GlobalEnv)
    } else {
      list(
        fred_api_key = NULL,
        whaleAlert_api_key = NULL
      )
    }
  })

  # Update API key dropdowns based on available keys
  observe({
    gv <- global_variables()

    # Update FRED choices
    fred_choices <- c("None")
    if (!is.null(gv$fred_api_key) && gv$fred_api_key != "") {
      fred_choices <- c(fred_choices, "Use from global_variables")
    }
    updateSelectInput(session, "fred_key_select", choices = fred_choices)

    # Update Whale Alert choices
    whale_choices <- c("None")
    if (!is.null(gv$whaleAlert_api_key) && gv$whaleAlert_api_key != "") {
      whale_choices <- c(whale_choices, "Use from global_variables")
    }
    updateSelectInput(session, "whale_key_select", choices = whale_choices)
  })

  # API Status displays
  output$fred_status <- renderText({
    gv <- global_variables()
    if (!is.null(gv$fred_api_key) && gv$fred_api_key != "") {
      "✅ FRED API Key available"
    } else {
      "❌ FRED API Key not set"
    }
  })

  output$whale_status <- renderText({
    gv <- global_variables()
    if (!is.null(gv$whaleAlert_api_key) && gv$whaleAlert_api_key != "") {
      "✅ Whale Alert API Key available"
    } else {
      "❌ Whale Alert API Key not set"
    }
  })

  # Load stored result file
  observeEvent(input$result_file, {
    req(input$result_file)

    tryCatch({
      res <- readRDS(input$result_file$datapath)
      results(res)
      output$status <- renderText("✅ Demo data loaded successfully!")
    }, error = function(e) {
      output$status <- renderText(paste("❌ Error loading file:", e$message))
    })
  })

  # Run analysis when button is clicked (live mode)
  observeEvent(input$run, {
    req(input$app_mode == "live")

    output$status <- renderText("🔄 Running analysis...")

    # Set API keys from global_variables if selected
    gv <- global_variables()

    if (input$fred_key_select == "Use from global_variables" && !is.null(gv$fred_api_key)) {
      assign("fred_api_key", gv$fred_api_key, envir = .GlobalEnv)
    }

    if (input$whale_key_select == "Use from global_variables" && !is.null(gv$whaleAlert_api_key)) {
      assign("whaleAlert_api_key", gv$whaleAlert_api_key, envir = .GlobalEnv)
    }

    # Run framework
    tryCatch({
      res <- crypto_predictive_framework(
        assets = input$asset,
        start_date = as.character(input$dates[1]),
        include_google_trends = TRUE
      )

      results(res)
      output$status <- renderText("✅ Analysis complete!")

    }, error = function(e) {
      output$status <- renderText(paste("❌ Error:", e$message))
    })
  })

  # Last price display
  output$last_price <- renderText({
    req(results())
    if (!is.null(results()$indicators$onchain)) {
      onchain <- results()$indicators$onchain
      last_close <- onchain$close[nrow(onchain)]
      paste("Last Price: $", format(round(last_close, 2), big.mark = ","))
    } else {
      ""
    }
  })

  # Risk box
  output$risk_box <- renderUI({
    req(results())

    risk_color <- results()$decisions$risk_off$color
    risk_level <- results()$decisions$risk_off$level
    risk_factors <- paste(results()$decisions$risk_off$factors, collapse = " • ")

    div(class = paste("risk-", risk_color, sep = ""),
        h4(paste("⚠️", risk_color, "-", risk_level)),
        p(risk_factors)
    )
  })

  # Time horizon boxes
  output$short_term <- renderUI({
    req(results())
    signal <- results()$decisions$short_term$momentum$signal
    score <- results()$decisions$short_term$momentum$score
    div(class = "well text-center",
        h4(signal),
        p("Score: ", score)
    )
  })

  output$medium_term <- renderUI({
    req(results())
    signal <- results()$decisions$medium_term$trend$direction
    score <- results()$decisions$medium_term$trend$score
    alloc <- results()$decisions$medium_term$position_sizing$recommended_allocation_pct
    div(class = "well text-center",
        h4(signal),
        p("Score: ", score),
        p("Allocation: ", alloc, "%")
    )
  })

  output$long_term <- renderUI({
    req(results())
    signal <- results()$decisions$long_term$strategic$core_position
    cycle <- results()$decisions$long_term$cycle$interpretation
    div(class = "well text-center",
        h4(signal),
        p(cycle)
    )
  })

  # Summary text
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
  # ON-CHAIN MODULE
  # ======================================

  output$mvrv_plot <- renderPlot({
    req(results())
    onchain <- results()$indicators$onchain
    if (is.null(onchain)) return(NULL)

    df <- filter_by_date(onchain, "timestamp", input$dates[1], input$dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = mvrv)) +
      geom_line(color = "steelblue", size = 1) +
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

    df <- filter_by_date(onchain, "timestamp", input$dates[1], input$dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = puell_multiple)) +
      geom_line(color = "darkorange", size = 1) +
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

    df <- filter_by_date(onchain, "timestamp", input$dates[1], input$dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = timestamp, y = nvts)) +
      geom_line(color = "purple", size = 1) +
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
                 oc$composite),
      Signal = c(oc$mvrv$signal, "-", "-", oc$composite)
    )
  })

  # ======================================
  # VOLATILITY MODULE
  # ======================================

  output$volatility_regime_plot <- renderPlot({
    req(results())
    vol <- results()$indicators$volatility
    if (is.null(vol)) return(NULL)

    # Get the last 365 days of volatility data
    n <- length(vol$regime)
    last_n <- min(365, n)
    regime_data <- data.frame(
      date = tail(results()$indicators$onchain$timestamp, last_n),
      regime = tail(vol$regime, last_n)
    )

    # Create a colored bar plot for regimes
    regime_data$color <- case_when(
      regime_data$regime == "Extreme High" ~ "red",
      regime_data$regime == "High" ~ "orange",
      regime_data$regime == "Low" ~ "lightgreen",
      regime_data$regime == "Extreme Low" ~ "green",
      TRUE ~ "gray"
    )

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

    # Get last 365 days
    n <- min(365, nrow(onchain))
    df <- data.frame(
      date = tail(onchain$timestamp, n),
      short_vol = tail(vol$volatility$short, n) * 100,
      medium_vol = tail(vol$volatility$medium, n) * 100
    )

    ggplot(df, aes(x = date)) +
      geom_line(aes(y = short_vol, color = "Short-term (7d)"), size = 1) +
      geom_line(aes(y = medium_vol, color = "Medium-term (30d)"), size = 1, linetype = "dashed") +
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

    # Get last 365 days
    n <- min(365, nrow(onchain))
    df <- data.frame(
      date = tail(onchain$timestamp, n),
      breakout = tail(vol$signals$vol_breakout, n),
      compression = tail(vol$signals$vol_compression, n)
    )

    # Create signals plot
    df$signal <- ifelse(df$breakout, "Breakout",
                        ifelse(df$compression, "Compression", "Normal"))
    df$color <- ifelse(df$breakout, "red",
                       ifelse(df$compression, "green", "gray"))

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

    # Calculate current signal counts (last 30 days)
    vol_full <- results()$indicators$volatility
    last_30 <- tail(vol_full$signals$vol_breakout, 30)
    breakout_count <- sum(last_30, na.rm = TRUE)
    compression_count <- sum(tail(vol_full$signals$vol_compression, 30), na.rm = TRUE)

    data.frame(
      Metric = c("Current Regime", "Environment", "Strategy",
                 "7d Percentile", "30d Percentile",
                 "Breakouts (30d)", "Compressions (30d)"),
      Value = c(
        vol_sig$regime,
        vol_sig$current_regime,
        vol_sig$strategy,
        paste0(round(vol_sig$percentile_short * 100, 1), "%"),
        paste0(round(vol_sig$percentile_medium * 100, 1), "%"),
        breakout_count,
        compression_count
      )
    )
  })

  # ======================================
  # DERIVATIVES MODULE
  # ======================================

  output$funding_plot <- renderPlot({
    req(results())
    deriv <- results()$raw_data$derivatives
    if (is.null(deriv) || is.null(deriv$data)) return(NULL)

    df <- filter_by_date(deriv$data, "date", input$dates[1], input$dates[2])
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

    df <- filter_by_date(df, "date", input$dates[1], input$dates[2])
    if (nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = date)) +
      geom_line(aes(y = oi_percentile, color = "OI Percentile"), size = 1) +
      geom_line(aes(y = funding_percentile, color = "Funding Percentile"),
                size = 1, linetype = "dashed") +
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

    # Create a simple indicator of crowded signals
    df <- data.frame(
      signal = c("Crowded Long", "Crowded Short", "OI Momentum"),
      value = c(ifelse(signals$crowded_long, 1, 0),
                ifelse(signals$crowded_short, 1, 0),
                signals$oi_momentum)
    )

    ggplot(df, aes(x = signal, y = value, fill = signal)) +
      geom_col() +
      scale_fill_manual(values = c("Crowded Long" = "red",
                                   "Crowded Short" = "green",
                                   "OI Momentum" = "blue")) +
      labs(title = "Crowded Trade Signals",
           x = "", y = "Signal Strength") +
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
                   "OI Momentum", "Crowded Long", "Crowded Short", "Interpretation"),
        Value = c(d$funding_regime,
                  paste0(round(d$oi_percentile * 100, 1), "%"),
                  paste0(round(d$funding_percentile * 100, 1), "%"),
                  paste0(round(d$oi_momentum * 100, 1), "%"),
                  ifelse(d$crowded_long, "⚠️ Yes", "No"),
                  ifelse(d$crowded_short, "⚠️ Yes", "No"),
                  d$interpretation)
      )
    }
  })

  # ======================================
  # MACRO MODULE
  # ======================================

  output$m2_plot <- renderPlot({
    req(results())
    macro <- results()$raw_data$macro
    if (is.null(macro) || is.null(macro$liquidity$m2_us)) return(NULL)

    m2_data <- data.frame(
      date = macro$liquidity$m2_us$date,
      m2_yoy = macro$liquidity$m2_yoy * 100
    )

    m2_data <- filter_by_date(m2_data, "date", input$dates[1], input$dates[2])
    m2_data <- m2_data[!is.na(m2_data$m2_yoy), ]

    if (nrow(m2_data) == 0) return(NULL)

    ggplot(m2_data, aes(x = date, y = m2_yoy)) +
      geom_line(color = "darkgreen", size = 1) +
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
      date = as.Date(index(dxy)),
      dxy = as.numeric(Cl(dxy))
    )

    dxy_df <- filter_by_date(dxy_df, "date", input$dates[1], input$dates[2])
    if (nrow(dxy_df) == 0) return(NULL)

    ggplot(dxy_df, aes(x = date, y = dxy)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = c(90, 100), linetype = "dashed", color = "red") +
      annotate("rect", xmin = min(dxy_df$date), xmax = max(dxy_df$date),
               ymin = 90, ymax = 100, alpha = 0.1, fill = "orange") +
      labs(title = "US Dollar Index (DXY)",
           x = "Date", y = "DXY") +
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

  # ======================================
  # POSITIONING MODULE
  # ======================================

  output$whale_plot <- renderPlot({
    req(results())
    pos <- results()$signals$positioning
    if (is.null(pos)) {
      df <- data.frame(x = 1, y = 1, label = "No whale data available")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void() +
        labs(title = "Whale Flow")
    } else {
      # Determine color based on flow
      flow_color <- ifelse(grepl("Extreme Inflows", pos$whale_flow), "red",
                           ifelse(grepl("Inflows", pos$whale_flow), "orange",
                                  ifelse(grepl("Extreme Outflows", pos$whale_flow), "green",
                                         ifelse(grepl("Outflows", pos$whale_flow), "lightgreen", "gray"))))

      # Get netflow data if available
      netflow <- ifelse(!is.null(results()$raw_data$positioning),
                        results()$raw_data$positioning$exchange_positioning$netflow_summary$netflow_usd / 1e6,
                        0)

      ggplot() +
        annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                 fill = flow_color, alpha = 0.5) +
        annotate("text", x = 0.5, y = 0.7,
                 label = paste("Flow:", pos$whale_flow), size = 4) +
        annotate("text", x = 0.5, y = 0.4,
                 label = paste("Net Flow: $", round(netflow, 1), "M"), size = 4) +
        labs(title = "Whale Positioning") +
        xlim(0, 1) + ylim(0, 1) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    }
  })

  output$sentiment_plot <- renderPlot({
    req(results())
    pos <- results()$signals$positioning
    if (is.null(pos) || is.null(pos$sentiment)) {
      df <- data.frame(x = 1, y = 1, label = "No Google Trends data")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void() +
        labs(title = "Retail Sentiment")
    } else {
      # Create a gauge-like plot
      df <- data.frame(
        x = c(0, 20, 40, 60, 80, 100),
        xend = c(20, 40, 60, 80, 100, 120),
        y = 1, yend = 1,
        zone = c("Low", "Normal", "Normal", "High", "Extreme", "")
      )

      ggplot() +
        geom_segment(data = df, aes(x = x, xend = xend, y = y, yend = yend, color = zone),
                     size = 20, alpha = 0.3) +
        geom_vline(xintercept = pos$sentiment$score, size = 2, color = "black") +
        annotate("text", x = pos$sentiment$score, y = 1.5,
                 label = paste0(round(pos$sentiment$score, 1), "/100"), size = 5) +
        annotate("text", x = 50, y = 0.5,
                 label = pos$sentiment$signal, size = 4) +
        scale_color_manual(values = c("Low" = "green", "Normal" = "yellow",
                                      "High" = "orange", "Extreme" = "red")) +
        labs(title = "Google Trends Sentiment") +
        xlim(0, 100) + ylim(0, 2) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              legend.position = "none")
    }
  })

  output$sentiment_history_plot <- renderPlot({
    req(results())
    pos_raw <- results()$raw_data$positioning
    if (is.null(pos_raw) || is.null(pos_raw$google_trends)) return(NULL)

    trends_data <- pos_raw$google_trends$interest_over_time
    if (is.null(trends_data)) return(NULL)

    trends_df <- data.frame(
      date = as.Date(trends_data$date),
      hits = trends_data$hits
    )

    trends_df <- filter_by_date(trends_df, "date", input$dates[1], input$dates[2])

    ggplot(trends_df, aes(x = date, y = hits)) +
      geom_line(color = "orange", size = 1) +
      geom_hline(yintercept = c(20, 80), linetype = "dashed", color = "red") +
      labs(title = "Google Trends History",
           x = "Date", y = "Search Interest (0-100)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$positioning_table <- renderTable({
    req(results())

    if (is.null(results()$signals$positioning)) {
      data.frame(Message = "No positioning data available. Provide Whale Alert API key.")
    } else {
      pos <- results()$signals$positioning

      df <- data.frame(
        Metric = c("Whale Flow", "Whale Activity", "Interpretation"),
        Value = c(pos$whale_flow, pos$whale_activity, pos$whale_interpretation)
      )

      if (!is.null(pos$sentiment)) {
        sent_df <- data.frame(
          Metric = c("Google Trends Score", "Sentiment Signal"),
          Value = c(paste0(round(pos$sentiment$score, 1), "/100"), pos$sentiment$signal)
        )
        df <- rbind(df, sent_df)
      }
      df
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
