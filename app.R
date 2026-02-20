# app.R
# Simplified CryptoIndicator Shiny Application
# Complete with all modules - No namespace conflicts

library(shiny)
library(CryptoIndicator)
library(ggplot2)
library(dplyr)
library(zoo)
library(quantmod)

# Define UI
ui <- fluidPage(

  # App title
  titlePanel("🚀 CryptoIndicator Dashboard"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,

      # Asset selection
      selectInput("asset",
                  "Select Asset:",
                  choices = c("Bitcoin", "Ethereum"),
                  selected = "Bitcoin"),

      # Date range
      dateRangeInput("dates",
                     "Date Range:",
                     start = "2020-01-01",
                     end = Sys.Date()),

      # API keys (optional)
      textInput("fred_key", "FRED API Key (optional):", value = "",
                placeholder = "Enter FRED API key"),
      textInput("whale_key", "Whale Alert API Key (optional):", value = "",
                placeholder = "Enter Whale Alert API key"),

      # Action button
      actionButton("run", "Run Analysis",
                   class = "btn-primary",
                   style = "width: 100%;"),

      # Status message
      br(),
      br(),
      textOutput("status"),
      br(),
      textOutput("last_price")
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
                 plotOutput("mvrv_plot", height = "300px"),
                 br(),
                 h4("Puell Multiple"),
                 plotOutput("puell_plot", height = "300px"),
                 br(),
                 tableOutput("onchain_table")),

        tabPanel("Derivatives",
                 br(),
                 h4("Funding Rates"),
                 plotOutput("funding_plot", height = "300px"),
                 br(),
                 h4("Open Interest & Funding Percentiles"),
                 plotOutput("percentile_plot", height = "300px"),
                 br(),
                 tableOutput("derivatives_table")),

        tabPanel("Macro",
                 br(),
                 h4("M2 Money Supply Growth"),
                 plotOutput("m2_plot", height = "300px"),
                 br(),
                 h4("US Dollar Index (DXY)"),
                 plotOutput("dxy_plot", height = "300px"),
                 br(),
                 tableOutput("macro_table")),

        tabPanel("Positioning",
                 br(),
                 h4("Whale Flow"),
                 plotOutput("whale_plot", height = "200px"),
                 br(),
                 h4("Google Trends Sentiment"),
                 plotOutput("sentiment_plot", height = "300px"),
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

  # Run analysis when button is clicked
  observeEvent(input$run, {

    # Show status
    output$status <- renderText("🔄 Running analysis...")

    # Set API keys if provided
    if (input$fred_key != "") {
      assign("fred_api_key", input$fred_key, envir = .GlobalEnv)
    }
    if (input$whale_key != "") {
      assign("whaleAlert_api_key", input$whale_key, envir = .GlobalEnv)
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
    risk_factors <- paste(results()$decisions$risk_off$factors, collapse = ", ")

    box_color <- switch(risk_color,
                        "GREEN" = "success",
                        "YELLOW" = "warning",
                        "RED" = "danger",
                        "info")

    div(
      class = paste("alert alert-", box_color, " text-center", sep = ""),
      style = "font-size: 20px; font-weight: bold; padding: 15px;",
      paste(risk_color, "-", risk_level),
      br(),
      span(style = "font-size: 14px;", risk_factors)
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
  # ON-CHAIN MODULE
  # ======================================

  # Helper function to filter by date
  filter_by_date <- function(data, date_col, start, end) {
    if (is.null(data) || nrow(data) == 0) return(data)
    data[data[[date_col]] >= start & data[[date_col]] <= end, ]
  }

  # MVRV plot
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
      annotate("text", x = min(df$timestamp), y = 0.65, label = "Extreme Bottom",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(df$timestamp), y = 1.8, label = "Extreme Top",
               hjust = 0, size = 3, color = "red") +
      labs(title = "MVRV Ratio",
           x = "Date", y = "MVRV") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Puell Multiple plot
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

  # On-chain table
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
  # DERIVATIVES MODULE
  # ======================================

  # Funding rates plot
  output$funding_plot <- renderPlot({
    req(results())

    deriv <- results()$raw_data$derivatives
    if (is.null(deriv) || is.null(deriv$data)) return(NULL)

    df <- filter_by_date(deriv$data, "date", input$dates[1], input$dates[2])

    if (nrow(df) == 0) return(NULL)

    # Create positive/negative flag
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

  # Percentile plot
  output$percentile_plot <- renderPlot({
    req(results())

    deriv <- results()$raw_data$derivatives
    if (is.null(deriv) || is.null(deriv$data)) return(NULL)

    # Create dataframe with percentiles
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

  # Derivatives table
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

  # M2 plot
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
      annotate("text", x = min(m2_data$date), y = 5.5, label = "Expanding",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(m2_data$date), y = -0.5, label = "Contracting",
               hjust = 0, size = 3, color = "red") +
      labs(title = "M2 Money Supply Growth",
           x = "Date", y = "YoY % Change") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # DXY plot
  output$dxy_plot <- renderPlot({
    req(results())

    macro <- results()$raw_data$macro
    if (is.null(macro) || is.null(macro$dollar$dxy)) return(NULL)

    dxy <- macro$dollar$dxy
    dxy_df <- data.frame(
      date = as.Date(zoo::index(dxy)),
      dxy = as.numeric(quantmod::Cl(dxy))
    )

    dxy_df <- filter_by_date(dxy_df, "date", input$dates[1], input$dates[2])

    if (nrow(dxy_df) == 0) return(NULL)

    ggplot(dxy_df, aes(x = date, y = dxy)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = c(90, 100), linetype = "dashed", color = "red") +
      annotate("text", x = min(dxy_df$date), y = 89, label = "Weak Dollar (Bullish)",
               hjust = 0, size = 3, color = "green") +
      annotate("text", x = min(dxy_df$date), y = 101, label = "Strong Dollar (Bearish)",
               hjust = 0, size = 3, color = "red") +
      labs(title = "US Dollar Index (DXY)",
           x = "Date", y = "DXY") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Macro table
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

  # Whale flow plot (simple colored box with text)
  output$whale_plot <- renderPlot({
    req(results())

    pos <- results()$signals$positioning
    if (is.null(pos)) {
      # Create empty plot with message
      df <- data.frame(x = 1, y = 1, label = "No whale data available.\nProvide Whale Alert API key.")
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

      df <- data.frame(
        xmin = 0, xmax = 1, ymin = 0, ymax = 1
      )

      ggplot(df) +
        geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = flow_color, alpha = 0.5) +
        annotate("text", x = 0.5, y = 0.7,
                 label = paste("Flow:", pos$whale_flow), size = 4) +
        annotate("text", x = 0.5, y = 0.4,
                 label = paste("Activity:", pos$whale_activity), size = 4) +
        labs(title = "Whale Positioning") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    }
  })

  # Google Trends sentiment plot
  output$sentiment_plot <- renderPlot({
    req(results())

    pos <- results()$signals$positioning
    if (is.null(pos) || is.null(pos$sentiment)) {
      df <- data.frame(x = 1, y = 1, label = "No Google Trends data available")
      ggplot(df, aes(x = x, y = y)) +
        geom_text(aes(label = label), size = 5) +
        theme_void() +
        labs(title = "Retail Sentiment")
    } else {
      # Create a simple bar plot
      df <- data.frame(
        metric = "Google Trends",
        score = pos$sentiment$score
      )

      ggplot(df, aes(x = metric, y = score)) +
        geom_col(fill = ifelse(pos$sentiment$score > 80, "red",
                               ifelse(pos$sentiment$score < 20, "green", "orange")),
                 width = 0.3) +
        geom_hline(yintercept = c(20, 60, 80), linetype = "dashed", color = "gray50") +
        annotate("text", x = 1, y = 10, label = "Low Interest\n(Potential Bottom)",
                 hjust = 0.5, size = 3, color = "green") +
        annotate("text", x = 1, y = 90, label = "Extreme Interest\n(Potential Top)",
                 hjust = 0.5, size = 3, color = "red") +
        annotate("text", x = 1, y = pos$sentiment$score + 5,
                 label = pos$sentiment$signal, size = 3, hjust = 0.5) +
        labs(title = "Google Trends Sentiment",
             x = "", y = "Score (0-100)") +
        ylim(0, 105) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  # Positioning table
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
