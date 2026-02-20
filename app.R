# app.R
library(shiny)
library(CryptoIndicator)
library(ggplot2)
library(dplyr)

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
      textInput("fred_key", "FRED API Key (optional):", value = ""),
      textInput("whale_key", "Whale Alert API Key (optional):", value = ""),

      # Action button
      actionButton("run", "Run Analysis",
                   class = "btn-primary",
                   style = "width: 100%;"),

      # Status message
      br(),
      br(),
      textOutput("status")
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
                 plotOutput("mvrv_plot"),
                 br(),
                 tableOutput("onchain_table")),

        tabPanel("Derivatives",
                 br(),
                 plotOutput("derivatives_plot"),
                 br(),
                 tableOutput("derivatives_table")),

        tabPanel("Macro",
                 br(),
                 plotOutput("macro_plot"),
                 br(),
                 tableOutput("macro_table"))
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
    output$status <- renderText("Running analysis...")

    # Set API keys if provided
    if (input$fred_key != "") {
      options(fred_api_key = input$fred_key)
    }
    if (input$whale_key != "") {
      options(whaleAlert_api_key = input$whale_key)
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

  # Risk box
  output$risk_box <- renderUI({
    req(results())

    risk_color <- results()$decisions$risk_off$color
    risk_level <- results()$decisions$risk_off$level

    box_color <- switch(risk_color,
                        "GREEN" = "success",
                        "YELLOW" = "warning",
                        "RED" = "danger",
                        "info")

    div(
      class = paste("alert alert-", box_color, " text-center", sep = ""),
      style = "font-size: 24px; font-weight: bold; padding: 20px;",
      paste(risk_color, "-", risk_level)
    )
  })

  # Time horizon boxes
  output$short_term <- renderUI({
    req(results())
    signal <- results()$decisions$short_term$momentum$signal
    div(class = "well text-center",
        h4(signal),
        p("Momentum Score: ", results()$decisions$short_term$momentum$score)
    )
  })

  output$medium_term <- renderUI({
    req(results())
    signal <- results()$decisions$medium_term$trend$direction
    alloc <- results()$decisions$medium_term$position_sizing$recommended_allocation_pct
    div(class = "well text-center",
        h4(signal),
        p("Allocation: ", alloc, "%")
    )
  })

  output$long_term <- renderUI({
    req(results())
    signal <- results()$decisions$long_term$strategic$core_position
    div(class = "well text-center",
        h4(signal)
    )
  })

  # Summary text
  output$summary <- renderPrint({
    req(results())
    print_crypto_summary(results())
  })

  # MVRV plot
  output$mvrv_plot <- renderPlot({
    req(results())

    onchain <- results()$indicators$onchain

    onchain %>%
      tail(365) %>%
      ggplot(aes(x = timestamp, y = mvrv)) +
      geom_line(color = "steelblue", size = 1) +
      geom_hline(yintercept = c(0.7, 0.9, 1.75),
                 linetype = "dashed", color = "red", alpha = 0.5) +
      labs(title = "MVRV Ratio (Last 365 Days)",
           x = "Date", y = "MVRV") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # On-chain table
  output$onchain_table <- renderTable({
    req(results())

    oc <- results()$signals$onchain
    data.frame(
      Indicator = c("MVRV", "NVT", "Puell", "Composite"),
      Value = c(oc$mvrv$value, oc$nvt$value, oc$puell$value, "-"),
      Regime = c(oc$mvrv$regime, oc$nvt$regime, oc$puell$regime, oc$composite),
      Signal = c(oc$mvrv$signal, "-", "-", oc$composite)
    )
  })

  # Derivatives plot
  output$derivatives_plot <- renderPlot({
    req(results())

    deriv <- results()$raw_data$derivatives$data

    if (!is.null(deriv) && nrow(deriv) > 0) {
      deriv %>%
        tail(90) %>%
        ggplot(aes(x = date)) +
        geom_line(aes(y = open_interest/1e9, color = "Open Interest (B USD)")) +
        geom_line(aes(y = avg_funding_rate * 1000, color = "Funding Rate * 1000")) +
        labs(title = "Derivatives Market (Last 90 Days)",
             x = "Date", y = "Value") +
        scale_color_manual(values = c("Open Interest (B USD)" = "blue",
                                      "Funding Rate * 1000" = "red")) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })

  # Derivatives table
  output$derivatives_table <- renderTable({
    req(results())

    d <- results()$signals$derivatives
    if (!is.null(d)) {
      data.frame(
        Metric = c("Funding Regime", "OI Percentile", "Funding Percentile", "Interpretation"),
        Value = c(d$funding_regime,
                  paste0(round(d$oi_percentile * 100, 1), "%"),
                  paste0(round(d$funding_percentile * 100, 1), "%"),
                  d$interpretation)
      )
    }
  })

  # Macro plot
  output$macro_plot <- renderPlot({
    req(results())

    macro <- results()$raw_data$macro

    if (!is.null(macro)) {
      m2_data <- data.frame(
        date = macro$liquidity$m2_us$date,
        m2_yoy = macro$liquidity$m2_yoy * 100
      ) %>% tidyr::drop_na()

      ggplot(m2_data, aes(x = date, y = m2_yoy)) +
        geom_line(color = "darkgreen") +
        geom_hline(yintercept = c(0, 5), linetype = "dashed", color = "red") +
        labs(title = "M2 Money Supply YoY Growth",
             subtitle = ">5% = Expanding, <0% = Contracting",
             x = "Date", y = "YoY % Change") +
        theme_minimal()
    }
  })

  # Macro table
  output$macro_table <- renderTable({
    req(results())

    m <- results()$signals$macro
    if (!is.null(m)) {
      data.frame(
        Indicator = c("Liquidity", "Rates", "Dollar", "Composite"),
        Regime = c(m$liquidity_regime, m$rate_regime, m$dollar_regime, "-"),
        Value = c("-", "-", "-", round(m$composite_risk, 2))
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
