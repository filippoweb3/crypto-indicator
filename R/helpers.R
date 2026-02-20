#' Get Market Data
#'
#' @param assets Character vector of assets names (Bitcoin and Ethereum are default)
#' @param start_date Date for the start of data collection
#' @param include_eth Bool
#' @return List containing price data and metadata
#' @export
get_market_data <- function(assets = c("Bitcoin"), start_date = "2015-01-01"){

  coin_list <- crypto2::crypto_list() %>% dplyr::filter(name %in% assets)

  data <- crypto2::crypto_history(coin_list = coin_list, start_date = start_date)

  return(
    list(data = data,
         assets = assets,
         start_date = start_date,
         last_update = Sys.Date()
         )
    )

}

get_btc_issuance <- function(target_date) {

  halvings <- data.frame(
    date = as.Date(c("2009-01-03", "2012-11-28", "2016-07-09", "2020-05-11", "2024-04-20")),
    reward = c(50, 25, 12.5, 6.25, 3.125)
  )

  current_reward <- halvings %>%
    filter(date <= target_date) %>%
    slice_tail(n = 1) %>%
    pull(reward)

  return(current_reward * 144)

}

get_onchain_indicators <- function(data){

  data <- dplyr::group_by(data, slug) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      returns = (close / dplyr::lag(close, 1)) - 1,
      log_returns = log(close) - log(dplyr::lag(close, 1)),
      volume_ma_7 = zoo::rollmean(volume, 7, fill = NA, align = "right"),
      price_ma_50 = zoo::rollmean(close, 50, fill = NA, align = "right"),
      price_ma_200 = zoo::rollmean(close, 200, fill = NA, align = "right"),
      nvt = market_cap / volume,
      nvts = market_cap / TTR::SMA(volume, n = 14),
      realized_price = TTR::SMA(close, n = 200), # proxy use 200 MA, normally would use UTXO data
      mvrv = close / realized_price,
      issuance = sapply(timestamp, function(x) get_btc_issuance(x)),
      issuance_usd = issuance*close,
      puell_multiple = issuance_usd/zoo::rollmean(issuance_usd, k = 365, fill = NA, align = "right"),

      # Signals
      nvt_regime = ifelse(nvt > 70, "Overvalued",
                          ifelse(nvt < 30, "Undervalued", "Neutral")),
      mvrv_regime = ifelse(mvrv > 1.75, "Extreme Top",
                           ifelse(mvrv < 0.7, "Extreme Bottom",
                                  ifelse(mvrv < 0.9 & mvrv >= 0.7, "Bear Market", "Bull Market"))),
      puell_signal = ifelse(puell_multiple > 2, "Miner Selling Pressure",
                            ifelse(puell_multiple < 0.7, "Miner Capitulation", "Neutral")),
      regime_combi = case_when(
        mvrv_regime == "Extreme Bottom" & puell_signal == "Miner Capitulation" ~ "Strong Buy",
        mvrv_regime == "Extreme Top" & puell_signal == "Miner Selling Pressure" ~ "Strong Sell",
        TRUE ~ "Neutral")
      )

  return(data)

}

get_derivatives <- function(ticker = "BTCUSDT") {

  # open interest
  oi <- cryptoQuotes::get_openinterest(ticker = ticker, source = "binance", interval = "1d")
  oi <- data.frame(date = as.Date(as.POSIXct(attributes(oi)$index)),
                   open_interest = oi$open_interest)


  # funding rate
  funding <- cryptoQuotes::get_fundingrate(ticker = ticker, source = "binance")
  funding <- data.frame(date = as.Date(as.POSIXct(attributes(funding)$index)),
                        funding_rate = funding$funding_rate)

  funding <- group_by(funding, date) %>%
    summarise(
      avg_funding_rate = mean(funding_rate, na.rm = TRUE),
      max_funding_rate = max(funding_rate, na.rm = TRUE)
    )

  # long/short ratio
  ls_ratio <- cryptoQuotes::get_lsratio(ticker = ticker, source = "Binance", interval = "1d")
  ls_ratio <- data.frame(date = as.Date(as.POSIXct(attributes(ls_ratio)$index)),
                         long = ls_ratio$long,
                         short = ls_ratio$short,
                         ls_ratio = ls_ratio$ls_ratio)

  derivatives_data <- dplyr::full_join(oi, funding, by = "date") %>%
    dplyr::full_join(ls_ratio, by = "date") %>%
    dplyr::arrange(date)

  oi_percentile <- rank(derivatives_data$open_interest) / nrow(derivatives_data)
  funding_percentile <- rank(derivatives_data$avg_funding_rate) / nrow(derivatives_data)

  # Signals

  signals <- list(
    # High OI with high funding suggests crowded long (potential reversal)
    crowded_long = oi_percentile > 0.9 & funding_percentile > 0.9,
    # Low OI with negative funding suggests capitulation (potential bottom)
    crowded_short = oi_percentile < 0.1 & funding_percentile < 0.1,
    # Open interest momentum
    oi_momentum = (derivatives_data$open_interest /
                     dplyr::lag(derivatives_data$open_interest, 7)) - 1,
    # Funding rate regime
    funding_regime = case_when(
      derivatives_data$avg_funding_rate*100 > 0.05 ~ "Extreme Long",
      derivatives_data$avg_funding_rate*100 > 0.01 ~ "Bullish",
      derivatives_data$avg_funding_rate*100 >= 0.00  ~ "Neutral",
      derivatives_data$avg_funding_rate*100 > -0.01  ~ "Bearish",
      derivatives_data$avg_funding_rate*100 <= -0.01  ~ "Extreme Short",
      TRUE ~ "Unknown"
    )
  )

  return(list(data = derivatives_data,
              oi_percentile = oi_percentile,
              funding_percentile = funding_percentile,
              signals = signals
              )
         )

}

get_macro <- function(fred_api_key = global_variables$fred_api_key, start_date = "2015-01-01"){

  fredr::fredr_set_key(fred_api_key)

  # Global Liquidity (M2 for major economies)
  m2_us <- fredr::fredr(
    series_id = "M2SL",
    observation_start = as.Date(start_date),
    observation_end = Sys.Date()
  )

  m2_yoy <- (m2_us$value / dplyr::lag(m2_us$value, 12)) - 1
  m2_mom <- (m2_us$value / dplyr::lag(m2_us$value, 1)) - 1

  liquidity_regime <- ifelse(
    tail(m2_yoy, 1) > 0.05, "Expanding",
    ifelse(tail(m2_yoy, 1) < 0, "Contracting", "Stable")
  )

  # US Dollar Index (DXY) - Inverse correlation with crypto
  dxy <- tryCatch({
    quantmod::getSymbols("DX-Y.NYB", src = "yahoo", auto.assign = FALSE)
  }, error = function(e) NULL)

  dollar_regime <- ifelse(
    !is.null(dxy) && tail(quantmod::Cl(dxy), 1) > 100, "Strong Dollar (Bearish Crypto)",
    ifelse(!is.null(dxy) && tail(quantmod::Cl(dxy), 1) < 90, "Weak Dollar (Bullish Crypto)", "Neutral")
  )

  # 10-Year Treasury Yield - Risk-free rate competition
  treasury_10y <- fredr::fredr(
    series_id = "DGS10",
    observation_start = as.Date(start_date),
    observation_end = Sys.Date()
  )

  # Inflation expectations
  inflation_expect <- fredr::fredr(
    series_id = "T5YIE",
    observation_start = as.Date(start_date),
    observation_end = Sys.Date()
  )

  aggregated <- full_join(treasury_10y, inflation_expect, by ="date")
  aggregated <- aggregated[,c(1,3,7)]
  colnames(aggregated) <- c("date", "DGS10", "T5YIE")
  aggregated$real_rates <- aggregated$DGS10 - aggregated$T5YIE

  rate_regime = ifelse(
    tail(treasury_10y$value, 1) > 3, "High Rates (Risk-Off)",
    ifelse(tail(treasury_10y$value, 1) < 1, "Low Rates (Risk-On)", "Neutral")
  )

  composite_risk_score = (
    ifelse(tail(m2_yoy, 1) > 0.05, 1, ifelse(tail(m2_yoy, 1) < 0, -1, 0)) +
      ifelse(tail(treasury_10y$value, 1) > 3, -1, ifelse(tail(treasury_10y$value, 1) < 1, 1, 0)) +
      ifelse(!is.null(dxy) && tail(quantmod::Cl(dxy), 1) > 100, -1,
             ifelse(!is.null(dxy) && tail(quantmod::Cl(dxy), 1) < 90, 1, 0))
  ) / 3

  return(
    list(
      liquidity = list(
        m2_us = m2_us,
        m2_yoy = m2_yoy,
        m2_mom = m2_mom,
        liquidity_regime = liquidity_regime
        ),
      rates = list(
        treasury_10y = treasury_10y,
        real_rates = aggregated$real_rates,
        rate_regime = rate_regime
      ),
      dollar = list(
        dxy = dxy,
        dollar_regime = dollar_regime
        ),
      composite_risk_score = composite_risk_score
      )
    )

}

get_volatility <- function(data, windows = list(short = 7, medium = 30, long = 90)) {

  returns <- data$log_returns

  # Realized volatility for different horizons
  vol_short <- TTR::runSD(returns, n = windows$short) * sqrt(365)
  vol_medium <- TTR::runSD(returns, n = windows$medium) * sqrt(365)
  vol_long <- TTR::runSD(returns, n = windows$long) * sqrt(365)

  # Volatility percentiles
  vol_percentile_short <- rank(vol_short) / length(vol_short)
  vol_percentile_medium <- rank(vol_medium) / length(vol_medium)

  # GARCH-style volatility clustering
  vol_regime_short <- ifelse(
    vol_short > mean(vol_short, na.rm = TRUE) + sd(vol_short, na.rm = TRUE), "Extreme High",
    ifelse(vol_short < mean(vol_short, na.rm = TRUE) - sd(vol_short, na.rm = TRUE), "Extreme Low",
           ifelse(vol_short > mean(vol_short, na.rm = TRUE), "High", "Low")
    )
  )

  # Volatility signals
  signals <- list(
    # Volatility breakout (momentum strategy)
    vol_breakout = vol_short > vol_medium * 1.5,
    # Volatility mean reversion (range-bound strategy)
    vol_compression = vol_short < vol_medium * 0.5,
    # Regime detection
    current_regime = switch(vol_regime_short[length(vol_regime_short)],
                            "Extreme High" = "Risk-Off (Capitulation)",
                            "High" = "Active Trading",
                            "Low" = "Trend Following",
                            "Extreme Low" = "Complacency (Watch for Breakout)")
  )

  return(list(
    volatility = list(
      short = vol_short,
      medium = vol_medium,
      long = vol_long,
      percentile_short = vol_percentile_short,
      percentile_medium = vol_percentile_medium
    ),
    regime = vol_regime_short,
    signals = signals
  ))

}

get_positioning <- function(api_key = NULL, start_time = "2026-02-01", google_trends_keywords = c("bitcoin", "crypto", "blockchain")){

  start_time <- as.numeric(as.POSIXct(start_time))

  url <- paste0("https://api.whale-alert.io/v1/transactions",
                "?api_key=", global_variables$whaleAlert_api_key,
                "&start=", start_time)

  response <- httr::GET(url)
  raw_data <- jsonlite::fromJSON(httr::content(response, "text"))

  if (is.null(raw_data$transactions)) {
    return("No whale transactions found in the specified window.")
  }

  whale_df <- as.data.frame(raw_data$transactions) %>%
    mutate(
      from_type = from$owner_type,
      to_type = to$owner_type,
      from_owner = from$owner,
      to_owner = to$owner
    )

  metrics <- whale_df %>%
    summarise(
      inflow_usd  = sum(amount_usd[from_type == "unknown" & to_type == "exchange"], na.rm = TRUE),
      outflow_usd = sum(amount_usd[from_type == "exchange" & to_type == "unknown"], na.rm = TRUE),
      tx_count    = n(),
      pct = sum(amount_usd > max(whale_df$amount_usd)/2)
    ) %>%
    mutate(netflow_usd = inflow_usd - outflow_usd)

  flow_regime <- case_when(
    metrics$netflow_usd > (max(whale_df$amount_usd) * 10)  ~ "Extreme Inflows (Heavy Selling Pressure)",
    metrics$netflow_usd > 0                     ~ "Net Inflows (Bearish Bias)",
    metrics$netflow_usd < -(max(whale_df$amount_usd) * 10) ~ "Extreme Outflows (Heavy Accumulation)",
    metrics$netflow_usd < 0                     ~ "Net Outflows (Bullish Bias)",
    TRUE                                        ~ "Neutral / Balanced"
  )

  whale_signal <- case_when(
    metrics$pct > 50  ~ "High Whale Intensity (Volality Expected)",
    metrics$pct < 10   ~ "Low Whale Activity",
    TRUE                   ~ "Normal Whale Activity"
  )

  google_trends_data <- NULL
  sentiment_score <- NULL
  sentiment_signal <- NULL

  tryCatch({
    # Fetch Google Trends data
    trends <- gtrendsR::gtrends(
      keyword = google_trends_keywords,
      geo = "",  # Worldwide
      time = "today 3-m",  # Last 90 days
      gprop = "web"
    )

    # Extract interest over time
    interest_data <- trends$interest_over_time

    # Calculate average sentiment score (0-100 scale)
    recent_interest <- interest_data[interest_data$date > Sys.Date() - 7, ]
    sentiment_score <- mean(recent_interest$hits, na.rm = TRUE)

    # Generate sentiment signal
    sentiment_signal <- ifelse(
      sentiment_score > 80, "Extreme Interest (Potential Top)",
      ifelse(sentiment_score < 20, "Low Interest (Potential Bottom)",
             ifelse(sentiment_score > 60, "High Interest", "Normal Interest"))
    )

    google_trends_data <- list(
      interest_over_time = interest_data,
      related_queries = trends$related_queries,
      interest_by_region = trends$interest_by_region,
      current_sentiment = sentiment_score,
      sentiment_signal = sentiment_signal,
      keywords = google_trends_keywords
    )

  }, error = function(e) {
    warning("Failed to fetch Google Trends data: ", e$message)
  })


  return(list(
    exchange_positioning = list(
      netflow_summary = metrics,
      flow_regime = flow_regime,
      top_exchange_target = head(whale_df$to_owner[whale_df$to_type == "exchange"], 1)
    ),
    whale_activity = list(
      total_tx = metrics$tx_count,
      whale_signal = whale_signal,
      largest_move_usd = max(whale_df$amount_usd)
    ),
    raw_whale_data = whale_df, # Useful for plotting individual bubbles later
    google_trends = google_trends_data
  ))

}



#' Comprehensive Crypto Predictive Framework
#'
#' A single function that combines on-chain, derivatives, macro, volatility, and positioning
#' indicators into actionable signals across different time horizons.
#'
#' @param assets Character vector of assets (default: c("Bitcoin"))
#' @param start_date Start date for historical data (default: "2015-01-01")
#' @param fred_api_key FRED API key for macro data (optional)
#' @param whale_api_key Whale Alert API key for whale transactions (optional)
#' @param include_google_trends Boolean to include Google Trends sentiment (default: FALSE)
#' @return List containing comprehensive framework with signals by time horizon
#' @export
crypto_predictive_framework <- function(assets = c("Bitcoin"),
                                        start_date = "2015-01-01",
                                        fred_api_key = NULL,
                                        whale_api_key = NULL,
                                        include_google_trends = TRUE) {

  #---------------------------------------------------------------------------
  # INITIALIZATION
  #---------------------------------------------------------------------------
  framework <- list(
    metadata = list(
      timestamp = Sys.time(),
      assets = assets,
      start_date = start_date,
      end_date = Sys.Date(),
      status = "pending"
    ),
    warnings = character(),
    errors = character(),
    raw_data = list(),
    indicators = list(),
    signals = list(),
    decisions = list(),
    summary = list()
  )

  # Set API keys if provided
  if (!is.null(fred_api_key)) {
    assign("fred_api_key", fred_api_key, envir = .GlobalEnv)
  }
  if (!is.null(whale_api_key)) {
    assign("whaleAlert_api_key", whale_api_key, envir = .GlobalEnv)
  }

  #---------------------------------------------------------------------------
  # MARKET DATA
  #---------------------------------------------------------------------------
  message("📊 Collecting market data...")

  tryCatch({
    market_data <- get_market_data(assets = assets, start_date = start_date)
    framework$raw_data$market <- market_data

    # Get Bitcoin data (assuming first asset is Bitcoin)
    btc_data <- market_data$data %>%
      dplyr::filter(slug == tolower(assets[1]))

    if (nrow(btc_data) == 0) {
      stop("No data returned for the specified asset")
    }

    message("  ✅ Retrieved ", nrow(btc_data), " days of data")

  }, error = function(e) {
    framework$errors <- c(framework$errors, paste("Market data:", e$message))
    framework$metadata$status <- "failed"
    return(framework)
  })

  #---------------------------------------------------------------------------
  # ON-CHAIN INDICATORS
  #---------------------------------------------------------------------------
  message("⛓️  Calculating on-chain indicators...")

  tryCatch({
    # Add market cap column if not present
    if (!"market_cap" %in% names(btc_data)) {
      # Approximate BTC supply (simplified)
      btc_data$market_cap <- btc_data$close * 19e6
    }

    onchain_data <- get_onchain_indicators(btc_data)
    framework$indicators$onchain <- onchain_data

    # Extract latest on-chain signals
    latest_onchain <- onchain_data %>%
      dplyr::slice_tail(n = 1)

    framework$signals$onchain <- list(
      mvrv = list(
        value = as.numeric(latest_onchain$mvrv),
        regime = as.character(latest_onchain$mvrv_regime),
        signal = dplyr::case_when(
          latest_onchain$mvrv_regime == "Extreme Bottom" ~ "Strong Buy",
          latest_onchain$mvrv_regime == "Bear Market" ~ "Accumulate",
          latest_onchain$mvrv_regime == "Bull Market" ~ "Hold",
          latest_onchain$mvrv_regime == "Extreme Top" ~ "Strong Sell",
          TRUE ~ "Neutral"
        )
      ),
      nvt = list(
        value = as.numeric(latest_onchain$nvt),
        regime = as.character(latest_onchain$nvt_regime)
      ),
      puell = list(
        value = as.numeric(latest_onchain$puell_multiple),
        regime = as.character(latest_onchain$puell_signal)
      ),
      composite = as.character(latest_onchain$regime_combi)
    )

    message("  ✅ MVRV: ", framework$signals$onchain$mvrv$regime)

  }, error = function(e) {
    framework$errors <- c(framework$errors, paste("On-chain:", e$message))
  })

  #---------------------------------------------------------------------------
  # DERIVATIVES INDICATORS
  #---------------------------------------------------------------------------
  message("📈 Fetching derivatives data...")

  ticker <- "BTCUSDT"
  message("  Ticker: ", ticker)

  tryCatch({
    deriv_result <- get_derivatives(ticker = ticker)
    framework$raw_data$derivatives <- deriv_result

    if (!is.null(deriv_result$data) && nrow(deriv_result$data) > 0) {

      # Get latest values
      signals_list <- deriv_result$signals

      framework$signals$derivatives <- list(
        funding_regime = if (length(signals_list$funding_regime) > 0) {
          as.character(tail(signals_list$funding_regime, 1))
        } else {
          "Unknown"
        },
        crowded_long = if (length(signals_list$crowded_long) > 0) {
          as.logical(tail(signals_list$crowded_long, 1))
        } else {
          FALSE
        },
        crowded_short = if (length(signals_list$crowded_short) > 0) {
          as.logical(tail(signals_list$crowded_short, 1))
        } else {
          FALSE
        },
        oi_momentum = if (length(signals_list$oi_momentum) > 0) {
          as.numeric(tail(signals_list$oi_momentum, 1))
        } else {
          NA
        },
        oi_percentile = if (length(deriv_result$oi_percentile) > 0) {
          as.numeric(tail(deriv_result$oi_percentile, 1))
        } else {
          NA
        },
        funding_percentile = if (length(deriv_result$funding_percentile) > 0) {
          as.numeric(tail(deriv_result$funding_percentile, 1))
        } else {
          NA
        },
        interpretation = {
          regime <- if (length(signals_list$funding_regime) > 0) {
            tail(signals_list$funding_regime, 1)
          } else {
            "Unknown"
          }
          crowded_l <- if (length(signals_list$crowded_long) > 0) {
            tail(signals_list$crowded_long, 1)
          } else {
            FALSE
          }
          crowded_s <- if (length(signals_list$crowded_short) > 0) {
            tail(signals_list$crowded_short, 1)
          } else {
            FALSE
          }

          if (crowded_l) {
            "Extreme long positioning - reversal risk"
          } else if (crowded_s) {
            "Extreme short positioning - bounce risk"
          } else if (regime == "Extreme Long") {
            "Speculative excess - cautious"
          } else if (regime == "Extreme Short") {
            "Capitulation - watch for reversal"
          } else if (regime == "Bullish") {
            "Healthy long positioning"
          } else if (regime == "Bearish") {
            "Cautious short positioning"
          } else {
            "Normal derivatives market"
          }
        }
      )

      message("  ✅ Funding regime: ", framework$signals$derivatives$funding_regime)

    } else {
      framework$warnings <- c(framework$warnings, "Derivatives data returned but empty")
    }

  }, error = function(e) {
    framework$warnings <- c(framework$warnings, paste("Derivatives:", e$message))
    framework$raw_data$derivatives <- NULL
  })

  #---------------------------------------------------------------------------
  # MACRO INDICATORS
  #---------------------------------------------------------------------------
  message("🌎 Fetching macro indicators...")

  tryCatch({
    macro_data <- get_macro(start_date = start_date)
    framework$raw_data$macro <- macro_data

    framework$signals$macro <- list(
      liquidity_regime = macro_data$liquidity$liquidity_regime,
      rate_regime = macro_data$rates$rate_regime,
      dollar_regime = macro_data$dollar$dollar_regime,
      composite_risk = as.numeric(macro_data$composite_risk_score),
      risk_bias = ifelse(macro_data$composite_risk_score > 0.3, "Bullish",
                         ifelse(macro_data$composite_risk_score < -0.3, "Bearish", "Neutral"))
    )

    message("  ✅ Liquidity: ", macro_data$liquidity$liquidity_regime)

  }, error = function(e) {
    framework$warnings <- c(framework$warnings, paste("Macro:", e$message))
    framework$raw_data$macro <- NULL
  })

  #---------------------------------------------------------------------------
  # VOLATILITY INDICATORS
  #---------------------------------------------------------------------------
  message("📊 Calculating volatility metrics...")

  tryCatch({
    if (!is.null(framework$indicators$onchain)) {
      vol_data <- get_volatility(data = framework$indicators$onchain)
      framework$indicators$volatility <- vol_data

      framework$signals$volatility <- list(
        regime = tail(vol_data$regime, 1),
        current_regime = vol_data$signals$current_regime,
        percentile_short = tail(vol_data$volatility$percentile_short, 1),
        percentile_medium = tail(vol_data$volatility$percentile_medium, 1),
        strategy = dplyr::case_when(
          tail(vol_data$signals$vol_compression, 1) ~ "Mean Reversion Setup",
          tail(vol_data$signals$vol_breakout, 1) ~ "Momentum Setup",
          TRUE ~ "No Clear Volatility Signal"
        )
      )

      message("  ✅ Volatility regime: ", framework$signals$volatility$regime)
    }

  }, error = function(e) {
    framework$warnings <- c(framework$warnings, paste("Volatility:", e$message))
  })

  #---------------------------------------------------------------------------
  # POSITIONING INDICATORS (Optional)
  #---------------------------------------------------------------------------
  if (!is.null(whale_api_key) || include_google_trends) {
    message("🐋 Fetching whale and sentiment data...")

    tryCatch({
      pos_data <- get_positioning(
        start_time = as.character(Sys.Date() - 7),
        google_trends_keywords = c("bitcoin", "crypto", "blockchain")
      )

      if (is.list(pos_data) && !is.character(pos_data)) {
        framework$raw_data$positioning <- pos_data

        framework$signals$positioning <- list(
          whale_flow = pos_data$exchange_positioning$flow_regime,
          whale_activity = pos_data$whale_activity$whale_signal,
          whale_interpretation = dplyr::case_when(
            grepl("Extreme Inflows", pos_data$exchange_positioning$flow_regime) ~ "Selling pressure",
            grepl("Extreme Outflows", pos_data$exchange_positioning$flow_regime) ~ "Accumulation",
            grepl("Net Inflows", pos_data$exchange_positioning$flow_regime) ~ "Mild bearish",
            grepl("Net Outflows", pos_data$exchange_positioning$flow_regime) ~ "Mild bullish",
            TRUE ~ "Neutral"
          )
        )

        # Add sentiment if available
        if (!is.null(pos_data$google_trends) && include_google_trends) {
          framework$signals$positioning$sentiment <- list(
            score = pos_data$google_trends$current_sentiment,
            signal = pos_data$google_trends$sentiment_signal
          )
        }

        message("  ✅ Whale flow: ", pos_data$exchange_positioning$flow_regime)
      } else {
        framework$warnings <- c(framework$warnings, "No whale transactions found")
      }

    }, error = function(e) {
      framework$warnings <- c(framework$warnings, paste("Positioning:", e$message))
    })
  }

  #---------------------------------------------------------------------------
  # DECISION FRAMEWORK
  #---------------------------------------------------------------------------
  message("🎯 Generating decision framework...")

  # SHORT-TERM (hours to days)
  framework$decisions$short_term <- list()

  momentum_score <- 0

  # Derivatives contribution
  if (!is.null(framework$signals$derivatives)) {
    if (framework$signals$derivatives$funding_regime %in% c("Bullish", "Neutral")) {
      momentum_score <- momentum_score + 1
    } else if (framework$signals$derivatives$funding_regime %in% c("Bearish", "Extreme Short")) {
      momentum_score <- momentum_score - 1
    }
  }

  # Price momentum
  if (!is.null(framework$indicators$onchain)) {
    recent_returns <- tail(framework$indicators$onchain$returns, 3)
    if (mean(recent_returns, na.rm = TRUE) > 0) {
      momentum_score <- momentum_score + 1
    } else {
      momentum_score <- momentum_score - 1
    }
  }

  framework$decisions$short_term$momentum <- list(
    score = momentum_score,
    signal = dplyr::case_when(
      momentum_score >= 2 ~ "Strong Long",
      momentum_score >= 1 ~ "Long",
      momentum_score <= -2 ~ "Strong Short",
      momentum_score <= -1 ~ "Short",
      TRUE ~ "Neutral"
    )
  )

  # Mean reversion
  framework$decisions$short_term$mean_reversion <- list(
    active = !is.null(framework$signals$volatility$strategy) &&
      grepl("Mean Reversion", framework$signals$volatility$strategy),
    direction = ifelse(!is.null(framework$indicators$onchain) &&
                         tail(framework$indicators$onchain$returns, 1) < -0.03,
                       "Oversold - Long",
                       ifelse(!is.null(framework$indicators$onchain) &&
                                tail(framework$indicators$onchain$returns, 1) > 0.03,
                              "Overbought - Short", "Neutral"))
  )

  # MEDIUM-TERM (weeks to months)
  framework$decisions$medium_term <- list()

  trend_score <- 0

  # On-chain contribution
  if (!is.null(framework$signals$onchain$mvrv$regime)) {
    if (framework$signals$onchain$mvrv$regime %in% c("Extreme Bottom", "Bear Market")) {
      trend_score <- trend_score + 2
    } else if (framework$signals$onchain$mvrv$regime == "Bull Market") {
      trend_score <- trend_score + 1
    } else if (framework$signals$onchain$mvrv$regime == "Extreme Top") {
      trend_score <- trend_score - 2
    }
  }

  # Macro contribution
  if (!is.null(framework$signals$macro$composite_risk)) {
    trend_score <- trend_score + round(framework$signals$macro$composite_risk * 2)
  }

  framework$decisions$medium_term$trend <- list(
    score = trend_score,
    direction = dplyr::case_when(
      trend_score >= 2 ~ "Strong Bull",
      trend_score >= 1 ~ "Bull",
      trend_score <= -2 ~ "Strong Bear",
      trend_score <= -1 ~ "Bear",
      TRUE ~ "Neutral"
    )
  )

  # Valuation composite
  framework$decisions$medium_term$valuation <- list(
    composite = if (!is.null(framework$signals$onchain$composite)) {
      framework$signals$onchain$composite
    } else {
      "Neutral"
    }
  )

  # Position sizing
  allocation <- 50
  if (trend_score >= 2) {
    allocation <- 75
  } else if (trend_score >= 1) {
    allocation <- 65
  } else if (trend_score <= -2) {
    allocation <- 25
  } else if (trend_score <= -1) {
    allocation <- 35
  }

  framework$decisions$medium_term$position_sizing <- list(
    recommended_allocation_pct = allocation,
    risk_per_trade_pct = ifelse(allocation > 60, 1.0, 1.5)
  )

  # LONG-TERM (months to years)
  framework$decisions$long_term <- list()

  # Cycle positioning
  framework$decisions$long_term$cycle <- list(
    mvrv_phase = if (!is.null(framework$signals$onchain$mvrv$regime)) {
      framework$signals$onchain$mvrv$regime
    } else {
      "Unknown"
    },
    interpretation = dplyr::case_when(
      !is.null(framework$signals$onchain$mvrv$regime) &&
        framework$signals$onchain$mvrv$regime == "Extreme Bottom" ~
        "Historical accumulation zone - strong long-term buy",
      !is.null(framework$signals$onchain$mvrv$regime) &&
        framework$signals$onchain$mvrv$regime == "Extreme Top" ~
        "Historical distribution zone - consider reducing exposure",
      TRUE ~ "Mid-cycle"
    )
  )

  # Halving impact
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years_since_halving <- current_year %% 4

  framework$decisions$long_term$halving <- list(
    phase = dplyr::case_when(
      years_since_halving == 0 ~ "Halving Year - Pre-halving accumulation",
      years_since_halving == 1 ~ "Post-halving - Historically strongest",
      years_since_halving == 2 ~ "Mid-cycle - Continued strength",
      years_since_halving == 3 ~ "Pre-halving year - Potential bottom"
    )
  )

  # Strategic allocation
  framework$decisions$long_term$strategic <- list(
    core_position = ifelse(
      !is.null(framework$signals$onchain$mvrv$regime) &&
        framework$signals$onchain$mvrv$regime %in% c("Extreme Bottom", "Bear Market"),
      "Accumulate core position",
      ifelse(!is.null(framework$signals$onchain$mvrv$regime) &&
               framework$signals$onchain$mvrv$regime == "Extreme Top",
             "Reduce core position", "Maintain core position")
    )
  )

  #---------------------------------------------------------------------------
  # RISK-OFF SIGNALS
  #---------------------------------------------------------------------------
  risk_level <- "LOW"
  risk_factors <- c()

  # Volatility check
  if (!is.null(framework$signals$volatility$regime)) {
    if (grepl("Extreme High", framework$signals$volatility$regime)) {
      risk_level <- "HIGH"
      risk_factors <- c(risk_factors, "Extreme volatility - panic conditions")
    } else if (grepl("High", framework$signals$volatility$regime)) {
      risk_level <- "MEDIUM"
      risk_factors <- c(risk_factors, "Elevated volatility")
    }
  }

  # Macro check
  if (!is.null(framework$signals$macro$composite_risk)) {
    if (framework$signals$macro$composite_risk < -0.3) {
      if (risk_level != "HIGH") risk_level <- "MEDIUM"
      risk_factors <- c(risk_factors, "Negative macro composite")
    }
    if (framework$signals$macro$composite_risk < -0.5) {
      risk_level <- "HIGH"
      risk_factors <- c(risk_factors, "Severe macro headwinds")
    }
  }

  # Derivatives check
  if (!is.null(framework$signals$derivatives$crowded_long) &&
      framework$signals$derivatives$crowded_long) {
    risk_level <- "MEDIUM"
    risk_factors <- c(risk_factors, "Extremely crowded long positioning")
  }

  actions <- list(
    LOW = c("Maintain full positions", "Normal stop losses"),
    MEDIUM = c("Reduce leverage", "Tighten stops", "Take partial profits"),
    HIGH = c("Move to stablecoins", "Close leveraged positions", "Wait for stabilization")
  )

  framework$decisions$risk_off <- list(
    level = risk_level,
    color = ifelse(risk_level == "LOW", "GREEN",
                   ifelse(risk_level == "MEDIUM", "YELLOW", "RED")),
    factors = risk_factors,
    actions = actions[[risk_level]]
  )

  #---------------------------------------------------------------------------
  # REGIME ALLOCATION
  #---------------------------------------------------------------------------
  regime <- "Neutral"

  if ((!is.null(framework$signals$macro$liquidity_regime) &&
       framework$signals$macro$liquidity_regime == "Expanding") ||
      (!is.null(framework$signals$onchain$mvrv$regime) &&
       framework$signals$onchain$mvrv$regime == "Bull Market")) {
    regime <- "Bull"
  }

  if ((!is.null(framework$signals$macro$liquidity_regime) &&
       framework$signals$macro$liquidity_regime == "Contracting") ||
      (!is.null(framework$signals$onchain$mvrv$regime) &&
       framework$signals$onchain$mvrv$regime %in% c("Bear Market", "Extreme Bottom"))) {
    regime <- "Bear"
  }

  if (!is.null(framework$signals$onchain$mvrv$regime) &&
      framework$signals$onchain$mvrv$regime == "Extreme Top") {
    regime <- "Distribution"
  }

  if (!is.null(framework$signals$onchain$mvrv$regime) &&
      framework$signals$onchain$mvrv$regime == "Extreme Bottom") {
    regime <- "Accumulation"
  }

  allocation_map <- list(
    Bull = list(strategic = "75-100%", tactical = "Trend following", hedges = "Minimal"),
    Bear = list(strategic = "25-50%", tactical = "Mean reversion", hedges = "Consider hedges"),
    Accumulation = list(strategic = "50-75% DCA", tactical = "Scale in", hedges = "Light"),
    Distribution = list(strategic = "<25%", tactical = "Take profits", hedges = "Increase"),
    Neutral = list(strategic = "50%", tactical = "Balanced", hedges = "Standard")
  )

  framework$decisions$regime_allocation <- list(
    current_regime = regime,
    allocation = allocation_map[[regime]]
  )

  #---------------------------------------------------------------------------
  # ACTIONABLE SUMMARY
  #---------------------------------------------------------------------------
  framework$summary <- list(
    timestamp = Sys.time(),
    risk = framework$decisions$risk_off$color,
    short_term = framework$decisions$short_term$momentum$signal,
    medium_term = framework$decisions$medium_term$trend$direction,
    long_term = framework$decisions$long_term$strategic$core_position,
    regime = regime,
    allocation = framework$decisions$regime_allocation$allocation$strategic,
    bullet_points = c(
      paste0("RISK: ", framework$decisions$risk_off$color, " - ",
             framework$decisions$risk_off$actions[1]),
      paste0("STRATEGY: ", framework$decisions$regime_allocation$allocation$tactical),
      paste0("ALLOCATION: ", framework$decisions$regime_allocation$allocation$strategic, " exposure")
    )
  )

  # Update metadata
  framework$metadata$status <- "success"
  framework$metadata$warning_count <- length(framework$warnings)
  framework$metadata$error_count <- length(framework$errors)

  # Set class for pretty printing
  class(framework) <- "crypto_framework"

  message("\n✅ Framework complete! Use print_crypto_summary() to view results")

  return(framework)
}






#' Print comprehensive crypto framework summary
#'
#' @param x The result object from crypto_predictive_framework()
#' @return None, prints to console
print_crypto_summary <- function(x) {

  # Helper function to safely format dates
  safe_format_date <- function(date_val) {
    tryCatch({
      if (inherits(date_val, "Date")) {
        format(date_val, "%Y-%m-%d")
      } else if (is.character(date_val)) {
        # Try to convert character to Date
        date_val <- as.Date(date_val)
        format(date_val, "%Y-%m-%d")
      } else {
        as.character(date_val)
      }
    }, error = function(e) {
      as.character(date_val)
    })
  }

  # Helper function to safely extract values
  safe_extract <- function(lst, path, default = "N/A") {
    tryCatch({
      value <- lst
      for (p in path) {
        if (is.null(value[[p]])) return(default)
        value <- value[[p]]
      }
      if (is.null(value)) return(default)
      if (is.numeric(value)) {
        return(sprintf("%.2f", value))
      }
      return(as.character(value))
    }, error = function(e) {
      return(default)
    })
  }

  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("🚀 CRYPTO PREDICTIVE FRAMEWORK - COMPREHENSIVE SUMMARY\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  # Safe metadata extraction
  cat("Generated:", safe_extract(x, c("metadata", "timestamp")), "\n")
  cat("Asset:", paste(safe_extract(x, c("metadata", "assets"), "Bitcoin"), collapse = ", "), "\n")

  # Handle dates safely
  start_date <- safe_extract(x, c("metadata", "start_date"), "2015-01-01")
  end_date <- safe_extract(x, c("metadata", "end_date"), safe_format_date(Sys.Date()))
  cat("Data Range:", start_date, "to", end_date, "\n")

  cat("Status:", toupper(safe_extract(x, c("metadata", "status"), "unknown")), "\n")

  # Warnings
  if (length(x$warnings) > 0) {
    cat("\n⚠️  WARNINGS:\n")
    for (w in x$warnings) cat("  •", w, "\n")
  }

  # Errors
  if (length(x$errors) > 0) {
    cat("\n❌ ERRORS:\n")
    for (e in x$errors) cat("  •", e, "\n")
  }

  if (x$metadata$status == "success") {

    #-------------------------------------------------------------------------
    # RISK-OFF SIGNAL
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    risk_color <- safe_extract(x, c("decisions", "risk_off", "color"), "UNKNOWN")
    risk_emoji <- ifelse(risk_color == "GREEN", "🟢",
                         ifelse(risk_color == "YELLOW", "🟡",
                                ifelse(risk_color == "RED", "🔴", "⚪")))
    cat(risk_emoji, " RISK-OFF SIGNAL: [", risk_color, "] ", risk_emoji, "\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    risk_factors <- x$decisions$risk_off$factors
    if (length(risk_factors) > 0 && !is.null(risk_factors)) {
      cat("Risk Factors:\n")
      for (factor in risk_factors) cat("  •", factor, "\n")
    } else {
      cat("  • No significant risk factors detected\n")
    }

    cat("\nRecommended Actions:\n")
    risk_actions <- x$decisions$risk_off$actions
    if (length(risk_actions) > 0 && !is.null(risk_actions)) {
      for (action in risk_actions) cat("  →", action, "\n")
    }

    #-------------------------------------------------------------------------
    # ON-CHAIN INDICATORS
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("⛓️  ON-CHAIN INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    if (!is.null(x$signals$onchain)) {
      oc <- x$signals$onchain

      # MVRV
      if (!is.null(oc$mvrv)) {
        mvrv_emoji <- ifelse(grepl("Buy|Accumulate", oc$mvrv$signal), "🟢",
                             ifelse(grepl("Sell|Reduce", oc$mvrv$signal), "🔴", "🟡"))
        cat(mvrv_emoji, " MVRV Ratio: ", safe_extract(oc, c("mvrv", "value")),
            " [", safe_extract(oc, c("mvrv", "regime")), "]\n", sep = "")
        cat("     Signal: ", safe_extract(oc, c("mvrv", "signal")), "\n")
      }

      # NVT
      if (!is.null(oc$nvt)) {
        nvt_emoji <- ifelse(oc$nvt$regime == "Undervalued", "🟢",
                            ifelse(oc$nvt$regime == "Overvalued", "🔴", "🟡"))
        cat(nvt_emoji, " NVT Ratio: ", safe_extract(oc, c("nvt", "value")),
            " [", safe_extract(oc, c("nvt", "regime")), "]\n", sep = "")
      }

      # Puell Multiple
      if (!is.null(oc$puell)) {
        puell_emoji <- ifelse(grepl("Capitulation", oc$puell$regime), "🟢",
                              ifelse(grepl("Selling", oc$puell$regime), "🔴", "🟡"))
        cat(puell_emoji, " Puell Multiple: ", safe_extract(oc, c("puell", "value")),
            " [", safe_extract(oc, c("puell", "regime")), "]\n", sep = "")
      }

      # Composite
      if (!is.null(oc$composite)) {
        composite_emoji <- ifelse(oc$composite == "Strong Buy", "🟢🟢",
                                  ifelse(oc$composite == "Strong Sell", "🔴🔴", "🟡"))
        cat(composite_emoji, " Composite Signal: ", oc$composite, "\n")
      }
    }

    #-------------------------------------------------------------------------
    # DERIVATIVES INDICATORS
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📈 DERIVATIVES INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    if (!is.null(x$signals$derivatives)) {
      d <- x$signals$derivatives

      funding_emoji <- ifelse(d$funding_regime %in% c("Bullish", "Neutral"), "🟢",
                              ifelse(d$funding_regime %in% c("Bearish"), "🟡", "🔴"))
      cat(funding_emoji, " Funding Regime: ", safe_extract(d, "funding_regime"), "\n")

      cat("  • OI Percentile: ", sprintf("%.1f%%", as.numeric(d$oi_percentile) * 100), "\n")
      cat("  • Funding Percentile: ", sprintf("%.1f%%", as.numeric(d$funding_percentile) * 100), "\n")

      if (!is.null(d$crowded_long) && d$crowded_long) cat("  ⚠️  Crowded Long Positioning - Reversal Risk\n")
      if (!is.null(d$crowded_short) && d$crowded_short) cat("  ⚠️  Crowded Short Positioning - Bounce Risk\n")

      cat("  • Interpretation: ", safe_extract(d, "interpretation"), "\n")
    }

    #-------------------------------------------------------------------------
    # MACRO INDICATORS
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("🌎 MACRO INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    if (!is.null(x$signals$macro)) {
      m <- x$signals$macro

      liq_emoji <- ifelse(m$liquidity_regime == "Expanding", "🟢",
                          ifelse(m$liquidity_regime == "Contracting", "🔴", "🟡"))
      cat(liq_emoji, " Liquidity: ", safe_extract(m, "liquidity_regime"), "\n")

      rate_emoji <- ifelse(grepl("Low Rates", m$rate_regime), "🟢",
                           ifelse(grepl("High Rates", m$rate_regime), "🔴", "🟡"))
      cat(rate_emoji, " Rates: ", safe_extract(m, "rate_regime"), "\n")

      dollar_emoji <- ifelse(grepl("Weak Dollar", m$dollar_regime), "🟢",
                             ifelse(grepl("Strong Dollar", m$dollar_regime), "🔴", "🟡"))
      cat(dollar_emoji, " Dollar: ", safe_extract(m, "dollar_regime"), "\n")

      cat("\n  • Composite Risk Score: ", safe_extract(m, "composite_risk"), "\n")
      cat("  • Overall Bias: ", safe_extract(m, "risk_bias"), "\n")
    }

    #-------------------------------------------------------------------------
    # VOLATILITY INDICATORS
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📊 VOLATILITY INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    if (!is.null(x$signals$volatility)) {
      v <- x$signals$volatility

      vol_emoji <- ifelse(grepl("Low", v$regime), "🟢",
                          ifelse(grepl("High", v$regime), "🟡", "🔴"))
      cat(vol_emoji, " Volatility Regime: ", safe_extract(v, "regime"), "\n")
      cat("  • Current Environment: ", safe_extract(v, "current_regime"), "\n")
      cat("  • Strategy Setup: ", safe_extract(v, "strategy"), "\n")
      cat("  • Percentile (7d): ", sprintf("%.1f%%", as.numeric(v$percentile_short) * 100), "\n")
      cat("  • Percentile (30d): ", sprintf("%.1f%%", as.numeric(v$percentile_medium) * 100), "\n")
    }

    #-------------------------------------------------------------------------
    # POSITIONING INDICATORS
    #-------------------------------------------------------------------------
    if (!is.null(x$signals$positioning)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🐋 POSITIONING & SENTIMENT\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      p <- x$signals$positioning

      if (!is.null(p$whale_flow)) {
        whale_emoji <- ifelse(grepl("Accumulation|Bullish", p$whale_flow), "🟢",
                              ifelse(grepl("Selling|Bearish", p$whale_flow), "🔴", "🟡"))
        cat(whale_emoji, " Whale Flow: ", safe_extract(p, "whale_flow"), "\n")
        cat("  • Interpretation: ", safe_extract(p, "whale_interpretation"), "\n")
        cat("  • Activity: ", safe_extract(p, "whale_activity"), "\n")
      }

      if (!is.null(p$sentiment)) {
        sent_emoji <- ifelse(grepl("Low Interest", p$sentiment$signal), "🟢",
                             ifelse(grepl("Extreme Interest", p$sentiment$signal), "🔴", "🟡"))
        cat(sent_emoji, " Google Trends: ", safe_extract(p, c("sentiment", "score")),
            " [", safe_extract(p, c("sentiment", "signal")), "]\n", sep = "")
      }
    }

    #-------------------------------------------------------------------------
    # DECISION FRAMEWORK BY TIME HORIZON
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("⏱️  DECISION FRAMEWORK BY TIME HORIZON\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    # Short-term
    if (!is.null(x$decisions$short_term)) {
      st <- x$decisions$short_term
      st_signal <- safe_extract(st, c("momentum", "signal"), "Neutral")
      st_emoji <- ifelse(grepl("Long", st_signal), "🟢",
                         ifelse(grepl("Short", st_signal), "🔴", "🟡"))
      cat(st_emoji, " SHORT-TERM (Hours to Days):\n")
      cat("  • Momentum: ", st_signal,
          " (Score: ", safe_extract(st, c("momentum", "score"), "0"), ")\n", sep = "")

      if (!is.null(st$mean_reversion) && !is.null(st$mean_reversion$active) && st$mean_reversion$active) {
        cat("  • Mean Reversion: ", safe_extract(st, c("mean_reversion", "direction"), "Neutral"), "\n")
      }
    }

    # Medium-term
    if (!is.null(x$decisions$medium_term)) {
      mt <- x$decisions$medium_term
      mt_signal <- safe_extract(mt, c("trend", "direction"), "Neutral")
      mt_emoji <- ifelse(grepl("Bull", mt_signal), "🟢",
                         ifelse(grepl("Bear", mt_signal), "🔴", "🟡"))
      cat("\n", mt_emoji, " MEDIUM-TERM (Weeks to Months):\n", sep = "")
      cat("  • Trend: ", mt_signal,
          " (Score: ", safe_extract(mt, c("trend", "score"), "0"), ")\n", sep = "")
      cat("  • Valuation: ", safe_extract(mt, c("valuation", "composite"), "Neutral"), "\n")
      cat("  • Recommended Allocation: ", safe_extract(mt, c("position_sizing", "recommended_allocation_pct"), "50"), "%\n")
      cat("  • Risk per Trade: ", safe_extract(mt, c("position_sizing", "risk_per_trade_pct"), "1.5"), "%\n")
    }

    # Long-term
    if (!is.null(x$decisions$long_term)) {
      lt <- x$decisions$long_term
      lt_signal <- safe_extract(lt, c("strategic", "core_position"), "Maintain core position")
      lt_emoji <- ifelse(grepl("Accumulate|Buy", lt_signal), "🟢",
                         ifelse(grepl("Reduce", lt_signal), "🔴", "🟡"))
      cat("\n", lt_emoji, " LONG-TERM (Months to Years):\n", sep = "")
      cat("  • Strategic: ", lt_signal, "\n")
      cat("  • Cycle: ", safe_extract(lt, c("cycle", "interpretation"), "Mid-cycle"), "\n")
      cat("  • Halving: ", safe_extract(lt, c("halving", "phase"), "Unknown"), "\n")
    }

    #-------------------------------------------------------------------------
    # REGIME ALLOCATION
    #-------------------------------------------------------------------------
    if (!is.null(x$decisions$regime_allocation)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🎯 REGIME ALLOCATION\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      regime <- x$decisions$regime_allocation
      regime_current <- safe_extract(regime, "current_regime", "Neutral")
      regime_emoji <- switch(regime_current,
                             "Bull" = "🐂",
                             "Bear" = "🐻",
                             "Accumulation" = "💰",
                             "Distribution" = "📤",
                             "Neutral" = "⚖️",
                             "⚪")
      cat(regime_emoji, " Current Regime: ", regime_current, "\n\n")

      if (!is.null(regime$allocation)) {
        cat("Recommended Positioning:\n")
        cat("  • Strategic: ", safe_extract(regime, c("allocation", "strategic"), "50%"), "\n")
        cat("  • Tactical: ", safe_extract(regime, c("allocation", "tactical"), "Balanced"), "\n")
        cat("  • Hedges: ", safe_extract(regime, c("allocation", "hedges"), "Standard"), "\n")
      }
    }

    #-------------------------------------------------------------------------
    # ACTIONABLE SUMMARY
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("✅ ACTIONABLE SUMMARY\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    if (!is.null(x$summary$bullet_points)) {
      for (bullet in x$summary$bullet_points) {
        cat("  •", bullet, "\n")
      }
    }

    # Quick reference table
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📋 QUICK REFERENCE\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

    quick_ref <- data.frame(
      Horizon = c("Short-term", "Medium-term", "Long-term", "Risk Level", "Regime"),
      Signal = c(
        safe_extract(x, c("decisions", "short_term", "momentum", "signal"), "Neutral"),
        safe_extract(x, c("decisions", "medium_term", "trend", "direction"), "Neutral"),
        safe_extract(x, c("decisions", "long_term", "strategic", "core_position"), "Maintain"),
        safe_extract(x, c("decisions", "risk_off", "color"), "UNKNOWN"),
        safe_extract(x, c("decisions", "regime_allocation", "current_regime"), "Neutral")
      ),
      stringsAsFactors = FALSE
    )

    print(quick_ref, row.names = FALSE)
  }

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("👉 Use str(result) to see full data structure\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
}
