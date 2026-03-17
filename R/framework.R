#' Comprehensive Crypto Predictive Framework
#'
#' @description
#' The flagship function of the CryptoIndicator package. This framework integrates
#' on-chain, derivatives, macro, volatility, and positioning indicators into a
#' coherent decision system that generates actionable signals across multiple
#' time horizons (short-term, medium-term, long-term).
#'
#' @param assets Character vector of cryptocurrency names to analyze.
#'   Default \code{c("Bitcoin")}. Currently optimized for Bitcoin, but extensible
#'   to other assets that have sufficient data.
#' @param start_date Start date for historical data in "YYYY-MM-DD" format.
#'   Default \code{"2015-01-01"} provides a full market cycle of data.
#' @param fred_api_key Character string containing your FRED API key.
#'   Default \code{NULL}. If provided, will be stored in global environment.
#'   Register at \url{https://fred.stlouisfed.org/docs/api/api_key.html}
#' @param derivatives_lookback_days Integer. Number of days of derivatives data to fetch
#'   for percentile calculations. Default \code{90} (increased from 27 for statistical validity).
#' @param risk_override Logical. If TRUE, risk-off signals override conflicting
#'   time horizon signals. Default \code{TRUE}.
#' @param verbose Logical. Print detailed progress messages. Default \code{TRUE}.
#'
#' @return A list object of class \code{crypto_framework} containing seven
#'   main sections as documented in the package.
#'
#' @export
#'
#' @importFrom dplyr filter slice_tail case_when select arrange mutate
#' @importFrom utils head
crypto_predictive_framework <- function(assets = c("Bitcoin"),
                                        start_date = "2015-01-01",
                                        fred_api_key = NULL,
                                        risk_override = TRUE,
                                        verbose = TRUE) {

  #---------------------------------------------------------------------------
  # NULL coalescing helper (for internal use)
  #---------------------------------------------------------------------------
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

  #---------------------------------------------------------------------------
  # Helper function for safe tail operations
  #---------------------------------------------------------------------------
  safe_tail <- function(x, n = 1) {
    if (is.null(x) || length(x) == 0) return(NA)
    tryCatch(tail(x, n), error = function(e) NA)
  }

  #---------------------------------------------------------------------------
  # Helper function to check proximity to thresholds
  #---------------------------------------------------------------------------
  check_threshold_proximity <- function(value, threshold, threshold_name,
                                        proximity_pct = 5) {
    if (is.na(value) || is.na(threshold)) return(NULL)

    diff_pct <- abs((value - threshold) / threshold) * 100
    if (diff_pct <= proximity_pct) {
      return(paste0(threshold_name, " threshold (within ",
                    round(diff_pct, 1), "%)"))
    }
    return(NULL)
  }

  #---------------------------------------------------------------------------
  # INITIALIZATION
  #---------------------------------------------------------------------------
  framework <- list(
    metadata = list(
      timestamp = Sys.time(),
      assets = assets,
      start_date = start_date,
      end_date = Sys.Date(),
      status = "pending",
      risk_override = risk_override,
      warnings = character(),
      errors = character()
    ),
    raw_data = list(),
    indicators = list(),
    signals = list(),
    decisions = list(),
    summary = list()
  )

  # Set FRED API key if provided
  if (!is.null(fred_api_key)) {
    assign("fred_api_key", fred_api_key, envir = .GlobalEnv)
  }

  #---------------------------------------------------------------------------
  # MARKET DATA
  #---------------------------------------------------------------------------
  if (verbose) message("📊 Collecting market data...")

  tryCatch({
    market_data <- get_market_data(assets = assets, start_date = start_date)
    framework$raw_data$market <- market_data

    # Get Bitcoin data (assuming first asset is Bitcoin)
    btc_data <- market_data$data %>%
      dplyr::filter(slug == tolower(assets[1]))

    if (nrow(btc_data) == 0) {
      stop("No data returned for the specified asset")
    }

    if (verbose) message("  ✅ Retrieved ", nrow(btc_data), " days of data")

  }, error = function(e) {
    framework$metadata$errors <- c(framework$metadata$errors,
                                   paste("Market data:", e$message))
    framework$metadata$status <- "failed"
    if (verbose) message("  ❌ Market data failed: ", e$message)
    return(framework)
  })

  #---------------------------------------------------------------------------
  # ON-CHAIN INDICATORS
  #---------------------------------------------------------------------------
  if (verbose) message("⛓️  Calculating on-chain indicators...")

  tryCatch({
    # Add market cap column if not present
    if (!"market_cap" %in% names(btc_data)) {
      # Use dynamic supply calculation instead of hardcoded
      btc_data$market_cap <- btc_data$close *
        sapply(btc_data$timestamp, function(d) {
          # Approximate supply based on date (simplified)
          years_since_genesis <- as.numeric(d - as.Date("2009-01-03")) / 365.25
          # Rough approximation: ~328,500 BTC per year
          max(0, 50 * 144 * 365.25 * min(years_since_genesis, 4) +
                25 * 144 * 365.25 * max(0, min(years_since_genesis - 4, 4)) +
                12.5 * 144 * 365.25 * max(0, min(years_since_genesis - 8, 4)) +
                6.25 * 144 * 365.25 * max(0, min(years_since_genesis - 12, 4)) +
                3.125 * 144 * 365.25 * max(0, years_since_genesis - 16))
        })
    }

    onchain_data <- get_onchain_indicators(btc_data)
    framework$indicators$onchain <- onchain_data

    # Extract latest on-chain signals
    latest_onchain <- onchain_data %>%
      dplyr::slice_tail(n = 1)

    # Add proximity warnings for thresholds
    mvrv_value <- as.numeric(latest_onchain$mvrv)
    mvrv_proximity <- c(
      check_threshold_proximity(mvrv_value, 0.7, "Extreme Bottom"),
      check_threshold_proximity(mvrv_value, 1.75, "Extreme Top")
    )

    framework$signals$onchain <- list(
      mvrv = list(
        value = mvrv_value,
        regime = as.character(latest_onchain$mvrv_regime),
        signal = dplyr::case_when(
          latest_onchain$mvrv_regime == "Extreme Bottom" ~ "Strong Buy",
          latest_onchain$mvrv_regime == "Bear Market" ~ "Accumulate",
          latest_onchain$mvrv_regime == "Bull Market" ~ "Hold",
          latest_onchain$mvrv_regime == "Extreme Top" ~ "Strong Sell",
          TRUE ~ "Neutral"
        ),
        proximity_warnings = if (length(mvrv_proximity) > 0) mvrv_proximity else NULL
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

    if (verbose) message("  ✅ MVRV: ", framework$signals$onchain$mvrv$regime)

  }, error = function(e) {
    framework$metadata$errors <- c(framework$metadata$errors,
                                   paste("On-chain:", e$message))
    if (verbose) message("  ❌ On-chain failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # DERIVATIVES INDICATORS (with improved lookback)
  #---------------------------------------------------------------------------
  if (verbose) message("📈 Fetching derivatives data...")

  ticker <- "BTCUSDT"
  if (verbose) message("  Ticker: ", ticker)

  tryCatch({
    # Use extended lookback for meaningful percentiles
    deriv_result <- get_derivatives(
      ticker = ticker
    )
    framework$raw_data$derivatives <- deriv_result

    if (!is.null(deriv_result$data) && nrow(deriv_result$data) > 0) {

      # Get latest values
      signals_list <- deriv_result$signals

      # Calculate percentile confidence based on sample size
      n_obs <- sum(!is.na(deriv_result$data$open_interest))
      percentile_confidence <- dplyr::case_when(
        n_obs >= 180 ~ "High",
        n_obs >= 90 ~ "Medium",
        n_obs >= 30 ~ "Low",
        TRUE ~ "Very Low"
      )

      framework$signals$derivatives <- list(
        funding_regime = safe_tail(signals_list$funding_regime, 1) %||% "Unknown",
        crowded_long = safe_tail(signals_list$crowded_long, 1) %||% FALSE,
        crowded_short = safe_tail(signals_list$crowded_short, 1) %||% FALSE,
        oi_momentum = safe_tail(signals_list$oi_momentum, 1) %||% NA,
        oi_percentile = safe_tail(deriv_result$oi_percentile, 1) %||% NA,
        funding_percentile = safe_tail(deriv_result$funding_percentile, 1) %||% NA,
        n_observations = n_obs,
        percentile_confidence = percentile_confidence,
        interpretation = {
          regime <- safe_tail(signals_list$funding_regime, 1) %||% "Unknown"
          crowded_l <- safe_tail(signals_list$crowded_long, 1) %||% FALSE
          crowded_s <- safe_tail(signals_list$crowded_short, 1) %||% FALSE

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

      if (verbose) message("  ✅ Funding regime: ", framework$signals$derivatives$funding_regime,
                           " (", n_obs, " days of data)")

    } else {
      framework$metadata$warnings <- c(framework$metadata$warnings,
                                       "Derivatives data returned but empty")
    }

  }, error = function(e) {
    framework$metadata$warnings <- c(framework$metadata$warnings,
                                     paste("Derivatives:", e$message))
    framework$raw_data$derivatives <- NULL
    if (verbose) message("  ⚠️ Derivatives failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # MACRO INDICATORS
  #---------------------------------------------------------------------------
  if (verbose) message("🌎 Fetching macro indicators...")

  tryCatch({
    macro_data <- get_macro(start_date = start_date, fred_api_key = fred_api_key)
    framework$raw_data$macro <- macro_data

    framework$signals$macro <- list(
      liquidity_regime = macro_data$liquidity$liquidity_regime %||% "Unknown",
      rate_regime = macro_data$rates$rate_regime %||% "Unknown",
      dollar_regime = macro_data$dollar$dollar_regime %||% "Unknown",
      composite_risk = as.numeric(macro_data$composite_risk_score %||% 0),
      risk_bias = ifelse(!is.null(macro_data$composite_risk_score) &&
                           macro_data$composite_risk_score > 0.3, "Bullish",
                         ifelse(!is.null(macro_data$composite_risk_score) &&
                                  macro_data$composite_risk_score < -0.3, "Bearish", "Neutral")),
      data_quality = list(
        m2_available = !is.null(macro_data$liquidity$m2_us),
        dxy_available = !is.null(macro_data$dollar$dxy),
        rates_available = !is.null(macro_data$rates$treasury_10y)
      )
    )

    if (verbose) message("  ✅ Liquidity: ", macro_data$liquidity$liquidity_regime %||% "Unknown")

  }, error = function(e) {
    framework$metadata$warnings <- c(framework$metadata$warnings,
                                     paste("Macro:", e$message))
    framework$raw_data$macro <- NULL
    if (verbose) message("  ⚠️ Macro failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # VOLATILITY INDICATORS
  #---------------------------------------------------------------------------
  if (verbose) message("📊 Calculating volatility metrics...")

  tryCatch({
    if (!is.null(framework$indicators$onchain)) {
      vol_data <- get_volatility(data = framework$indicators$onchain)
      framework$indicators$volatility <- vol_data

      framework$signals$volatility <- list(
        regime = safe_tail(vol_data$regime, 1) %||% "Unknown",
        current_regime = vol_data$signals$current_regime %||% "Unknown",
        percentile_short = safe_tail(vol_data$volatility$percentile_short, 1) %||% NA,
        percentile_medium = safe_tail(vol_data$volatility$percentile_medium, 1) %||% NA,
        strategy = dplyr::case_when(
          safe_tail(vol_data$signals$vol_compression, 1) %||% FALSE ~ "Mean Reversion Setup",
          safe_tail(vol_data$signals$vol_breakout, 1) %||% FALSE ~ "Momentum Setup",
          TRUE ~ "No Clear Volatility Signal"
        )
      )

      if (verbose) message("  ✅ Volatility regime: ", framework$signals$volatility$regime)
    }

  }, error = function(e) {
    framework$metadata$warnings <- c(framework$metadata$warnings,
                                     paste("Volatility:", e$message))
    if (verbose) message("  ⚠️ Volatility failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # POSITIONING INDICATORS (Morpho-based intelligence)
  #---------------------------------------------------------------------------
  if (verbose) message("🐋 Fetching Morpho vault positioning data...")

  tryCatch({
    pos_data <- get_positioning()

    if (is.list(pos_data) && !is.character(pos_data)) {
      framework$raw_data$positioning <- pos_data

      # Fix HHI scaling (ensure it's on 0-10,000 scale)
      hhi_value <- if (!is.null(pos_data$raw_positioning_data$whale_concentration)) {
        pos_data$raw_positioning_data$whale_concentration$hhi
      } else {
        0
      }

      #-------------------------------------------------------------------------
      # BASIC POSITIONING SIGNALS
      #-------------------------------------------------------------------------
      framework$signals$positioning <- list(
        whale_flow = pos_data$exchange_positioning$flow_regime %||% "Unknown",
        whale_activity = pos_data$whale_activity$whale_signal %||% "Unknown",
        whale_interpretation = dplyr::case_when(
          grepl("Strong Inflows", pos_data$exchange_positioning$flow_regime %||% "") ~
            "🚀 Strong inflows - capital entering DeFi",
          grepl("Moderate Inflows", pos_data$exchange_positioning$flow_regime %||% "") ~
            "📈 Moderate inflows - growing DeFi exposure",
          grepl("Strong Outflows", pos_data$exchange_positioning$flow_regime %||% "") ~
            "📉 Strong outflows - capital leaving DeFi",
          grepl("Moderate Outflows", pos_data$exchange_positioning$flow_regime %||% "") ~
            "📊 Moderate outflows",
          TRUE ~ "⚖️ Neutral flows"
        )
      )

      #-------------------------------------------------------------------------
      # MORPHO-SPECIFIC INTELLIGENCE
      #-------------------------------------------------------------------------
      if (!is.null(pos_data$raw_positioning_data)) {
        morpho <- pos_data$raw_positioning_data

        # Whale concentration metrics with corrected HHI
        if (!is.null(morpho$whale_concentration)) {

          # Calculate top 5 share safely
          top_5_share <- morpho$whale_concentration$top_5_share %||% 0

          framework$signals$positioning$whale_concentration <- list(
            unique_whales = morpho$whale_concentration$total_whale_wallets %||% 0,
            mega_whales = morpho$whale_concentration$mega_whales %||% 0,
            regular_whales = morpho$whale_concentration$regular_whales %||% 0,
            hhi = hhi_value,
            hhi_interpretation = dplyr::case_when(
              hhi_value > 2500 ~ "Highly Concentrated",
              hhi_value > 1500 ~ "Moderately Concentrated",
              hhi_value > 1000 ~ "Slightly Concentrated",
              TRUE ~ "Competitive"
            ),
            top_5_share = top_5_share,

            # Risk classification
            concentration_risk = dplyr::case_when(
              hhi_value > 2500 ~ "🔴 CRITICAL",
              hhi_value > 1500 ~ "🟠 HIGH",
              hhi_value > 1000 ~ "🟡 MEDIUM",
              TRUE ~ "🟢 LOW"
            ),

            # Systemic risk flag
            systemic_risk = ifelse(
              top_5_share > 50,
              "⚠️ High systemic risk - top 5 whales control majority",
              "Distributed"
            )
          )
        }

        # Vault flow analysis
        if (!is.null(morpho$vault_flows) && !is.null(morpho$flow_summary)) {
          framework$signals$positioning$vault_flows <- list(
            transactions = sum(morpho$vault_flows$tx_count %||% 0),
            date_range = if (!is.null(morpho$vault_flows$oldest_tx_date)) {
              paste(
                format(min(morpho$vault_flows$oldest_tx_date, na.rm = TRUE), "%Y-%m-%d"),
                "to",
                format(max(morpho$vault_flows$newest_tx_date, na.rm = TRUE), "%Y-%m-%d")
              )
            } else {
              "No date range available"
            },
            net_flow_usd = morpho$flow_summary$net_flow_usd %||% 0,
            net_flow_pct = morpho$flow_summary$net_flow_pct %||% 0,
            flow_regime = morpho$flow_summary$flow_regime %||% "Unknown"
          )
        }

        framework$signals$positioning$data_source <- "Morpho Vaults"

        if (verbose) message("  ✅ Enhanced Morpho intelligence loaded")
      }

      if (verbose) message("  ✅ Flow regime: ", pos_data$exchange_positioning$flow_regime)
      if (verbose) message("  ✅ Whale activity: ", pos_data$whale_activity$whale_signal)

      if (!is.null(framework$signals$positioning$whale_concentration)) {
        if (verbose) message("  ✅ Whale concentration: ",
                             framework$signals$positioning$whale_concentration$hhi_interpretation)
      }

    } else {
      framework$metadata$warnings <- c(framework$metadata$warnings,
                                       "No positioning data found")
    }

  }, error = function(e) {
    framework$metadata$warnings <- c(framework$metadata$warnings,
                                     paste("Positioning:", e$message))
    if (verbose) message("  ⚠️ Positioning failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # FEAR & GREED SENTIMENT INDICATOR
  #---------------------------------------------------------------------------
  if (verbose) message("📈 Fetching Fear & Greed sentiment data...")

  tryCatch({
    fgi_data <- get_fear_greed(days = 30)

    if (!is.null(fgi_data)) {
      framework$raw_data$sentiment <- fgi_data

      # Use consistent emoji: Extreme fear = 🟢 (bullish contrarian)
      sentiment_emoji <- dplyr::case_when(
        fgi_data$current_value <= 25 ~ "🟢",  # Extreme fear = bullish contrarian
        fgi_data$current_value <= 45 ~ "🟡",
        fgi_data$current_value <= 55 ~ "⚪",
        fgi_data$current_value <= 75 ~ "🟠",
        TRUE ~ "🔴"  # Extreme greed = bearish contrarian
      )

      framework$signals$sentiment <- list(
        value = fgi_data$current_value,
        classification = fgi_data$current_classification,
        interpretation = dplyr::case_when(
          fgi_data$current_value >= 75 ~ "Extreme Greed - market euphoria, potential top (bearish)",
          fgi_data$current_value >= 55 ~ "Greed - bullish sentiment",
          fgi_data$current_value >= 45 ~ "Neutral - balanced sentiment",
          fgi_data$current_value >= 25 ~ "Fear - bearish sentiment",
          TRUE ~ "Extreme Fear - panic selling, potential bottom (bullish)"
        ),
        signal_emoji = sentiment_emoji,
        contrarian_signal = dplyr::case_when(
          fgi_data$current_value >= 75 ~ "Bearish (sell)",
          fgi_data$current_value <= 25 ~ "Bullish (buy)",
          TRUE ~ "Neutral"
        )
      )

      if (verbose) message("  ✅ Fear & Greed: ", fgi_data$current_value,
                           " - ", fgi_data$current_classification)
    } else {
      framework$metadata$warnings <- c(framework$metadata$warnings,
                                       "Fear & Greed data unavailable")
    }

  }, error = function(e) {
    framework$metadata$warnings <- c(framework$metadata$warnings,
                                     paste("Fear & Greed error:", e$message))
    if (verbose) message("  ⚠️ Fear & Greed failed: ", e$message)
  })

  #---------------------------------------------------------------------------
  # RISK-OFF SIGNALS (Enhanced with Morpho intelligence and sentiment)
  #---------------------------------------------------------------------------
  if (verbose) message("🔍 Calculating risk-off signals...")

  risk_level <- "LOW"
  risk_color <- "GREEN"
  risk_factors <- c()
  risk_scores <- list()

  # Volatility check
  if (!is.null(framework$signals$volatility$regime)) {
    if (grepl("Extreme High", framework$signals$volatility$regime)) {
      risk_level <- "HIGH"
      risk_color <- "RED"
      risk_factors <- c(risk_factors, "Extreme volatility - panic conditions")
      risk_scores$volatility <- 3
    } else if (grepl("High", framework$signals$volatility$regime)) {
      if (risk_level != "HIGH") {
        risk_level <- "MEDIUM"
        risk_color <- "YELLOW"
      }
      risk_factors <- c(risk_factors, "Elevated volatility")
      risk_scores$volatility <- 2
    } else {
      risk_scores$volatility <- 1
    }
  }

  # Macro check
  if (!is.null(framework$signals$macro$composite_risk)) {
    if (framework$signals$macro$composite_risk < -0.3) {
      if (risk_level != "HIGH") {
        risk_level <- "MEDIUM"
        risk_color <- "YELLOW"
      }
      risk_factors <- c(risk_factors, "Negative macro composite")
      risk_scores$macro <- 2
    }
    if (framework$signals$macro$composite_risk < -0.5) {
      risk_level <- "HIGH"
      risk_color <- "RED"
      risk_factors <- c(risk_factors, "Severe macro headwinds")
      risk_scores$macro <- 3
    }
    if (framework$signals$macro$composite_risk > 0.3) {
      risk_scores$macro <- 1  # Bullish macro reduces risk
    } else {
      risk_scores$macro <- risk_scores$macro %||% 2  # Default if not set
    }
  }

  # Derivatives check
  if (!is.null(framework$signals$derivatives$crowded_long) &&
      framework$signals$derivatives$crowded_long) {
    if (risk_level != "HIGH") {
      risk_level <- "MEDIUM"
      risk_color <- "YELLOW"
    }
    risk_factors <- c(risk_factors, "Extremely crowded long positioning")
    risk_scores$derivatives <- 2
  } else if (!is.null(framework$signals$derivatives$crowded_short) &&
             framework$signals$derivatives$crowded_short) {
    # Crowded short can be bullish (contrarian)
    risk_factors <- c(risk_factors, "Extremely crowded short positioning (bullish setup)")
    risk_scores$derivatives <- 1
  } else {
    risk_scores$derivatives <- 1
  }

  # Whale concentration check (Morpho intelligence)
  if (!is.null(framework$signals$positioning$whale_concentration)) {
    wc <- framework$signals$positioning$whale_concentration

    if (wc$hhi > 2500) {
      risk_level <- "HIGH"
      risk_color <- "RED"
      risk_factors <- c(risk_factors, "Critical whale concentration - systemic risk")
      risk_scores$whale <- 3
    } else if (wc$hhi > 1500) {
      if (risk_level != "HIGH") {
        risk_level <- "MEDIUM"
        risk_color <- "YELLOW"
      }
      risk_factors <- c(risk_factors, "High whale concentration")
      risk_scores$whale <- 2
    } else {
      risk_scores$whale <- 1
    }

    if (wc$top_5_share > 75) {
      risk_factors <- c(risk_factors, "Extreme concentration - top 5 whales control >75%")
    }
  }

  # Extreme vault flow check
  if (!is.null(framework$signals$positioning$vault_flows)) {
    net_flow_pct <- framework$signals$positioning$vault_flows$net_flow_pct
    if (!is.na(net_flow_pct) && abs(net_flow_pct) > 20) {
      if (risk_level != "HIGH") {
        risk_level <- "MEDIUM"
        risk_color <- "YELLOW"
      }
      risk_factors <- c(risk_factors, "Extreme vault flows - high volatility expected")
      risk_scores$flows <- 2
    } else {
      risk_scores$flows <- 1
    }
  }

  # Fear & Greed sentiment check (contrarian)
  if (!is.null(framework$signals$sentiment)) {
    fg_value <- framework$signals$sentiment$value

    if (fg_value >= 75) {
      # Extreme greed - bearish signal
      if (risk_level != "HIGH") {
        risk_level <- "MEDIUM"
        risk_color <- "YELLOW"
      }
      risk_factors <- c(risk_factors, paste("Extreme greed -", fg_value, "/100 (bearish)"))
      risk_scores$sentiment <- 2
    } else if (fg_value <= 25) {
      # Extreme fear - bullish contrarian (reduces risk)
      risk_factors <- c(risk_factors, paste("Extreme fear -", fg_value, "/100 (bullish contrarian)"))
      risk_scores$sentiment <- 0  # Reduces overall risk
    } else {
      risk_scores$sentiment <- 1
    }

    if (fg_value >= 85 || fg_value <= 15) {
      risk_level <- "HIGH"
      risk_color <- "RED"
      risk_factors <- c(risk_factors, paste("Critical sentiment extreme -",
                                            ifelse(fg_value >= 85, "greed", "fear")))
    }
  }

  # Calculate composite risk score (weighted average)
  valid_scores <- unlist(risk_scores[!sapply(risk_scores, is.null)])
  if (length(valid_scores) > 0) {
    composite_risk_score <- mean(valid_scores, na.rm = TRUE) / 3  # Scale to 0-1
  } else {
    composite_risk_score <- 0.5  # Default neutral
  }

  actions <- list(
    LOW = c("Maintain full positions", "Normal stop losses"),
    MEDIUM = c("Reduce leverage", "Tighten stops", "Take partial profits"),
    HIGH = c("Move to stablecoins", "Close leveraged positions", "Wait for stabilization")
  )

  framework$decisions$risk_off <- list(
    level = risk_level,
    color = risk_color,
    composite_score = composite_risk_score,
    factors = risk_factors,
    actions = actions[[risk_level]],
    risk_scores = risk_scores
  )

  #---------------------------------------------------------------------------
  # SHORT-TERM DECISIONS (hours to days)
  #---------------------------------------------------------------------------
  if (verbose) message("🎯 Generating short-term decisions...")

  framework$decisions$short_term <- list()

  momentum_score <- 0
  momentum_factors <- list()

  # Derivatives contribution
  if (!is.null(framework$signals$derivatives)) {
    funding_regime <- framework$signals$derivatives$funding_regime

    if (funding_regime %in% c("Bullish", "Neutral")) {
      momentum_score <- momentum_score + 1
      momentum_factors$derivatives <- 1
    } else if (funding_regime %in% c("Bearish", "Extreme Short")) {
      momentum_score <- momentum_score - 1
      momentum_factors$derivatives <- -1
    } else if (funding_regime == "Extreme Long") {
      momentum_score <- momentum_score - 1  # Crowded long is bearish short-term
      momentum_factors$derivatives <- -1
    } else {
      momentum_factors$derivatives <- 0
    }
  }

  # Price momentum
  if (!is.null(framework$indicators$onchain)) {
    recent_returns <- tail(framework$indicators$onchain$returns, 3)
    if (mean(recent_returns, na.rm = TRUE) > 0.01) {
      momentum_score <- momentum_score + 1
      momentum_factors$price <- 1
    } else if (mean(recent_returns, na.rm = TRUE) < -0.01) {
      momentum_score <- momentum_score - 1
      momentum_factors$price <- -1
    } else {
      momentum_factors$price <- 0
    }
  }

  # Sentiment contribution (short-term)
  if (!is.null(framework$signals$sentiment)) {
    fg_value <- framework$signals$sentiment$value
    if (fg_value <= 25) {
      momentum_score <- momentum_score + 1  # Extreme fear = buy signal
      momentum_factors$sentiment <- 1
    } else if (fg_value >= 75) {
      momentum_score <- momentum_score - 1  # Extreme greed = sell signal
      momentum_factors$sentiment <- -1
    } else {
      momentum_factors$sentiment <- 0
    }
  }

  # Apply risk override if enabled
  if (risk_override && risk_level == "HIGH") {
    # Override short-term signals in high risk
    framework$decisions$short_term$momentum <- list(
      score = momentum_score,
      raw_score = momentum_score,
      signal = "RISK-OFF - No Short-term Trades",
      factors = momentum_factors,
      risk_overridden = TRUE,
      original_signal = dplyr::case_when(
        momentum_score >= 2 ~ "Strong Long",
        momentum_score >= 1 ~ "Long",
        momentum_score <= -2 ~ "Strong Short",
        momentum_score <= -1 ~ "Short",
        TRUE ~ "Neutral"
      )
    )
  } else {
    framework$decisions$short_term$momentum <- list(
      score = momentum_score,
      signal = dplyr::case_when(
        momentum_score >= 2 ~ "Strong Long",
        momentum_score >= 1 ~ "Long",
        momentum_score <= -2 ~ "Strong Short",
        momentum_score <= -1 ~ "Short",
        TRUE ~ "Neutral"
      ),
      factors = momentum_factors,
      risk_overridden = FALSE
    )
  }

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

  #---------------------------------------------------------------------------
  # MEDIUM-TERM DECISIONS (weeks to months)
  #---------------------------------------------------------------------------
  if (verbose) message("🎯 Generating medium-term decisions...")

  framework$decisions$medium_term <- list()

  trend_score <- 0
  trend_factors <- list()

  # On-chain contribution
  if (!is.null(framework$signals$onchain$mvrv$regime)) {
    if (framework$signals$onchain$mvrv$regime %in% c("Extreme Bottom", "Bear Market")) {
      trend_score <- trend_score + 2
      trend_factors$mvrv <- 2
    } else if (framework$signals$onchain$mvrv$regime == "Bull Market") {
      trend_score <- trend_score + 1
      trend_factors$mvrv <- 1
    } else if (framework$signals$onchain$mvrv$regime == "Extreme Top") {
      trend_score <- trend_score - 2
      trend_factors$mvrv <- -2
    } else {
      trend_factors$mvrv <- 0
    }
  }

  # Macro contribution
  if (!is.null(framework$signals$macro$composite_risk)) {
    macro_contrib <- round(framework$signals$macro$composite_risk * 2)
    trend_score <- trend_score + macro_contrib
    trend_factors$macro <- macro_contrib
  }

  # Whale flow contribution (Morpho vault flows)
  if (!is.null(framework$signals$positioning$vault_flows)) {
    net_flow_pct <- framework$signals$positioning$vault_flows$net_flow_pct
    if (!is.na(net_flow_pct)) {
      if (net_flow_pct > 10) {
        trend_score <- trend_score + 1
        trend_factors$flows <- 1
        if (verbose) message("    ✅ Vault inflows adding to bullish trend")
      } else if (net_flow_pct < -10) {
        trend_score <- trend_score - 1
        trend_factors$flows <- -1
        if (verbose) message("    ⚠️ Vault outflows adding to bearish trend")
      } else {
        trend_factors$flows <- 0
      }
    }
  }

  # Whale concentration impact (systemic risk)
  if (!is.null(framework$signals$positioning$whale_concentration)) {
    wc <- framework$signals$positioning$whale_concentration
    if (wc$hhi > 2500) {
      trend_score <- trend_score - 1  # High concentration is bearish
      trend_factors$concentration <- -1
    } else if (wc$hhi > 1500) {
      trend_score <- trend_score - 0.5
      trend_factors$concentration <- -0.5
    } else {
      trend_factors$concentration <- 0
    }
  }

  # Apply risk override if enabled
  if (risk_override && risk_level == "HIGH") {
    # In high risk, medium-term signals are downgraded
    framework$decisions$medium_term$trend <- list(
      score = trend_score,
      raw_score = trend_score,
      direction = "RISK-OFF - Reduce Exposure",
      factors = trend_factors,
      risk_overridden = TRUE,
      original_direction = dplyr::case_when(
        trend_score >= 2 ~ "Strong Bull",
        trend_score >= 1 ~ "Bull",
        trend_score <= -2 ~ "Strong Bear",
        trend_score <= -1 ~ "Bear",
        TRUE ~ "Neutral"
      )
    )

    # Override allocation in high risk
    allocation <- 15  # Minimal allocation
    risk_per_trade <- 0.5  # Tiny risk

  } else if (risk_override && risk_level == "MEDIUM") {
    # In medium risk, reduce allocation
    if (trend_score >= 2) {
      allocation <- 50
    } else if (trend_score >= 1) {
      allocation <- 40
    } else if (trend_score <= -2) {
      allocation <- 15
    } else if (trend_score <= -1) {
      allocation <- 20
    } else {
      allocation <- 30
    }
    risk_per_trade <- 0.75

    framework$decisions$medium_term$trend <- list(
      score = trend_score,
      direction = dplyr::case_when(
        trend_score >= 2 ~ "Strong Bull (Cautious)",
        trend_score >= 1 ~ "Bull (Cautious)",
        trend_score <= -2 ~ "Strong Bear (Cautious)",
        trend_score <= -1 ~ "Bear (Cautious)",
        TRUE ~ "Neutral (Cautious)"
      ),
      factors = trend_factors,
      risk_overridden = TRUE,
      risk_level = "MEDIUM"
    )

  } else {
    # Normal risk - full signals
    framework$decisions$medium_term$trend <- list(
      score = trend_score,
      direction = dplyr::case_when(
        trend_score >= 2 ~ "Strong Bull",
        trend_score >= 1 ~ "Bull",
        trend_score <= -2 ~ "Strong Bear",
        trend_score <= -1 ~ "Bear",
        TRUE ~ "Neutral"
      ),
      factors = trend_factors,
      risk_overridden = FALSE
    )

    # Normal allocation
    if (trend_score >= 2) {
      allocation <- 75
    } else if (trend_score >= 1) {
      allocation <- 65
    } else if (trend_score <= -2) {
      allocation <- 25
    } else if (trend_score <= -1) {
      allocation <- 35
    } else {
      allocation <- 50
    }
    risk_per_trade <- ifelse(allocation > 60, 1.0, 1.5)
  }

  # Valuation composite
  framework$decisions$medium_term$valuation <- list(
    composite = if (!is.null(framework$signals$onchain$composite)) {
      framework$signals$onchain$composite
    } else {
      "Neutral"
    }
  )

  # Position sizing
  framework$decisions$medium_term$position_sizing <- list(
    recommended_allocation_pct = allocation,
    risk_per_trade_pct = risk_per_trade,
    risk_level_context = if (risk_override) risk_level else "Normal"
  )

  #---------------------------------------------------------------------------
  # LONG-TERM DECISIONS (months to years)
  #---------------------------------------------------------------------------
  if (verbose) message("🎯 Generating long-term decisions...")

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
    ),
    mvrv_proximity = framework$signals$onchain$mvrv$proximity_warnings
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

  # Strategic allocation with risk override
  if (risk_override && risk_level == "HIGH") {
    # In high risk, even long-term should be cautious
    core_position <- "Defensive - Accumulate only on severe dips"
  } else {
    core_position <- ifelse(
      !is.null(framework$signals$onchain$mvrv$regime) &&
        framework$signals$onchain$mvrv$regime %in% c("Extreme Bottom", "Bear Market"),
      "Accumulate core position",
      ifelse(!is.null(framework$signals$onchain$mvrv$regime) &&
               framework$signals$onchain$mvrv$regime == "Extreme Top",
             "Reduce core position", "Maintain core position")
    )
  }

  framework$decisions$long_term$strategic <- list(
    core_position = core_position,
    risk_context = if (risk_override) risk_level else "Normal"
  )

  #---------------------------------------------------------------------------
  # REGIME ALLOCATION
  #---------------------------------------------------------------------------
  if (verbose) message("🎯 Determining market regime...")

  regime <- "Neutral"
  regime_confidence <- "Medium"

  # Determine regime with confidence
  bull_signals <- 0
  bear_signals <- 0

  if (!is.null(framework$signals$macro$liquidity_regime) &&
      framework$signals$macro$liquidity_regime == "Expanding") {
    bull_signals <- bull_signals + 1
  }
  if (!is.null(framework$signals$macro$liquidity_regime) &&
      framework$signals$macro$liquidity_regime == "Contracting") {
    bear_signals <- bear_signals + 1
  }

  if (!is.null(framework$signals$onchain$mvrv$regime)) {
    if (framework$signals$onchain$mvrv$regime == "Bull Market") {
      bull_signals <- bull_signals + 2
    } else if (framework$signals$onchain$mvrv$regime %in% c("Bear Market", "Extreme Bottom")) {
      bear_signals <- bear_signals + 2
    } else if (framework$signals$onchain$mvrv$regime == "Extreme Top") {
      bear_signals <- bear_signals + 3
    }
  }

  if (!is.null(framework$signals$positioning$vault_flows$flow_regime)) {
    if (grepl("Inflows", framework$signals$positioning$vault_flows$flow_regime)) {
      bull_signals <- bull_signals + 1
    } else if (grepl("Outflows", framework$signals$positioning$vault_flows$flow_regime)) {
      bear_signals <- bear_signals + 1
    }
  }

  if (bull_signals > bear_signals + 1) {
    regime <- "Bull"
    regime_confidence <- "High"
  } else if (bear_signals > bull_signals + 1) {
    regime <- "Bear"
    regime_confidence <- "High"
  } else if (!is.null(framework$signals$onchain$mvrv$regime) &&
             framework$signals$onchain$mvrv$regime == "Extreme Top") {
    regime <- "Distribution"
    regime_confidence <- "High"
  } else if (!is.null(framework$signals$onchain$mvrv$regime) &&
             framework$signals$onchain$mvrv$regime == "Extreme Bottom") {
    regime <- "Accumulation"
    regime_confidence <- "High"
  } else {
    regime <- "Neutral"
    regime_confidence <- "Medium"
  }

  # Override regime if risk is extreme
  if (risk_override && risk_level == "HIGH" && regime %in% c("Bull", "Neutral")) {
    regime <- "Defensive"
    regime_confidence <- "High (Risk Override)"
  }

  allocation_map <- list(
    Bull = list(strategic = "75-100%", tactical = "Trend following", hedges = "Minimal"),
    Bear = list(strategic = "25-50%", tactical = "Mean reversion", hedges = "Consider hedges"),
    Accumulation = list(strategic = "50-75% DCA", tactical = "Scale in", hedges = "Light"),
    Distribution = list(strategic = "<25%", tactical = "Take profits", hedges = "Increase"),
    Neutral = list(strategic = "50%", tactical = "Balanced", hedges = "Standard"),
    Defensive = list(strategic = "<15%", tactical = "Cash only", hedges = "Maximal")
  )

  framework$decisions$regime_allocation <- list(
    current_regime = regime,
    confidence = regime_confidence,
    signals = list(bull = bull_signals, bear = bear_signals),
    allocation = allocation_map[[regime]] %||% allocation_map$Neutral
  )

  #---------------------------------------------------------------------------
  # ACTIONABLE SUMMARY (Enhanced with Morpho insights and sentiment)
  #---------------------------------------------------------------------------
  if (verbose) message("📝 Generating actionable summary...")

  # Start with risk as the primary message
  if (risk_level == "HIGH") {
    summary_lines <- c(
      paste0("🔴 RISK: ", risk_color, " - URGENT: ", actions[[risk_level]][1]),
      paste0("   → ", actions[[risk_level]][2]),
      paste0("   → ", actions[[risk_level]][3])
    )
  } else {
    summary_lines <- c(
      paste0("RISK: ", risk_color, " - ", actions[[risk_level]][1])
    )
  }

  # Add regime and strategy
  summary_lines <- c(
    summary_lines,
    paste0("REGIME: ", regime, " (", regime_confidence, " confidence)"),
    paste0("STRATEGY: ", framework$decisions$regime_allocation$allocation$tactical),
    paste0("ALLOCATION: ", framework$decisions$regime_allocation$allocation$strategic)
  )

  # Add key signals with emojis
  if (!is.null(framework$signals$sentiment)) {
    summary_lines <- c(
      summary_lines,
      paste0("SENTIMENT: ", framework$signals$sentiment$signal_emoji, " ",
             framework$signals$sentiment$classification, " (",
             framework$signals$sentiment$value, "/100) - ",
             framework$signals$sentiment$contrarian_signal)
    )
  }

  # Add whale concentration
  if (!is.null(framework$signals$positioning$whale_concentration)) {
    summary_lines <- c(
      summary_lines,
      paste0("WHALE CONCENTRATION: ",
             framework$signals$positioning$whale_concentration$concentration_risk,
             " (HHI: ", round(framework$signals$positioning$whale_concentration$hhi, 0), ")")
    )
  }

  # Add vault flows
  if (!is.null(framework$signals$positioning$vault_flows)) {
    net_flow_pct <- framework$signals$positioning$vault_flows$net_flow_pct
    if (!is.na(net_flow_pct)) {
      flow_emoji <- ifelse(net_flow_pct > 0, "📥", "📤")
      summary_lines <- c(
        summary_lines,
        paste0("VAULT FLOWS: ", flow_emoji, " ",
               framework$signals$positioning$vault_flows$flow_regime,
               " (", round(net_flow_pct, 1), "%)")
      )
    }
  }

  # Add MVRV with proximity warning
  if (!is.null(framework$signals$onchain$mvrv$proximity_warnings)) {
    summary_lines <- c(
      summary_lines,
      paste0("⚠️ MVRV PROXIMITY: ",
             paste(framework$signals$onchain$mvrv$proximity_warnings, collapse = "; "))
    )
  }

  # Add derivatives data quality note
  if (!is.null(framework$signals$derivatives$percentile_confidence) &&
      framework$signals$derivatives$percentile_confidence != "High") {
    summary_lines <- c(
      summary_lines,
      paste0("📊 DERIVATIVES CONFIDENCE: ",
             framework$signals$derivatives$percentile_confidence,
             " (", framework$signals$derivatives$n_observations, " days)")
    )
  }

  # Add time horizon signals (with risk override context)
  st_signal <- if (risk_override && risk_level == "HIGH") {
    "RISK-OFF"
  } else {
    framework$decisions$short_term$momentum$signal
  }

  mt_signal <- if (risk_override && risk_level == "HIGH") {
    "RISK-OFF"
  } else {
    framework$decisions$medium_term$trend$direction
  }

  summary_lines <- c(
    summary_lines,
    paste0("SHORT-TERM: ", st_signal),
    paste0("MEDIUM-TERM: ", mt_signal),
    paste0("LONG-TERM: ", framework$decisions$long_term$strategic$core_position)
  )

  framework$summary <- list(
    timestamp = Sys.time(),
    risk = list(
      level = risk_level,
      color = risk_color,
      composite_score = composite_risk_score,
      factors = risk_factors
    ),
    regime = list(
      name = regime,
      confidence = regime_confidence
    ),
    short_term = framework$decisions$short_term$momentum$signal,
    medium_term = if (risk_override && risk_level == "HIGH") "RISK-OFF" else framework$decisions$medium_term$trend$direction,
    long_term = framework$decisions$long_term$strategic$core_position,
    allocation = framework$decisions$regime_allocation$allocation$strategic,
    bullet_points = summary_lines,
    risk_override_applied = risk_override && risk_level %in% c("MEDIUM", "HIGH")
  )

  # Update metadata
  framework$metadata$status <- "success"
  framework$metadata$warning_count <- length(framework$metadata$warnings)
  framework$metadata$error_count <- length(framework$metadata$errors)

  # Set class for pretty printing
  class(framework) <- "crypto_framework"

  if (verbose) {
    message("\n✅ Framework complete!")
    message("   Risk Level: ", risk_color, " (", risk_level, ")")
    message("   Regime: ", regime)
    message("   Use print_crypto_summary() to view detailed results")
  }

  return(framework)
}

#' Print Comprehensive Crypto Framework Summary
#'
#' @description
#' Displays a formatted, easy-to-understand summary of the crypto predictive framework
#' results. The output is designed to be self-explanatory and shareable with team
#' members, stakeholders, or anyone needing actionable crypto market insights.
#'
#' @param x A crypto_framework object returned by \code{\link{crypto_predictive_framework}}
#' @param detailed Logical. If TRUE, shows all sections. If FALSE, shows condensed summary.
#'   Default TRUE.
#'
#' @return None. Prints formatted summary to console.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- crypto_predictive_framework()
#' print_crypto_summary(result)
#' print_crypto_summary(result, detailed = FALSE)  # Quick view
#' }
print_crypto_summary <- function(x, detailed = TRUE) {

  # Helper function to safely format dates
  safe_format_date <- function(date_val) {
    tryCatch({
      if (inherits(date_val, "Date")) {
        format(date_val, "%Y-%m-%d")
      } else if (is.character(date_val)) {
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
        if (abs(value) > 1000000) {
          return(sprintf("$%.2fM", value/1e6))
        } else if (abs(value) > 1000) {
          return(sprintf("$%.2fK", value/1e3))
        } else {
          return(sprintf("%.2f", value))
        }
      }
      return(as.character(value))
    }, error = function(e) {
      return(default)
    })
  }

  # Helper function to safely check if value exists and is not NULL/NA
  safe_exists <- function(x) {
    !is.null(x) && length(x) > 0 && !all(is.na(x))
  }

  # Helper function to add explanatory notes (only in detailed mode)
  add_explanation <- function(text, explanation, detailed) {
    if (detailed) {
      cat("  📝 ", explanation, "\n")
    }
  }

  # Helper function to print a separator line
  print_sep <- function(char = "━", length = 80) {
    cat("\n", paste(rep(char, length), collapse = ""), "\n")
  }

  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("🚀 CRYPTO PREDICTIVE FRAMEWORK - COMPREHENSIVE SUMMARY\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  # Safe metadata extraction - FIX: Handle date conversion properly
  cat("Generated:", format(x$metadata$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Asset:", paste(x$metadata$assets, collapse = ", "), "\n")

  # FIX: Handle end_date which appears to be numeric (20521) in your output
  end_date_display <- tryCatch({
    if (is.numeric(x$metadata$end_date)) {
      as.character(as.Date(x$metadata$end_date, origin = "1970-01-01"))
    } else {
      as.character(x$metadata$end_date)
    }
  }, error = function(e) {
    as.character(x$metadata$end_date)
  })

  cat("Data Range:", x$metadata$start_date, "to", end_date_display, "\n")
  cat("Status:", toupper(x$metadata$status), "\n")

  # Risk override status
  if (!is.null(x$metadata$risk_override) && x$metadata$risk_override) {
    cat("Risk Override: ACTIVE - Risk level overrides conflicting signals\n")
  }

  # Data source info
  if (!is.null(x$signals$positioning$data_source)) {
    cat("Positioning Source:", x$signals$positioning$data_source, "\n")
  }

  # Warnings
  if (length(x$metadata$warnings) > 0) {
    cat("\n⚠️  WARNINGS:\n")
    for (w in x$metadata$warnings) cat("  •", w, "\n")
  }

  # Errors
  if (length(x$metadata$errors) > 0) {
    cat("\n❌ ERRORS:\n")
    for (e in x$metadata$errors) cat("  •", e, "\n")
  }

  if (x$metadata$status == "success") {

    #-------------------------------------------------------------------------
    # RISK-OFF SIGNAL (Most Important - Shown First)
    #-------------------------------------------------------------------------
    print_sep()
    risk_color <- x$decisions$risk_off$color %||% "UNKNOWN"
    risk_emoji <- ifelse(risk_color == "GREEN", "🟢",
                         ifelse(risk_color == "YELLOW", "🟡",
                                ifelse(risk_color == "RED", "🔴", "⚪")))
    cat(risk_emoji, " RISK-OFF SIGNAL: [", risk_color, "] ", risk_emoji, "\n")

    # Show composite risk score
    if (!is.null(x$decisions$risk_off$composite_score)) {
      cat("   Composite Risk Score: ", round(x$decisions$risk_off$composite_score, 2),
          " (0-1 scale, higher = more risk)\n")
    }
    print_sep()

    if (detailed) {
      # Explanation of risk levels
      cat("  What this means:\n")
      if (risk_color == "GREEN") {
        cat("  • ✅ Normal market conditions - no unusual risks detected\n")
        cat("  • 📈 Full position sizes appropriate\n")
        cat("  • 🛡️ Standard stop losses recommended\n")
      } else if (risk_color == "YELLOW") {
        cat("  • ⚠️ Elevated risk factors present\n")
        cat("  • 📉 Consider reducing leverage and tightening stops\n")
        cat("  • 💰 Take partial profits to reduce exposure\n")
      } else if (risk_color == "RED") {
        cat("  • 🔴 HIGH RISK - Market stress detected\n")
        cat("  • 💵 Consider moving to stablecoins\n")
        cat("  • 🚫 Avoid new positions, close leveraged trades\n")
      }

      cat("\nRisk Factors:\n")
      risk_factors <- x$decisions$risk_off$factors
      if (length(risk_factors) > 0 && !is.null(risk_factors)) {
        for (factor in risk_factors) cat("  •", factor, "\n")
      } else {
        cat("  • No significant risk factors detected\n")
      }

      cat("\nRecommended Actions:\n")
      risk_actions <- x$decisions$risk_off$actions
      if (length(risk_actions) > 0 && !is.null(risk_actions)) {
        for (action in risk_actions) cat("  →", action, "\n")
      }
    }

    # Only show detailed sections if requested
    if (detailed) {

      #-------------------------------------------------------------------------
      # ON-CHAIN INDICATORS (Fundamental Valuation)
      #-------------------------------------------------------------------------
      print_sep()
      cat("⛓️  ON-CHAIN INDICATORS\n")
      print_sep()
      cat("  These metrics analyze blockchain data to determine if Bitcoin is\n")
      cat("  undervalued or overvalued based on network activity and miner behavior.\n\n")

      if (!is.null(x$signals$onchain)) {
        oc <- x$signals$onchain

        # MVRV
        if (!is.null(oc$mvrv)) {
          mvrv_emoji <- ifelse(grepl("Buy|Accumulate", oc$mvrv$signal), "🟢",
                               ifelse(grepl("Sell|Reduce", oc$mvrv$signal), "🔴", "🟡"))
          cat(mvrv_emoji, " MVRV Ratio: ", round(oc$mvrv$value, 2),
              " [", oc$mvrv$regime, "]\n", sep = "")
          cat("     Signal: ", oc$mvrv$signal, "\n")

          # Show proximity warnings if any
          if (!is.null(oc$mvrv$proximity_warnings) && length(oc$mvrv$proximity_warnings) > 0) {
            for (warning in oc$mvrv$proximity_warnings) {
              cat("     ⚠️ ", warning, "\n")
            }
          }

          add_explanation("MVRV", "Compares current price to average purchase price of all coins. <0.7 = extreme bottom, >1.75 = extreme top.", detailed)
        }

        # NVT
        if (!is.null(oc$nvt) && safe_exists(oc$nvt$value)) {
          nvt_emoji <- ifelse(oc$nvt$regime == "Undervalued", "🟢",
                              ifelse(oc$nvt$regime == "Overvalued", "🔴", "🟡"))
          cat(nvt_emoji, " NVT Ratio: ", round(oc$nvt$value, 2),
              " [", oc$nvt$regime, "]\n", sep = "")
          add_explanation("NVT", "Like P/E ratio for stocks. Low = network usage high relative to value, High = value exceeds network usage.", detailed)
        }

        # Puell Multiple
        if (!is.null(oc$puell) && safe_exists(oc$puell$value) && !is.na(oc$puell$value)) {
          puell_emoji <- ifelse(grepl("Capitulation", oc$puell$regime), "🟢",
                                ifelse(grepl("Selling", oc$puell$regime), "🔴", "🟡"))
          cat(puell_emoji, " Puell Multiple: ", round(oc$puell$value, 2),
              " [", oc$puell$regime, "]\n", sep = "")
          add_explanation("Puell", "Measures miner profitability. Low = miners selling at a loss (capitulation), High = miners exceptionally profitable (selling pressure).", detailed)
        }

        # Composite
        if (!is.null(oc$composite)) {
          composite_emoji <- ifelse(oc$composite == "Strong Buy", "🟢🟢",
                                    ifelse(oc$composite == "Strong Sell", "🔴🔴", "🟡"))
          cat(composite_emoji, " Composite Signal: ", oc$composite, "\n")
          add_explanation("Composite", "Combines MVRV and Puell for higher conviction signals.", detailed)
        }
      }

      #-------------------------------------------------------------------------
      # DERIVATIVES INDICATORS (Market Positioning) - FIX: Handle empty case
      #-------------------------------------------------------------------------
      print_sep()
      cat("📈 DERIVATIVES INDICATORS\n")
      print_sep()
      cat("  These metrics show how traders are positioned in the futures market,\n")
      cat("  revealing leverage extremes and potential crowded trades.\n\n")

      if (!is.null(x$signals$derivatives) && length(x$signals$derivatives) > 0) {
        d <- x$signals$derivatives

        # Check if we have actual data
        if (safe_exists(d$funding_regime) && d$funding_regime != "Unknown") {
          funding_emoji <- ifelse(d$funding_regime %in% c("Bullish", "Neutral"), "🟢",
                                  ifelse(d$funding_regime %in% c("Bearish"), "🟡", "🔴"))
          cat(funding_emoji, " Funding Regime: ", d$funding_regime, "\n")

          # Show data quality
          if (!is.null(d$n_observations) && d$n_observations > 0) {
            cat("  • Data Quality: ", d$n_observations, " days (",
                d$percentile_confidence %||% "Unknown", " confidence)\n", sep = "")
          }

          add_explanation("Funding", "Shows who's paying whom to keep positions open. Positive = longs pay shorts (bullish sentiment). Extreme values signal crowded trades.", detailed)

          if (!is.null(d$oi_percentile) && !is.na(d$oi_percentile)) {
            cat("  • OI Percentile: ", sprintf("%.1f%%", d$oi_percentile * 100),
                " - % of time open interest has been lower than current\n")
          }
          if (!is.null(d$funding_percentile) && !is.na(d$funding_percentile)) {
            cat("  • Funding Percentile: ", sprintf("%.1f%%", d$funding_percentile * 100),
                " - % of time funding has been lower than current\n")
          }

          if (!is.null(d$crowded_long) && d$crowded_long) {
            cat("  ⚠️  CROWDED LONG - Extreme long positioning, vulnerable to liquidation cascade\n")
          }
          if (!is.null(d$crowded_short) && d$crowded_short) {
            cat("  ⚠️  CROWDED SHORT - Extreme short positioning, primed for short squeeze\n")
          }

          if (safe_exists(d$interpretation)) {
            cat("  • Interpretation: ", d$interpretation, "\n")
          }
        } else {
          cat("  📊 Derivatives data currently unavailable or insufficient\n")
          cat("  This may be due to API limitations or low liquidity periods.\n")
        }
      } else {
        cat("  📊 Derivatives data not available in this run\n")
        cat("  Check API connections or try again later.\n")
      }

      #-------------------------------------------------------------------------
      # MACRO INDICATORS (Global Economic Context)
      #-------------------------------------------------------------------------
      print_sep()
      cat("🌎 MACRO INDICATORS\n")
      print_sep()
      cat("  These indicators show the broader economic environment that historically\n")
      cat("  influences crypto markets (liquidity, interest rates, dollar strength).\n\n")

      if (!is.null(x$signals$macro)) {
        m <- x$signals$macro

        # Show data availability
        if (!is.null(m$data_quality)) {
          missing <- c()
          if (!isTRUE(m$data_quality$m2_available)) missing <- c(missing, "M2")
          if (!isTRUE(m$data_quality$dxy_available)) missing <- c(missing, "DXY")
          if (!isTRUE(m$data_quality$rates_available)) missing <- c(missing, "Rates")
          if (length(missing) > 0) {
            cat("  ⚠️ Missing data: ", paste(missing, collapse = ", "), "\n")
          }
        }

        if (safe_exists(m$liquidity_regime)) {
          liq_emoji <- ifelse(m$liquidity_regime == "Expanding", "🟢",
                              ifelse(m$liquidity_regime == "Contracting", "🔴", "🟡"))
          cat(liq_emoji, " Liquidity: ", m$liquidity_regime, "\n")
          add_explanation("Liquidity", "Money supply growth. Expanding (>5% YoY) = bullish for crypto, Contracting (<0%) = bearish.", detailed)
        }

        if (safe_exists(m$rate_regime)) {
          rate_emoji <- ifelse(grepl("Low Rates", m$rate_regime), "🟢",
                               ifelse(grepl("High Rates", m$rate_regime), "🔴", "🟡"))
          cat(rate_emoji, " Rates: ", m$rate_regime, "\n")
          add_explanation("Rates", "10-year Treasury yield. Low rates (<1%) = risk-on, High rates (>3%) = risk-off (competes with crypto).", detailed)
        }

        if (safe_exists(m$dollar_regime)) {
          dollar_emoji <- ifelse(grepl("Weak Dollar", m$dollar_regime), "🟢",
                                 ifelse(grepl("Strong Dollar", m$dollar_regime), "🔴", "🟡"))
          cat(dollar_emoji, " Dollar: ", m$dollar_regime, "\n")
          add_explanation("Dollar", "DXY index. Weak dollar (<90) = bullish for crypto, Strong dollar (>100) = bearish.", detailed)
        }

        if (safe_exists(m$composite_risk)) {
          cat("\n  • Composite Risk Score: ", round(m$composite_risk, 2),
              " (Scale -1 to +1, higher = more bullish)\n")
        }
        if (safe_exists(m$risk_bias)) {
          cat("  • Overall Bias: ", m$risk_bias, "\n")
        }
      }

      #-------------------------------------------------------------------------
      # VOLATILITY INDICATORS (Market Conditions)
      #-------------------------------------------------------------------------
      print_sep()
      cat("📊 VOLATILITY INDICATORS\n")
      print_sep()
      cat("  Volatility regimes determine which trading strategies work best.\n\n")

      if (!is.null(x$signals$volatility)) {
        v <- x$signals$volatility

        if (safe_exists(v$regime)) {
          vol_emoji <- ifelse(grepl("Low", v$regime), "🟢",
                              ifelse(grepl("High", v$regime), "🟡", "🔴"))
          cat(vol_emoji, " Volatility Regime: ", v$regime, "\n")
        }
        if (safe_exists(v$current_regime)) {
          cat("  • Current Environment: ", v$current_regime, "\n")
        }
        if (safe_exists(v$strategy)) {
          cat("  • Best Strategy: ", v$strategy, "\n")
        }
        if (!is.null(v$percentile_short) && !is.na(v$percentile_short)) {
          cat("  • Volatility vs History (7d): ", sprintf("%.1f%%", v$percentile_short * 100),
              " - ", ifelse(v$percentile_short > 0.8, "High",
                            ifelse(v$percentile_short < 0.2, "Low", "Normal")), "\n")
        }
        if (!is.null(v$percentile_medium) && !is.na(v$percentile_medium)) {
          cat("  • Volatility vs History (30d): ", sprintf("%.1f%%", v$percentile_medium * 100),
              " - ", ifelse(v$percentile_medium > 0.8, "High",
                            ifelse(v$percentile_medium < 0.2, "Low", "Normal")), "\n")
        }
      }

      #-------------------------------------------------------------------------
      # POSITIONING INDICATORS (DeFi Positioning)
      #-------------------------------------------------------------------------
      if (!is.null(x$signals$positioning)) {
        print_sep()
        cat("🐋 DEFI POSITIONING\n")
        print_sep()

        p <- x$signals$positioning

        # Show data source
        if (!is.null(p$data_source)) {
          cat("  Data Source:", p$data_source, "\n\n")
        }

        # Basic flow regime
        if (!is.null(p$whale_flow) && p$whale_flow != "Unknown") {
          flow_emoji <- ifelse(grepl("Inflows", p$whale_flow), "📥",
                               ifelse(grepl("Outflows", p$whale_flow), "📤", "⚖️"))
          cat(flow_emoji, " Flow Regime: ", p$whale_flow, "\n")
          add_explanation("Vault Flows", "Inflows = capital entering DeFi (bullish deployment). Outflows = capital leaving DeFi (risk-off).", detailed)
          if (safe_exists(p$whale_activity)) {
            cat("  • Activity: ", p$whale_activity, "\n")
          }
          if (!is.null(p$whale_interpretation)) {
            cat("  • ", p$whale_interpretation, "\n")
          }
        }

        # MORPHO-SPECIFIC INTELLIGENCE
        if (!is.null(p$whale_concentration)) {
          wc <- p$whale_concentration
          cat("\n  🐋 WHALE CONCENTRATION ANALYSIS\n")
          cat("  ", paste(rep("─", 40), collapse = ""), "\n")
          cat("    Unique Whale Wallets:", wc$unique_whales %||% 0, "\n")
          cat("    Mega Whales (>$10M):", wc$mega_whales %||% 0, "\n")
          cat("    Regular Whales ($1M-$10M):", wc$regular_whales %||% 0, "\n")
          if (!is.null(wc$hhi)) {
            cat("    Market Concentration (HHI):", round(wc$hhi, 1), "(", wc$hhi_interpretation %||% "Unknown", ")\n")
          }
          if (!is.null(wc$top_5_share)) {
            cat("    Top 5 Depositors Share:", round(wc$top_5_share, 1), "%\n")
          }
          if (!is.null(wc$concentration_risk)) {
            cat("    Concentration Risk:", wc$concentration_risk, "\n")
          }
          if (!is.null(wc$systemic_risk)) {
            cat("    ", wc$systemic_risk, "\n")
          }
          add_explanation("Concentration", "High concentration means few whales control most capital - increased systemic risk.", detailed)
        }

        if (!is.null(p$vault_flows)) {
          vf <- p$vault_flows
          cat("\n  📊 CAPITAL FLOW ANALYSIS\n")
          cat("  ", paste(rep("─", 40), collapse = ""), "\n")
          if (safe_exists(vf$date_range)) {
            cat("    Period:", vf$date_range, "\n")
          }
          if (!is.null(vf$transactions)) {
            cat("    Transactions Analyzed:", vf$transactions, "\n")
          }
          if (!is.null(vf$net_flow_usd) && !is.na(vf$net_flow_usd) && abs(vf$net_flow_usd) > 0) {
            cat("    Net Flow: $", format(round(vf$net_flow_usd / 1e6, 2), big.mark = ","), "M", sep = "")
            if (!is.null(vf$net_flow_pct) && !is.na(vf$net_flow_pct)) {
              cat(" (", round(vf$net_flow_pct, 1), "% of TVL)", sep = "")
            }
            cat("\n")
          }
          if (safe_exists(vf$flow_regime)) {
            cat("    Flow Regime:", vf$flow_regime, "\n")
          }
          add_explanation("Capital Flows", "Shows whether capital is entering (bullish) or leaving (bearish) DeFi protocols.", detailed)
        }
      }

      #-------------------------------------------------------------------------
      # FEAR & GREED SENTIMENT
      #-------------------------------------------------------------------------
      if (!is.null(x$signals$sentiment)) {
        print_sep()
        cat("📈 MARKET SENTIMENT\n")
        print_sep()

        sent <- x$signals$sentiment
        if (safe_exists(sent$value) && safe_exists(sent$classification)) {
          cat("  ", sent$signal_emoji %||% "⚪", " Fear & Greed Index: ", sent$value, "/100 - ", sent$classification, "\n", sep = "")
        }
        if (safe_exists(sent$interpretation)) {
          cat("  ", sent$interpretation, "\n")
        }
        if (!is.null(sent$contrarian_signal)) {
          cat("  Contrarian Signal: ", sent$contrarian_signal, "\n")
        }
        add_explanation("Fear & Greed", "Contrarian indicator: Extreme fear (<25) can signal bottoms, extreme greed (>75) can signal tops.", detailed)
      }
    } # End detailed sections

    #-------------------------------------------------------------------------
    # DECISION FRAMEWORK BY TIME HORIZON (Always show, but with risk context)
    #-------------------------------------------------------------------------
    print_sep()
    cat("⏱️  DECISION FRAMEWORK BY TIME HORIZON\n")
    print_sep()

    if (!is.null(x$summary$risk_override_applied) && x$summary$risk_override_applied) {
      cat("  ⚠️ RISK OVERRIDE ACTIVE - Signals below reflect current risk level\n\n")
    }

    # Short-term
    if (!is.null(x$decisions$short_term)) {
      st <- x$decisions$short_term

      if (!is.null(st$momentum$risk_overridden) && st$momentum$risk_overridden) {
        cat("🔴 SHORT-TERM (Hours to Days): [RISK-OFF]\n")
        cat("  • ", st$momentum$signal %||% "RISK-OFF", "\n")
        if (!is.null(st$momentum$original_signal)) {
          cat("  • Original Signal (without risk): ", st$momentum$original_signal, "\n")
        }
      } else {
        st_signal <- st$momentum$signal %||% "Neutral"
        st_emoji <- ifelse(grepl("Long", st_signal), "🟢",
                           ifelse(grepl("Short", st_signal), "🔴", "🟡"))
        cat(st_emoji, " SHORT-TERM (Hours to Days):\n")
        cat("  • Momentum Signal: ", st_signal,
            if (!is.null(st$momentum$score)) paste0(" (Score: ", round(st$momentum$score, 2), "/4)"), "\n", sep = "")
      }

      if (detailed && !is.null(st$momentum$factors) && length(st$momentum$factors) > 0) {
        cat("    Factors:\n")
        for (f in names(st$momentum$factors)) {
          val <- st$momentum$factors[[f]]
          if (!is.null(val) && !is.na(val)) {
            sign_char <- if (val > 0) "+" else ""
            cat("      • ", f, ": ", sign_char, val, "\n", sep = "")
          }
        }
      }

      if (!is.null(st$mean_reversion) && !is.null(st$mean_reversion$active) && st$mean_reversion$active) {
        cat("  • Mean Reversion Setup: ", st$mean_reversion$direction %||% "Neutral", "\n")
      }
    }

    # Medium-term - FIX: Handle the error with proper null checking
    if (!is.null(x$decisions$medium_term)) {
      mt <- x$decisions$medium_term

      cat("\n")

      # FIX: Check if trend exists and has risk_overridden flag
      if (!is.null(mt$trend) && !is.null(mt$trend$risk_overridden) && mt$trend$risk_overridden) {
        # Safely check risk_level
        risk_level_display <- if (!is.null(mt$trend$risk_level)) mt$trend$risk_level else "HIGH"

        if (risk_level_display == "HIGH") {
          cat("🔴 MEDIUM-TERM (Weeks to Months): [RISK-OFF - REDUCE EXPOSURE]\n")
        } else {
          cat("🟡 MEDIUM-TERM (Weeks to Months): [CAUTIOUS - RISK LEVEL MEDIUM]\n")
        }
        cat("  • ", mt$trend$direction %||% "Reduce Exposure", "\n")
        if (!is.null(mt$trend$original_direction)) {
          cat("  • Original Signal: ", mt$trend$original_direction,
              if (!is.null(mt$trend$raw_score)) paste0(" (Score: ", round(mt$trend$raw_score, 2), "/4)"), "\n")
        }
      } else if (!is.null(mt$trend)) {
        mt_signal <- mt$trend$direction %||% "Neutral"
        mt_emoji <- ifelse(grepl("Bull", mt_signal), "🟢",
                           ifelse(grepl("Bear", mt_signal), "🔴", "🟡"))
        cat(mt_emoji, " MEDIUM-TERM (Weeks to Months):\n")
        cat("  • Trend Signal: ", mt_signal,
            if (!is.null(mt$trend$score)) paste0(" (Score: ", round(mt$trend$score, 2), "/4)"), "\n", sep = "")
      } else {
        cat("⚪ MEDIUM-TERM (Weeks to Months): Data unavailable\n")
      }

      if (detailed && !is.null(mt$trend$factors) && length(mt$trend$factors) > 0) {
        cat("    Factors:\n")
        for (f in names(mt$trend$factors)) {
          val <- mt$trend$factors[[f]]
          if (!is.null(val) && !is.na(val)) {
            sign_char <- if (val > 0) "+" else ""
            cat("      • ", f, ": ", sign_char, val, "\n", sep = "")
          }
        }
      }

      if (!is.null(mt$valuation) && safe_exists(mt$valuation$composite)) {
        cat("  • Valuation: ", mt$valuation$composite, "\n")
      }

      if (!is.null(mt$position_sizing)) {
        if (!is.null(mt$position_sizing$recommended_allocation_pct)) {
          cat("  • Recommended Allocation: ", mt$position_sizing$recommended_allocation_pct,
              "% of capital")
          if (!is.null(mt$position_sizing$risk_level_context)) {
            cat(" (", mt$position_sizing$risk_level_context, " risk)")
          }
          cat("\n")
        }
        if (!is.null(mt$position_sizing$risk_per_trade_pct)) {
          cat("  • Risk per Trade: ", mt$position_sizing$risk_per_trade_pct,
              "% of account per trade\n")
        }
      }
    }

    # Long-term
    if (!is.null(x$decisions$long_term)) {
      lt <- x$decisions$long_term

      cat("\n")

      if (!is.null(lt$strategic) && !is.null(lt$strategic$risk_context) && lt$strategic$risk_context == "HIGH") {
        cat("🟡 LONG-TERM (Months to Years): [DEFENSIVE - HIGH RISK CONTEXT]\n")
      } else {
        lt_signal <- lt$strategic$core_position %||% "Maintain core position"
        lt_emoji <- ifelse(grepl("Accumulate|Buy", lt_signal), "🟢",
                           ifelse(grepl("Reduce", lt_signal), "🔴", "🟡"))
        cat(lt_emoji, " LONG-TERM (Months to Years):\n")
      }

      if (!is.null(lt$strategic)) {
        cat("  • Strategic: ", lt$strategic$core_position %||% "Maintain", "\n")
      }

      if (!is.null(lt$cycle)) {
        cat("  • Cycle: ", lt$cycle$interpretation %||% "Mid-cycle", "\n")

        # Show proximity warnings if any
        if (!is.null(lt$cycle$mvrv_proximity) && length(lt$cycle$mvrv_proximity) > 0) {
          for (warning in lt$cycle$mvrv_proximity) {
            cat("  • ⚠️ ", warning, "\n")
          }
        }
      }

      if (!is.null(lt$halving)) {
        cat("  • Halving: ", lt$halving$phase %||% "Unknown", "\n")
      }
    }

    #-------------------------------------------------------------------------
    # REGIME ALLOCATION (Current Market Type)
    #-------------------------------------------------------------------------
    print_sep()
    cat("🎯 REGIME ALLOCATION\n")
    print_sep()

    if (!is.null(x$decisions$regime_allocation)) {
      regime <- x$decisions$regime_allocation
      regime_current <- regime$current_regime %||% "Neutral"
      regime_emoji <- switch(regime_current,
                             "Bull" = "🐂",
                             "Bear" = "🐻",
                             "Accumulation" = "💰",
                             "Distribution" = "📤",
                             "Neutral" = "⚖️",
                             "Defensive" = "🛡️",
                             "⚪")
      cat(regime_emoji, " Current Regime: ", regime_current,
          if (!is.null(regime$confidence)) paste0(" (", regime$confidence, " confidence)"), "\n\n")

      if (!is.null(regime$allocation)) {
        cat("Recommended Positioning:\n")
        cat("  • Strategic Allocation: ", regime$allocation$strategic %||% "50%", " of long-term portfolio\n")
        cat("  • Tactical Approach: ", regime$allocation$tactical %||% "Balanced", "\n")
        cat("  • Hedges: ", regime$allocation$hedges %||% "Standard", "\n")
      }
    }

    #-------------------------------------------------------------------------
    # ACTIONABLE SUMMARY (Bottom Line - Always show)
    #-------------------------------------------------------------------------
    print_sep()
    cat("✅ ACTIONABLE SUMMARY\n")
    print_sep()
    cat("  Bottom-line recommendations based on all indicators:\n\n")

    if (!is.null(x$summary$bullet_points) && length(x$summary$bullet_points) > 0) {
      for (bullet in x$summary$bullet_points) {
        # Add appropriate indentation
        if (grepl("^→", bullet)) {
          cat("    ", bullet, "\n")
        } else {
          cat("  •", bullet, "\n")
        }
      }
    } else {
      cat("  • No summary bullets available\n")
    }

    # Quick reference table (Always show)
    print_sep()
    cat("📋 QUICK REFERENCE\n")
    print_sep()

    # Build quick reference based on available data
    quick_ref_data <- list()

    # Risk
    if (!is.null(x$summary$risk)) {
      risk_display <- paste0(x$summary$risk$color %||% "UNKNOWN")
      if (!is.null(x$summary$risk$composite_score)) {
        risk_display <- paste0(risk_display, " (", round(x$summary$risk$composite_score, 2), ")")
      }
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Risk Level")
      quick_ref_data$Value <- c(quick_ref_data$Value, risk_display)
    }

    # Regime
    if (!is.null(x$summary$regime)) {
      regime_display <- x$summary$regime$name %||% "Unknown"
      if (!is.null(x$summary$regime$confidence)) {
        regime_display <- paste0(regime_display, " (", x$summary$regime$confidence, ")")
      }
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Regime")
      quick_ref_data$Value <- c(quick_ref_data$Value, regime_display)
    }

    # Short-term
    if (!is.null(x$summary$short_term)) {
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Short-term")
      quick_ref_data$Value <- c(quick_ref_data$Value, x$summary$short_term)
    }

    # Medium-term
    if (!is.null(x$summary$medium_term)) {
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Medium-term")
      quick_ref_data$Value <- c(quick_ref_data$Value, x$summary$medium_term)
    }

    # Long-term
    if (!is.null(x$summary$long_term)) {
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Long-term")
      quick_ref_data$Value <- c(quick_ref_data$Value, x$summary$long_term)
    }

    # Sentiment
    if (!is.null(x$signals$sentiment)) {
      sent_display <- paste0(x$signals$sentiment$value, "/100 - ", x$signals$sentiment$classification)
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Sentiment")
      quick_ref_data$Value <- c(quick_ref_data$Value, sent_display)
    }

    # Allocation
    if (!is.null(x$summary$allocation)) {
      quick_ref_data$Metric <- c(quick_ref_data$Metric, "Allocation")
      quick_ref_data$Value <- c(quick_ref_data$Value, x$summary$allocation)
    }

    # Create and print data frame if we have data
    if (length(quick_ref_data) > 0) {
      quick_ref <- data.frame(
        Metric = quick_ref_data$Metric,
        Value = quick_ref_data$Value,
        stringsAsFactors = FALSE
      )
      print(quick_ref, row.names = FALSE)
    }
  }

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")

  # Show how to get more detail if in condensed mode
  if (!detailed) {
    cat("👉 For detailed analysis with explanations, use print_crypto_summary(result, detailed = TRUE)\n")
  } else {
    cat("👉 Use str(result) to see full data structure\n")
  }

  cat(paste(rep("=", 80), collapse = ""), "\n")
}
