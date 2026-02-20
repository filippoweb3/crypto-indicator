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
#' @param whale_api_key Character string containing your Whale Alert API key.
#'   Default \code{NULL}. If provided, will be stored in global environment.
#'   Register at \url{https://whale-alert.io/api}
#' @param include_google_trends Logical. If \code{TRUE}, fetches Google Trends
#'   data for retail sentiment analysis. Default \code{TRUE}.
#'
#' @return A list object of class \code{crypto_framework} containing seven
#'   main sections:
#'
#'   \strong{1. Metadata:}
#'   \itemize{
#'     \item \code{timestamp}: When the analysis was run
#'     \item \code{assets}: Assets analyzed
#'     \item \code{start_date}/\code{end_date}: Data range
#'     \item \code{status}: "success", "pending", or "failed"
#'     \item \code{warning_count}/\code{error_count}: Issue tracking
#'   }
#'
#'   \strong{2. Raw Data:}
#'   \itemize{
#'     \item \code{market}: Output from \code{\link{get_market_data}}
#'     \item \code{derivatives}: Output from \code{\link{get_derivatives}}
#'     \item \code{macro}: Output from \code{\link{get_macro}}
#'     \item \code{positioning}: Output from \code{\link{get_positioning}} (if requested)
#'   }
#'
#'   \strong{3. Indicators:}
#'   \itemize{
#'     \item \code{onchain}: Output from \code{\link{get_onchain_indicators}}
#'     \item \code{volatility}: Output from \code{\link{get_volatility}}
#'   }
#'
#'   \strong{4. Signals:} Latest values from each indicator category
#'   \itemize{
#'     \item \code{onchain}: MVRV, NVT, Puell, composite signal
#'     \item \code{derivatives}: Funding regime, crowded trades, percentiles
#'     \item \code{macro}: Liquidity, rates, dollar, composite risk
#'     \item \code{volatility}: Regime, strategy setup, percentiles
#'     \item \code{positioning}: Whale flows, sentiment (if available)
#'   }
#'
#'   \strong{5. Decisions:} Framework outputs by time horizon
#'   \itemize{
#'     \item \code{short_term}: Momentum score and mean reversion signals
#'     \item \code{medium_term}: Trend score, valuation, position sizing
#'     \item \code{long_term}: Cycle positioning, halving impact, strategic allocation
#'     \item \code{risk_off}: Current risk level (GREEN/YELLOW/RED) with actions
#'     \item \code{regime_allocation}: Market regime with specific positioning
#'   }
#'
#'   \strong{6. Summary:} Actionable bullet points and quick reference
#'
#' @details
#' \strong{Framework Architecture}
#'
#' The framework processes data through five specialized modules, each capturing
#' a different aspect of market dynamics:
#'
#' \describe{
#'   \item{\strong{On-Chain Module}}{Uses \code{\link{get_onchain_indicators}} to
#'     assess fundamental valuation via MVRV, network activity via NVT, and miner
#'     behavior via Puell Multiple. These indicators identify cycle extremes and
#'     accumulation/distribution zones.}
#'
#'   \item{\strong{Derivatives Module}}{Uses \code{\link{get_derivatives}} to
#'     analyze market positioning via open interest, leverage dynamics via funding
#'     rates, and crowd behavior via long/short ratios. Identifies crowded trades
#'     and potential reversal points.}
#'
#'   \item{\strong{Macro Module}}{Uses \code{\link{get_macro}} to capture global
#'     liquidity (M2), interest rates, and dollar strength. These factors drive
#'     the broader risk environment for crypto assets.}
#'
#'   \item{\strong{Volatility Module}}{Uses \code{\link{get_volatility}} to
#'     identify optimal trading strategies based on market conditions—
#'     trend following in low vol, mean reversion in high vol.}
#'
#'   \item{\strong{Positioning Module}}{Uses \code{\link{get_positioning}} to
#'     track whale movements (smart money) and retail sentiment (Google Trends)
#'     as contrarian indicators at extremes.}
#' }
#'
#' \strong{Decision Framework Methodology}
#'
#' \emph{Short-Term (Hours to Days)}
#' \itemize{
#'   \item Combines derivatives positioning (funding regime) with recent price momentum
#'   \item Momentum score range: -4 to +4
#'   \item Signals: Strong Long (+2 to +4), Long (+1), Neutral (0), Short (-1), Strong Short (-2 to -4)
#'   \item Volatility regime determines if mean reversion setups are active
#' }
#'
#' \emph{Medium-Term (Weeks to Months)}
#' \itemize{
#'   \item On-chain valuation (MVRV) contributes up to ±2 points
#'   \item Macro composite risk contributes up to ±2 points (scaled)
#'   \item Trend score range: -4 to +4
#'   \item Position sizing scales with conviction (25-75% allocation)
#'   \item Risk per trade adjusts based on allocation (1.0-1.5%)
#' }
#'
#' \emph{Long-Term (Months to Years)}
#' \itemize{
#'   \item Cycle positioning based on MVRV regimes (Extreme Bottom → Accumulation)
#'   \item Halving cycle phase (pre-halving, post-halving, mid-cycle)
#'   \item Strategic guidance: Accumulate, Maintain, or Reduce core position
#' }
#'
#' \strong{Risk-Off Framework}
#'
#' Three-tier risk system that overrides other signals during dangerous conditions:
#'
#' \describe{
#'   \item{\strong{GREEN} (Low Risk)}{
#'     \itemize{
#'       \item Conditions: Volatility normal, macro positive, no crowded trades
#'       \item Actions: Maintain full positions, normal stop losses
#'     }
#'   }
#'   \item{\strong{YELLOW} (Medium Risk)}{
#'     \itemize{
#'       \item Conditions: Elevated volatility OR negative macro OR crowded longs
#'       \item Actions: Reduce leverage, tighten stops, take partial profits
#'     }
#'   }
#'   \item{\strong{RED} (High Risk)}{
#'     \itemize{
#'       \item Conditions: Extreme volatility (>1sd) OR severe macro headwinds (< -0.5)
#'       \item Actions: Move to stablecoins, close leveraged positions, wait
#'     }
#'   }
#' }
#'
#' \strong{Regime Allocation}
#'
#' The framework classifies the current market into one of five regimes:
#'
#' \describe{
#'   \item{\strong{Bull}}{Expanding liquidity + bull market MVRV}
#'   \itemize{
#'     \item Strategic: 75-100% allocation
#'     \item Tactical: Trend following
#'     \item Hedges: Minimal
#'   }
#'   \item{\strong{Bear}}{Contracting liquidity + bear market MVRV}
#'   \itemize{
#'     \item Strategic: 25-50% allocation
#'     \item Tactical: Mean reversion
#'     \item Hedges: Consider hedges
#'   }
#'   \item{\strong{Accumulation}}{Extreme bottom MVRV}
#'   \itemize{
#'     \item Strategic: 50-75% DCA
#'     \item Tactical: Scale in
#'     \item Hedges: Light
#'   }
#'   \item{\strong{Distribution}}{Extreme top MVRV}
#'   \itemize{
#'     \item Strategic: <25% allocation
#'     \item Tactical: Take profits
#'     \item Hedges: Increase
#'   }
#'   \item{\strong{Neutral}}{Mixed signals}
#'   \itemize{
#'     \item Strategic: 50% allocation
#'     \item Tactical: Balanced
#'     \item Hedges: Standard
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item The framework is optimized for Bitcoin but can analyze other assets
#'     with sufficient data (on-chain metrics like Puell are Bitcoin-specific)
#'   \item API keys are stored in the global environment; manage them carefully
#'   \item First run may be slow due to data caching and multiple API calls
#'   \item Internet connection required for all data sources
#'   \item Some indicators (e.g., Puell Multiple) are Bitcoin-specific
#' }
#'
#' @section Expected Output:
#' When you run the framework, you'll see progress messages:
#' \preformatted{
#' 📊 Collecting market data...
#'   ✅ Retrieved 3287 days of data
#' ⛓️  Calculating on-chain indicators...
#'   ✅ MVRV: Bear Market
#' 📈 Fetching derivatives data...
#'   Ticker: BTCUSDT
#'   ✅ Funding regime: Neutral
#' 🌎 Fetching macro indicators...
#'   ✅ Liquidity: Stable
#' 📊 Calculating volatility metrics...
#'   ✅ Volatility regime: Low
#' 🐋 Fetching whale and sentiment data...
#'   ✅ Whale flow: Net Outflows (Bullish Bias)
#' 🎯 Generating decision framework...
#'
#' ✅ Framework complete! Use print_crypto_summary() to view results
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage (Bitcoin only)
#' result <- crypto_predictive_framework()
#'
#' # View summary
#' print_crypto_summary(result)
#'
#' # With all data sources
#' result <- crypto_predictive_framework(
#'   assets = c("Bitcoin"),
#'   start_date = "2015-01-01",
#'   fred_api_key = "your_fred_key",
#'   whale_api_key = "your_whale_key",
#'   include_google_trends = TRUE
#' )
#'
#' # Access specific components
#' result$signals$onchain$mvrv$regime        # Current MVRV regime
#' result$decisions$risk_off$color           # Risk level (GREEN/YELLOW/RED)
#' result$decisions$regime_allocation$allocation$strategic  # Recommended allocation
#'
#' # Extract for custom reporting
#' report_data <- data.frame(
#'   date = Sys.Date(),
#'   mvrv = result$signals$onchain$mvrv$value,
#'   mvrv_regime = result$signals$onchain$mvrv$regime,
#'   risk_level = result$decisions$risk_off$color,
#'   allocation = result$decisions$regime_allocation$allocation$strategic
#' )
#'
#' # Run for Ethereum (limited on-chain metrics)
#' eth_result <- crypto_predictive_framework(
#'   assets = c("Ethereum"),
#'   start_date = "2017-01-01"
#' )
#'
#' # Compare multiple timeframes
#' cat("Short-term:", result$decisions$short_term$momentum$signal, "\n")
#' cat("Medium-term:", result$decisions$medium_term$trend$direction, "\n")
#' cat("Long-term:", result$decisions$long_term$strategic$core_position, "\n")
#' }
#'
#' @section Error Handling:
#' The framework is designed to be resilient:
#' \itemize{
#'   \item If market data fails, the framework returns early with status "failed"
#'   \item Other modules that fail generate warnings but don't stop execution
#'   \item Missing data results in \code{NULL} values with appropriate warnings
#'   \item API failures are caught and logged as warnings, not errors
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_market_data}} for raw data acquisition
#'   \item \code{\link{get_onchain_indicators}} for on-chain metrics
#'   \item \code{\link{get_derivatives}} for derivatives analysis
#'   \item \code{\link{get_macro}} for macroeconomic indicators
#'   \item \code{\link{get_volatility}} for volatility regimes
#'   \item \code{\link{get_positioning}} for whale and sentiment data
#'   \item \code{\link{print_crypto_summary}} for formatted output
#' }
#'
#' @references
#' \itemize{
#'   \item Woo, W. (2019). "On-Chain Indicators for Bitcoin Valuation"
#'   \item Puell, D. (2019). "The Puell Multiple: A Bitcoin Mining Indicator"
#'   \item Checkmate (2020). "The Bitcoin On-Chain Market Cycle Guide"
#'   \item Binance Exchange (2024). "Perpetual Futures Guide"
#'   \item Federal Reserve Economic Data (FRED): \url{https://fred.stlouisfed.org/}
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr filter slice_tail case_when
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






#' Print Comprehensive Crypto Framework Summary
#'
#' @description
#' Displays a formatted, easy-to-understand summary of the crypto predictive framework
#' results. The output is designed to be self-explanatory and shareable with team
#' members, stakeholders, or anyone needing actionable crypto market insights.
#'
#' @param x A crypto_framework object returned by \code{\link{crypto_predictive_framework}}
#'
#' @return None. Prints formatted summary to console.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- crypto_predictive_framework()
#' print_crypto_summary(result)
#' }
print_crypto_summary <- function(x) {

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
        return(sprintf("%.2f", value))
      }
      return(as.character(value))
    }, error = function(e) {
      return(default)
    })
  }

  # Helper function to add explanatory notes
  add_explanation <- function(text, explanation) {
    cat("  📝 ", explanation, "\n")
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
    # RISK-OFF SIGNAL (Most Important - Shown First)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    risk_color <- safe_extract(x, c("decisions", "risk_off", "color"), "UNKNOWN")
    risk_emoji <- ifelse(risk_color == "GREEN", "🟢",
                         ifelse(risk_color == "YELLOW", "🟡",
                                ifelse(risk_color == "RED", "🔴", "⚪")))
    cat(risk_emoji, " RISK-OFF SIGNAL: [", risk_color, "] ", risk_emoji, "\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")

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

    #-------------------------------------------------------------------------
    # ON-CHAIN INDICATORS (Fundamental Valuation)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("⛓️  ON-CHAIN INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  These metrics analyze blockchain data to determine if Bitcoin is\n")
    cat("  undervalued or overvalued based on network activity and miner behavior.\n\n")

    if (!is.null(x$signals$onchain)) {
      oc <- x$signals$onchain

      # MVRV
      if (!is.null(oc$mvrv)) {
        mvrv_emoji <- ifelse(grepl("Buy|Accumulate", oc$mvrv$signal), "🟢",
                             ifelse(grepl("Sell|Reduce", oc$mvrv$signal), "🔴", "🟡"))
        cat(mvrv_emoji, " MVRV Ratio: ", safe_extract(oc, c("mvrv", "value")),
            " [", safe_extract(oc, c("mvrv", "regime")), "]\n", sep = "")
        cat("     Signal: ", safe_extract(oc, c("mvrv", "signal")), "\n")
        add_explanation("MVRV", "Compares current price to average purchase price of all coins. <0.7 = extreme bottom, >1.75 = extreme top.")
      }

      # NVT
      if (!is.null(oc$nvt)) {
        nvt_emoji <- ifelse(oc$nvt$regime == "Undervalued", "🟢",
                            ifelse(oc$nvt$regime == "Overvalued", "🔴", "🟡"))
        cat(nvt_emoji, " NVT Ratio: ", safe_extract(oc, c("nvt", "value")),
            " [", safe_extract(oc, c("nvt", "regime")), "]\n", sep = "")
        add_explanation("NVT", "Like P/E ratio for stocks. Low = network usage high relative to value, High = value exceeds network usage.")
      }

      # Puell Multiple
      if (!is.null(oc$puell)) {
        puell_emoji <- ifelse(grepl("Capitulation", oc$puell$regime), "🟢",
                              ifelse(grepl("Selling", oc$puell$regime), "🔴", "🟡"))
        cat(puell_emoji, " Puell Multiple: ", safe_extract(oc, c("puell", "value")),
            " [", safe_extract(oc, c("puell", "regime")), "]\n", sep = "")
        add_explanation("Puell", "Measures miner profitability. Low = miners selling at a loss (capitulation), High = miners exceptionally profitable (selling pressure).")
      }

      # Composite
      if (!is.null(oc$composite)) {
        composite_emoji <- ifelse(oc$composite == "Strong Buy", "🟢🟢",
                                  ifelse(oc$composite == "Strong Sell", "🔴🔴", "🟡"))
        cat(composite_emoji, " Composite Signal: ", oc$composite, "\n")
        add_explanation("Composite", "Combines MVRV and Puell for higher conviction signals.")
      }
    }

    #-------------------------------------------------------------------------
    # DERIVATIVES INDICATORS (Market Positioning)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📈 DERIVATIVES INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  These metrics show how traders are positioned in the futures market,\n")
    cat("  revealing leverage extremes and potential crowded trades.\n\n")

    if (!is.null(x$signals$derivatives)) {
      d <- x$signals$derivatives

      funding_emoji <- ifelse(d$funding_regime %in% c("Bullish", "Neutral"), "🟢",
                              ifelse(d$funding_regime %in% c("Bearish"), "🟡", "🔴"))
      cat(funding_emoji, " Funding Regime: ", safe_extract(d, "funding_regime"), "\n")
      add_explanation("Funding", "Shows who's paying whom to keep positions open. Positive = longs pay shorts (bullish sentiment). Extreme values signal crowded trades.")

      cat("  • OI Percentile: ", sprintf("%.1f%%", as.numeric(d$oi_percentile) * 100),
          " - % of time open interest has been lower than current\n")
      cat("  • Funding Percentile: ", sprintf("%.1f%%", as.numeric(d$funding_percentile) * 100),
          " - % of time funding has been lower than current\n")

      if (!is.null(d$crowded_long) && d$crowded_long) {
        cat("  ⚠️  CROWDED LONG - Extreme long positioning, vulnerable to liquidation cascade\n")
      }
      if (!is.null(d$crowded_short) && d$crowded_short) {
        cat("  ⚠️  CROWDED SHORT - Extreme short positioning, primed for short squeeze\n")
      }

      cat("  • Interpretation: ", safe_extract(d, "interpretation"), "\n")
    }

    #-------------------------------------------------------------------------
    # MACRO INDICATORS (Global Economic Context)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("🌎 MACRO INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  These indicators show the broader economic environment that historically\n")
    cat("  influences crypto markets (liquidity, interest rates, dollar strength).\n\n")

    if (!is.null(x$signals$macro)) {
      m <- x$signals$macro

      liq_emoji <- ifelse(m$liquidity_regime == "Expanding", "🟢",
                          ifelse(m$liquidity_regime == "Contracting", "🔴", "🟡"))
      cat(liq_emoji, " Liquidity: ", safe_extract(m, "liquidity_regime"), "\n")
      add_explanation("Liquidity", "Money supply growth. Expanding (>5% YoY) = bullish for crypto, Contracting (<0%) = bearish.")

      rate_emoji <- ifelse(grepl("Low Rates", m$rate_regime), "🟢",
                           ifelse(grepl("High Rates", m$rate_regime), "🔴", "🟡"))
      cat(rate_emoji, " Rates: ", safe_extract(m, "rate_regime"), "\n")
      add_explanation("Rates", "10-year Treasury yield. Low rates (<1%) = risk-on, High rates (>3%) = risk-off (competes with crypto).")

      dollar_emoji <- ifelse(grepl("Weak Dollar", m$dollar_regime), "🟢",
                             ifelse(grepl("Strong Dollar", m$dollar_regime), "🔴", "🟡"))
      cat(dollar_emoji, " Dollar: ", safe_extract(m, "dollar_regime"), "\n")
      add_explanation("Dollar", "DXY index. Weak dollar (<90) = bullish for crypto, Strong dollar (>100) = bearish.")

      cat("\n  • Composite Risk Score: ", safe_extract(m, "composite_risk"),
          " (Scale -1 to +1, higher = more bullish)\n")
      cat("  • Overall Bias: ", safe_extract(m, "risk_bias"), "\n")
    }

    #-------------------------------------------------------------------------
    # VOLATILITY INDICATORS (Market Conditions)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📊 VOLATILITY INDICATORS\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  Volatility regimes determine which trading strategies work best.\n\n")

    if (!is.null(x$signals$volatility)) {
      v <- x$signals$volatility

      vol_emoji <- ifelse(grepl("Low", v$regime), "🟢",
                          ifelse(grepl("High", v$regime), "🟡", "🔴"))
      cat(vol_emoji, " Volatility Regime: ", safe_extract(v, "regime"), "\n")
      cat("  • Current Environment: ", safe_extract(v, "current_regime"), "\n")
      cat("  • Best Strategy: ", safe_extract(v, "strategy"), "\n")
      cat("  • Volatility vs History (7d): ", sprintf("%.1f%%", as.numeric(v$percentile_short) * 100),
          " - ", ifelse(as.numeric(v$percentile_short) > 0.8, "High",
                        ifelse(as.numeric(v$percentile_short) < 0.2, "Low", "Normal")), "\n")
      cat("  • Volatility vs History (30d): ", sprintf("%.1f%%", as.numeric(v$percentile_medium) * 100),
          " - ", ifelse(as.numeric(v$percentile_medium) > 0.8, "High",
                        ifelse(as.numeric(v$percentile_medium) < 0.2, "Low", "Normal")), "\n")
    }

    #-------------------------------------------------------------------------
    # POSITIONING INDICATORS (Whales & Retail)
    #-------------------------------------------------------------------------
    if (!is.null(x$signals$positioning)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🐋 POSITIONING & SENTIMENT\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")
      cat("  Whale transactions show what 'smart money' is doing.\n")
      cat("  Google Trends shows retail interest (contrarian indicator at extremes).\n\n")

      p <- x$signals$positioning

      if (!is.null(p$whale_flow)) {
        whale_emoji <- ifelse(grepl("Accumulation|Bullish", p$whale_flow), "🟢",
                              ifelse(grepl("Selling|Bearish", p$whale_flow), "🔴", "🟡"))
        cat(whale_emoji, " Whale Flow: ", safe_extract(p, "whale_flow"), "\n")
        add_explanation("Whale Flow", "Inflows = whales sending to exchanges (potential selling). Outflows = whales moving to cold storage (accumulation).")
        cat("  • Interpretation: ", safe_extract(p, "whale_interpretation"), "\n")
        cat("  • Activity: ", safe_extract(p, "whale_activity"), "\n")
      }

      if (!is.null(p$sentiment)) {
        sent_emoji <- ifelse(grepl("Low Interest", p$sentiment$signal), "🟢",
                             ifelse(grepl("Extreme Interest", p$sentiment$signal), "🔴", "🟡"))
        cat(sent_emoji, " Google Trends: ", safe_extract(p, c("sentiment", "score")),
            " [", safe_extract(p, c("sentiment", "signal")), "]\n", sep = "")
        add_explanation("Google Trends", "Retail interest. <20 = apathy (potential bottom), >80 = euphoria (potential top).")
      }
    }

    #-------------------------------------------------------------------------
    # DECISION FRAMEWORK BY TIME HORIZON
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("⏱️  DECISION FRAMEWORK BY TIME HORIZON\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  Different strategies for different timeframes based on current signals.\n\n")

    # Short-term
    if (!is.null(x$decisions$short_term)) {
      st <- x$decisions$short_term
      st_signal <- safe_extract(st, c("momentum", "signal"), "Neutral")
      st_emoji <- ifelse(grepl("Long", st_signal), "🟢",
                         ifelse(grepl("Short", st_signal), "🔴", "🟡"))
      cat(st_emoji, " SHORT-TERM (Hours to Days):\n")
      cat("  • Momentum Signal: ", st_signal, " (Score: ", safe_extract(st, c("momentum", "score"), "0"), "/4)\n", sep = "")
      add_explanation("Momentum Score", "Combines funding rates (derivatives) with recent price action. Higher = more bullish.")

      if (!is.null(st$mean_reversion) && !is.null(st$mean_reversion$active) && st$mean_reversion$active) {
        cat("  • Mean Reversion Setup: ", safe_extract(st, c("mean_reversion", "direction"), "Neutral"), "\n")
        add_explanation("Mean Reversion", "Volatility compression suggests range-bound trading - fade the moves.")
      }
    }

    # Medium-term
    if (!is.null(x$decisions$medium_term)) {
      mt <- x$decisions$medium_term
      mt_signal <- safe_extract(mt, c("trend", "direction"), "Neutral")
      mt_emoji <- ifelse(grepl("Bull", mt_signal), "🟢",
                         ifelse(grepl("Bear", mt_signal), "🔴", "🟡"))
      cat("\n", mt_emoji, " MEDIUM-TERM (Weeks to Months):\n", sep = "")
      cat("  • Trend Signal: ", mt_signal, " (Score: ", safe_extract(mt, c("trend", "score"), "0"), "/4)\n", sep = "")
      add_explanation("Trend Score", "Combines on-chain valuation (MVRV) with macro conditions. Higher = stronger bull trend.")

      cat("  • Valuation: ", safe_extract(mt, c("valuation", "composite"), "Neutral"), "\n")

      # EXPLANATION OF ALLOCATION PERCENTAGES
      alloc_pct <- safe_extract(mt, c("position_sizing", "recommended_allocation_pct"), "50")
      cat("  • Recommended Allocation: ", alloc_pct, "% of capital for this timeframe\n")

      if (as.numeric(alloc_pct) >= 75) {
        add_explanation("High Allocation", "Strong conviction - can use full position sizes")
      } else if (as.numeric(alloc_pct) >= 65) {
        add_explanation("Moderate-High Allocation", "Good conviction - near full positions")
      } else if (as.numeric(alloc_pct) >= 50) {
        add_explanation("Moderate Allocation", "Balanced - half-sized positions")
      } else if (as.numeric(alloc_pct) >= 35) {
        add_explanation("Low-Moderate Allocation", "Reduced exposure - one-third positions")
      } else {
        add_explanation("Low Allocation", "Minimal exposure - mostly cash")
      }

      risk_pct <- safe_extract(mt, c("position_sizing", "risk_per_trade_pct"), "1.5")
      cat("  • Risk per Trade: ", risk_pct, "% of account per trade\n")
      add_explanation("Risk per Trade", "How much of your account to risk on any single trade. Lower in high conviction? No - actually lower when allocation is high to manage overall risk.")
    }

    # Long-term
    if (!is.null(x$decisions$long_term)) {
      lt <- x$decisions$long_term
      lt_signal <- safe_extract(lt, c("strategic", "core_position"), "Maintain core position")
      lt_emoji <- ifelse(grepl("Accumulate|Buy", lt_signal), "🟢",
                         ifelse(grepl("Reduce", lt_signal), "🔴", "🟡"))
      cat("\n", lt_emoji, " LONG-TERM (Months to Years):\n", sep = "")
      cat("  • Strategic: ", lt_signal, "\n")
      add_explanation("Strategic", "Long-term positioning based on multi-year cycle analysis.")

      cat("  • Cycle: ", safe_extract(lt, c("cycle", "interpretation"), "Mid-cycle"), "\n")
      cat("  • Halving: ", safe_extract(lt, c("halving", "phase"), "Unknown"), "\n")
      add_explanation("Halving", "Bitcoin's 4-year supply cut cycle. Post-halving years historically strongest.")
    }

    #-------------------------------------------------------------------------
    # REGIME ALLOCATION (Current Market Type)
    #-------------------------------------------------------------------------
    if (!is.null(x$decisions$regime_allocation)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🎯 REGIME ALLOCATION\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")
      cat("  The overall market regime determines the appropriate playbook.\n\n")

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

        # EXPLANATION OF STRATEGIC ALLOCATION PERCENTAGES
        strategic <- safe_extract(regime, c("allocation", "strategic"), "50%")
        cat("  • Strategic Allocation: ", strategic, " of long-term portfolio\n")

        if (grepl("75-100%", strategic)) {
          add_explanation("Strategic", "Bull market - fully invested, ride the trend")
        } else if (grepl("50-75%", strategic)) {
          add_explanation("Strategic", "Accumulation zone - building position gradually")
        } else if (grepl("25-50%", strategic)) {
          add_explanation("Strategic", "Bear market - defensive, mostly cash")
        } else if (grepl("<25%", strategic)) {
          add_explanation("Strategic", "Distribution zone - taking profits, minimal exposure")
        } else if (grepl("50%", strategic)) {
          add_explanation("Strategic", "Neutral - balanced, waiting for clearer signals")
        }

        tactical <- safe_extract(regime, c("allocation", "tactical"), "Balanced")
        cat("  • Tactical Approach: ", tactical, "\n")

        if (tactical == "Trend following") {
          add_explanation("Tactical", "Buy dips, hold through volatility, let profits run")
        } else if (tactical == "Mean reversion") {
          add_explanation("Tactical", "Sell rallies, buy dips, range-bound trading")
        } else if (tactical == "Scale in") {
          add_explanation("Tactical", "DCA into positions, don't chase")
        } else if (tactical == "Take profits") {
          add_explanation("Tactical", "Reduce size on strength, move to cash")
        } else {
          add_explanation("Tactical", "Balanced approach - mix of strategies")
        }

        hedges <- safe_extract(regime, c("allocation", "hedges"), "Standard")
        cat("  • Hedges: ", hedges, "\n")

        if (hedges == "Minimal") {
          add_explanation("Hedges", "No hedging needed in strong bull trend")
        } else if (hedges == "Consider hedges") {
          add_explanation("Hedges", "Look at inverse products or options for protection")
        } else if (hedges == "Light") {
          add_explanation("Hedges", "Small hedge positions during accumulation")
        } else if (hedges == "Increase") {
          add_explanation("Hedges", "Add hedges to protect profits")
        } else {
          add_explanation("Hedges", "Standard portfolio hedges")
        }
      }
    }

    #-------------------------------------------------------------------------
    # ACTIONABLE SUMMARY (Bottom Line)
    #-------------------------------------------------------------------------
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("✅ ACTIONABLE SUMMARY\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat("  Bottom-line recommendations based on all indicators:\n\n")

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
      Meaning = c(
        "Days timeframe - momentum trading",
        "Weeks to months - trend following",
        "Years - strategic positioning",
        "Current market risk level",
        "Overall market regime"
      ),
      stringsAsFactors = FALSE
    )

    print(quick_ref, row.names = FALSE)
  }

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("👉 Use str(result) to see full data structure\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
}
