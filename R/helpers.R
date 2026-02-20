#' Retrieve historical cryptocurrency market data
#'
#' @description
#' Fetches historical price, volume, and market capitalization data for specified
#' cryptocurrencies from CoinMarketCap via the crypto2 package. This function serves
#' as the primary data acquisition layer for the crypto predictive framework, providing
#' consistent OHLCV (Open, High, Low, Close, Volume) data for subsequent indicator
#' calculations.
#'
#' @param assets Character vector of cryptocurrency names to fetch data for.
#'   Default is c("Bitcoin"). Names must match CoinMarketCap naming conventions
#'   (e.g., "Bitcoin", "Ethereum", "Cardano"). Case-sensitive.
#' @param start_date Start date for historical data collection in "YYYY-MM-DD" format.
#'   Default is "2015-01-01". Data will be fetched from this date to present.
#'
#' @return A list object of class \code{market_data} containing:
#'   \describe{
#'     \item{data}{A data frame with the following columns:
#'       \itemize{
#'         \item \code{timestamp}: Date of the observation
#'         \item \code{slug}: Asset identifier (e.g., "bitcoin")
#'         \item \code{symbol}: Trading symbol (e.g., "BTC")
#'         \item \code{name}: Full asset name (e.g., "Bitcoin")
#'         \item \code{open}, \code{high}, \code{low}, \code{close}: OHLC prices in USD
#'         \item \code{volume}: Trading volume in USD
#'         \item \code{market_cap}: Market capitalization in USD
#'       }
#'     }
#'     \item{assets}{Character vector of requested asset names (echoed input)}
#'     \item{start_date}{Requested start date (echoed input)}
#'     \item{last_update}{System date when data was fetched}
#'   }
#'
#' @details
#' The function uses \code{crypto2::crypto_history()} which sources data from
#' CoinMarketCap's historical API. Key characteristics:
#' \itemize{
#'   \item Data frequency: Daily
#'   \item Price currency: USD
#'   \item Coverage: From 2013 for major assets, later for smaller assets
#'   \item Rate limiting: Respects API rate limits (use responsibly)
#' }
#'
#' @note
#' \itemize{
#'   \item Internet connection required
#'   \item First call may be slow as it caches coin list
#'   \item Some assets may have limited historical data before their listing date
#'   \item Market capitalization is as reported by CoinMarketCap
#' }
#'
#' @examples
#' \dontrun{
#' # Get Bitcoin data from 2020 onwards
#' btc_data <- get_market_data(assets = "Bitcoin", start_date = "2020-01-01")
#'
#' # Get multiple assets
#' multi_asset <- get_market_data(
#'   assets = c("Bitcoin", "Ethereum"),
#'   start_date = "2018-01-01"
#' )
#'
#' # Access the data
#' head(multi_asset$data)
#'
#' # Check metadata
#' cat("Assets:", paste(multi_asset$assets, collapse = ", "), "\n")
#' cat("Last updated:", multi_asset$last_update)
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_onchain_indicators}} for deriving on-chain metrics from this data
#'   \item \code{\link{get_volatility}} for volatility calculations
#'   \item \code{crypto2::crypto_history()} for underlying API details
#' }
#'
#' @references
#' CoinMarketCap API Documentation: \url{https://coinmarketcap.com/api/}
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom crypto2 crypto_list crypto_history
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
