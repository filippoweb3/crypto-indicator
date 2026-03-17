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
#'     \item{data}{A data frame with OHLCV and market cap data}
#'     \item{assets}{Character vector of requested asset names}
#'     \item{start_date}{Requested start date}
#'     \item{last_update}{System date when data was fetched}
#'   }
#'
#' @details
#' The function uses \code{crypto2::crypto_history()} which sources data from
#' CoinMarketCap's historical API. Data is returned at daily frequency in USD.
#' Coverage begins from 2013 for major assets, with later start dates for smaller assets.
#'
#' @note
#' \itemize{
#'   \item Internet connection required
#'   \item First call may be slow due to coin list caching
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
#' }
#'
#' @seealso
#' \code{\link{get_onchain_indicators}} for deriving on-chain metrics
#' \code{\link{get_volatility}} for volatility calculations
#'
#' @references CoinMarketCap API Documentation: \url{https://coinmarketcap.com/api/}
#'
#' @export
get_market_data <- function(assets = c("Bitcoin"), start_date = "2015-01-01"){

  coin_list <- crypto2::crypto_list() %>% dplyr::filter(name %in% assets)
  data <- crypto2::crypto_history(coin_list = coin_list, start_date = start_date)

  if (!"market_cap" %in% names(data)) {
    data$market_cap <- data$close * 19991700
  }

  return(list(
    data = data,
    assets = assets,
    start_date = start_date,
    last_update = Sys.Date()
  ))
}


#' Calculate Bitcoin Daily Issuance Based on Halving Schedule
#'
#' @description
#' Computes the theoretical daily Bitcoin issuance (new coins created) for any given date
#' based on Bitcoin's predetermined halving schedule. This function models Bitcoin's
#' monetary policy where block rewards are cut in half approximately every four years.
#'
#' @param target_date A date object or character string in "YYYY-MM-DD" format for which
#'   to calculate the daily issuance.
#'
#' @return A numeric value representing the estimated number of new Bitcoins mined per day
#'   on the target date. Returns 0 for dates before the first block (2009-01-03).
#'
#' @details
#' Bitcoin's issuance follows a strict, predetermined schedule:
#' \itemize{
#'   \item \strong{2009-01-03 to 2012-11-27}: 50 BTC/block × 144 blocks/day = 7,200 BTC/day
#'   \item \strong{2012-11-28 to 2016-07-08}: 25 BTC/block × 144 blocks/day = 3,600 BTC/day
#'   \item \strong{2016-07-09 to 2020-05-10}: 12.5 BTC/block × 144 blocks/day = 1,800 BTC/day
#'   \item \strong{2020-05-11 to 2024-04-19}: 6.25 BTC/block × 144 blocks/day = 900 BTC/day
#'   \item \strong{2024-04-20 to present}: 3.125 BTC/block × 144 blocks/day = 450 BTC/day
#' }
#'
#' The calculation assumes consistent 10-minute block intervals (144 blocks per day).
#' Actual daily issuance may vary slightly due to block time variance, orphaned blocks,
#' and difficulty adjustments, but averages converge to this theoretical value over time.
#'
#' @examples
#' \dontrun{
#' get_btc_issuance("2011-01-01")  # 7,200 BTC/day
#' get_btc_issuance("2022-01-01")  # 900 BTC/day
#' get_btc_issuance(Sys.Date())    # 450 BTC/day
#' }
#'
#' @seealso \code{\link{get_onchain_indicators}} which uses this to calculate Puell Multiple
#'
#' @references
#' \itemize{
#'   \item Nakamoto, S. (2008). Bitcoin: A Peer-to-Peer Electronic Cash System
#'   \item Bitcoin Block Halving Schedule: \url{https://www.bitcoinblockhalf.com}
#' }
#'
#' @export
get_btc_issuance <- function(target_date) {

  if (is.character(target_date)) {
    target_date <- as.Date(target_date)
  }

  halvings <- data.frame(
    date = as.Date(c("2009-01-03", "2012-11-28", "2016-07-09", "2020-05-11", "2024-04-20")),
    reward = c(50, 25, 12.5, 6.25, 3.125)
  )

  if (target_date < min(halvings$date)) {
    return(0)
  }

  current_reward <- halvings %>%
    filter(date <= target_date) %>%
    slice_tail(n = 1) %>%
    pull(reward)

  return(current_reward * 144)
}


#' Calculate On-Chain Indicators for Cryptocurrency Analysis
#'
#' @description
#' Derives critical on-chain metrics from price and volume data to assess market
#' valuation, miner behavior, and network activity. These indicators form the
#' fundamental analysis layer of the crypto predictive framework, providing
#' insights into market cycles and potential turning points.
#'
#' @param data A data frame containing cryptocurrency market data, typically the
#'   output from \code{\link{get_market_data()}}. Must contain columns:
#'   \code{timestamp}, \code{slug}, \code{close}, \code{volume}, and \code{market_cap}.
#'
#' @return The input data frame augmented with calculated columns:
#'   \describe{
#'     \item{\code{returns, log_returns}}{Daily return metrics}
#'     \item{\code{volume_ma_7, price_ma_50, price_ma_200}}{Moving averages}
#'     \item{\code{nvt, nvts}}{Network Value to Transactions ratios}
#'     \item{\code{realized_price, mvrv}}{Realized price and MVRV ratio}
#'     \item{\code{issuance, issuance_usd, puell_multiple}}{Miner-related metrics}
#'     \item{\code{nvt_regime, mvrv_regime, puell_signal, regime_combi}}{Signal regimes}
#'   }
#'
#' @details
#' \strong{MVRV Ratio (Market Value to Realized Value)}
#'
#' Compares current market price to average acquisition cost (approximated using 200-day SMA):
#' \itemize{
#'   \item \strong{< 0.7}: Extreme Bottom - Market significantly below cost basis
#'   \item \strong{0.7 - 0.9}: Bear Market - Trading below cost basis
#'   \item \strong{0.9 - 1.75}: Bull Market - Healthy premium to cost basis
#'   \item \strong{> 1.75}: Extreme Top - Excessive unrealized profits
#' }
#'
#' \strong{NVT Ratio (Network Value to Transactions)}
#'
#' Analogous to P/E ratio in equities:
#' \itemize{
#'   \item \strong{< 30}: Undervalued - Network usage high relative to value
#'   \item \strong{30 - 70}: Neutral - Balanced valuation
#'   \item \strong{> 70}: Overvalued - Value exceeds network usage
#' }
#'
#' \strong{Puell Multiple}
#'
#' Measures miner revenue relative to 365-day moving average:
#' \itemize{
#'   \item \strong{< 0.7}: Miner Capitulation - Miners selling at a loss
#'   \item \strong{0.7 - 2.0}: Neutral - Normal miner profitability
#'   \item \strong{> 2.0}: Miner Selling Pressure - Miners exceptionally profitable
#' }
#'
#' \strong{Composite Signal}
#'
#' Combines MVRV and Puell for higher conviction:
#' \itemize{
#'   \item \strong{Strong Buy}: MVRV Extreme Bottom + Puell Miner Capitulation
#'   \item \strong{Strong Sell}: MVRV Extreme Top + Puell Miner Selling Pressure
#'   \item \strong{Neutral}: All other combinations
#' }
#'
#' @note
#' \itemize{
#'   \item Realized price is approximated using 200-day SMA; true realized price requires UTXO data
#'   \item Puell Multiple is Bitcoin-specific
#'   \item First 200 days will have NA values for moving averages
#' }
#'
#' @examples
#' \dontrun{
#' market_data <- get_market_data("Bitcoin", "2015-01-01")
#' onchain_data <- get_onchain_indicators(market_data$data)
#' }
#'
#' @seealso
#' \code{\link{get_market_data}} for input data
#' \code{\link{get_btc_issuance}} for issuance calculation
#'
#' @references
#' \itemize{
#'   \item MVRV: Murad Mahmudov & Adam Taché (2019)
#'   \item NVT: Willy Woo (2017)
#'   \item Puell Multiple: David Puell (2019)
#' }
#'
#' @export
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
      realized_price = TTR::SMA(close, n = 200),
      mvrv = close / realized_price,
      issuance = sapply(timestamp, function(x) get_btc_issuance(x)),
      issuance_usd = issuance * close,
      puell_multiple = issuance_usd / zoo::rollmean(issuance_usd, k = 365, fill = NA, align = "right"),

      nvt_regime = dplyr::case_when(
        nvt > 70 ~ "Overvalued",
        nvt < 30 ~ "Undervalued",
        TRUE ~ "Neutral"
      ),
      mvrv_regime = dplyr::case_when(
        mvrv > 1.75 ~ "Extreme Top",
        mvrv < 0.7 ~ "Extreme Bottom",
        mvrv < 0.9 ~ "Bear Market",
        TRUE ~ "Bull Market"
      ),
      puell_signal = dplyr::case_when(
        puell_multiple > 2 ~ "Miner Selling Pressure",
        puell_multiple < 0.7 ~ "Miner Capitulation",
        TRUE ~ "Neutral"
      ),
      regime_combi = case_when(
        mvrv_regime == "Extreme Bottom" & puell_signal == "Miner Capitulation" ~ "Strong Buy",
        mvrv_regime == "Extreme Top" & puell_signal == "Miner Selling Pressure" ~ "Strong Sell",
        TRUE ~ "Neutral"
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}


#' Fetch and Analyze Cryptocurrency Derivatives Market Data
#'
#' @description
#' Retrieves and analyzes key derivatives market metrics from Binance, including
#' open interest, funding rates, and long/short ratios. These indicators provide
#' critical insights into market positioning, leverage dynamics, and potential
#' reversal points in the perpetual futures market.
#'
#' @param ticker Character string specifying the trading pair. Default "BTCUSDT".
#'   Format should follow Binance convention (e.g., "BTCUSDT", "ETHUSDT").
#'
#' @return A list object of class \code{derivatives_data} containing:
#'   \describe{
#'     \item{data}{Data frame with date, open interest, funding rates, and L/S ratio}
#'     \item{oi_percentile}{Percentile rank of current open interest (0-1)}
#'     \item{funding_percentile}{Percentile rank of current funding rate (0-1)}
#'     \item{signals}{List of derived trading signals including crowded trades and funding regime}
#'   }
#'
#' @details
#' The function analyzes three core derivatives metrics:
#' \itemize{
#'   \item \strong{Open Interest}: Total outstanding contracts, indicating money flow
#'   \item \strong{Funding Rates}: Periodic payments between longs and shorts, revealing sentiment
#'   \item \strong{Long/Short Ratio}: Relative positioning of market participants
#' }
#'
#' Crowded trade detection combines OI and funding percentiles:
#' \itemize{
#'   \item \strong{Crowded Long}: OI >90th %ile AND funding >90th %ile
#'   \item \strong{Crowded Short}: OI <10th %ile AND funding <10th %ile
#' }
#'
#' @note
#' \itemize{
#'   \item Percentiles calculated over available history for each metric
#'   \item No API key required for Binance public data
#'   \item Funding rates aggregated to daily averages from 8-hour snapshots
#' }
#'
#' @examples
#' \dontrun{
#' btc_derivatives <- get_derivatives("BTCUSDT")
#' }
#'
#' @seealso \code{\link{crypto_predictive_framework}} for integrated analysis
#'
#' @references Binance Exchange (2024). "Perpetual Futures Guide"
#'
#' @export
get_derivatives <- function(ticker = "BTCUSDT") {

  oi <- cryptoQuotes::get_openinterest(ticker = ticker, source = "binance",
                                       interval = "1d", from = Sys.Date() - 27)
  oi <- data.frame(date = as.Date(as.POSIXct(attributes(oi)$index)),
                   open_interest = oi$open_interest)

  funding <- cryptoQuotes::get_fundingrate(ticker = ticker, source = "binance",
                                           from = Sys.Date() - 27)
  funding <- data.frame(date = as.Date(as.POSIXct(attributes(funding)$index)),
                        funding_rate = funding$funding_rate)

  funding <- dplyr::group_by(funding, date) %>%
    dplyr::summarise(
      avg_funding_rate = mean(funding_rate, na.rm = TRUE),
      max_funding_rate = max(funding_rate, na.rm = TRUE),
      .groups = "drop"
    )

  ls_ratio <- cryptoQuotes::get_lsratio(ticker = ticker, source = "Binance",
                                        interval = "1d", from = Sys.Date() - 27)
  ls_ratio <- data.frame(date = as.Date(as.POSIXct(attributes(ls_ratio)$index)),
                         long = ls_ratio$long,
                         short = ls_ratio$short,
                         ls_ratio = ls_ratio$ls_ratio)

  derivatives_data <- dplyr::full_join(oi, funding, by = "date") %>%
    dplyr::full_join(ls_ratio, by = "date") %>%
    dplyr::arrange(date)

  if (nrow(derivatives_data) == 0) {
    warning("No derivatives data returned for ", ticker)
    return(list(data = derivatives_data,
                oi_percentile = numeric(0),
                funding_percentile = numeric(0),
                signals = list()))
  }

  oi_percentile <- rank(derivatives_data$open_interest, na.last = "keep") /
    sum(!is.na(derivatives_data$open_interest))
  funding_percentile <- rank(derivatives_data$avg_funding_rate, na.last = "keep") /
    sum(!is.na(derivatives_data$avg_funding_rate))

  signals <- list(
    crowded_long = !is.na(oi_percentile) & !is.na(funding_percentile) &
      oi_percentile > 0.9 & funding_percentile > 0.9,
    crowded_short = !is.na(oi_percentile) & !is.na(funding_percentile) &
      oi_percentile < 0.1 & funding_percentile < 0.1,
    oi_momentum = (derivatives_data$open_interest /
                     dplyr::lag(derivatives_data$open_interest, 7)) - 1,
    funding_regime = dplyr::case_when(
      is.na(derivatives_data$avg_funding_rate) ~ NA_character_,
      derivatives_data$avg_funding_rate * 100 > 0.05 ~ "Extreme Long",
      derivatives_data$avg_funding_rate * 100 > 0.01 ~ "Bullish",
      derivatives_data$avg_funding_rate * 100 >= 0.00 ~ "Neutral",
      derivatives_data$avg_funding_rate * 100 > -0.01 ~ "Bearish",
      derivatives_data$avg_funding_rate * 100 <= -0.01 ~ "Extreme Short",
      TRUE ~ "Unknown"
    )
  )

  return(list(
    data = derivatives_data,
    oi_percentile = oi_percentile,
    funding_percentile = funding_percentile,
    signals = signals
  ))
}


#' Fetch and Analyze Macroeconomic Indicators for Crypto Markets
#'
#' @description
#' Retrieves and analyzes key macroeconomic indicators that have demonstrated
#' significant correlation with cryptocurrency markets. This function aggregates
#' global liquidity conditions, interest rates, dollar strength, and inflation
#' expectations into a composite risk score for crypto asset allocation.
#'
#' @param fred_api_key Character string containing your FRED API key.
#'   Register for free at \url{https://fred.stlouisfed.org/docs/api/api_key.html}
#' @param start_date Start date for historical data in "YYYY-MM-DD" format.
#'   Default "2015-01-01" aligns with crypto market maturity.
#'
#' @return A list object of class \code{macro_data} containing:
#'   \describe{
#'     \item{liquidity}{M2 money supply data and growth rates}
#'     \item{rates}{Treasury yields and real rates}
#'     \item{dollar}{US Dollar Index data and regime classification}
#'     \item{composite_risk_score}{Weighted average macro score (-1 to +1)}
#'   }
#'
#' @details
#' The function captures three significant macro drivers:
#' \itemize{
#'   \item \strong{Global Liquidity (M2)}: Money supply growth (Expanding >5%, Contracting <0%)
#'   \item \strong{Interest Rates}: 10-year Treasury yield (Low <1%, High >3%)
#'   \item \strong{Dollar Strength}: DXY index via FRED's DTWEXBGS (Weak <110, Strong >120)
#' }
#'
#' The composite risk score weights each factor equally, ranging from -1 (maximum bearish)
#' to +1 (maximum bullish). Scores above 0.3 indicate bullish macro conditions, while
#' scores below -0.3 indicate bearish conditions.
#'
#' @note
#' \itemize{
#'   \item FRED API key required for M2, treasury yields, and inflation data
#'   \item DXY data sourced from FRED's Trade Weighted Dollar Index (DTWEXBGS)
#'   \item First observation for YoY calculations requires 12 months of data
#' }
#'
#' @examples
#' \dontrun{
#' macro <- get_macro(fred_api_key = "your_api_key")
#' }
#'
#' @seealso
#' \code{\link{get_onchain_indicators}} for fundamental crypto valuation
#' \code{\link{get_derivatives}} for market positioning signals
#'
#' @references Federal Reserve Economic Data (FRED): \url{https://fred.stlouisfed.org/}
#'
#' @export
get_macro <- function(fred_api_key = NULL, start_date = "2015-01-01"){

  fredr::fredr_set_key(fred_api_key)

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

  dxy <- tryCatch({
    suppressWarnings({
      fredr::fredr(
        series_id = "DTWEXBGS",
        observation_start = as.Date(start_date),
        observation_end = Sys.Date()
      )
    })
  }, error = function(e) {
    message("Note: DXY data unavailable from FRED")
    return(NULL)
  })

  if (!is.null(dxy) && nrow(dxy) > 0) {
    latest_dxy <- tail(dxy$value, 1)
    dollar_regime <- ifelse(
      latest_dxy > 120, "Strong Dollar (Bearish Crypto)",
      ifelse(latest_dxy < 110, "Weak Dollar (Bullish Crypto)", "Neutral")
    )
    dollar_score <- ifelse(latest_dxy > 120, -1,
                           ifelse(latest_dxy < 110, 1, 0))
  } else {
    dollar_regime <- "Unknown"
    dollar_score <- 0
    warning("Dollar index data unavailable")
  }

  treasury_10y <- fredr::fredr(
    series_id = "DGS10",
    observation_start = as.Date(start_date),
    observation_end = Sys.Date()
  )

  inflation_expect <- fredr::fredr(
    series_id = "T5YIE",
    observation_start = as.Date(start_date),
    observation_end = Sys.Date()
  )

  aggregated <- dplyr::full_join(treasury_10y, inflation_expect, by = "date")
  aggregated <- aggregated[, c(1, 3, 7)]
  colnames(aggregated) <- c("date", "DGS10", "T5YIE")
  aggregated$real_rates <- aggregated$DGS10 - aggregated$T5YIE

  rate_regime <- ifelse(
    tail(treasury_10y$value, 1) > 3, "High Rates (Risk-Off)",
    ifelse(tail(treasury_10y$value, 1) < 1, "Low Rates (Risk-On)", "Neutral")
  )

  m2_score <- ifelse(tail(m2_yoy, 1) > 0.05, 1,
                     ifelse(tail(m2_yoy, 1) < 0, -1, 0))
  rate_score <- ifelse(tail(treasury_10y$value, 1) > 3, -1,
                       ifelse(tail(treasury_10y$value, 1) < 1, 1, 0))

  composite_risk_score <- (m2_score + rate_score + dollar_score) / 3

  return(list(
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
      dollar_regime = dollar_regime,
      latest_value = if (!is.null(dxy)) tail(dxy$value, 1) else NA
    ),
    composite_risk_score = composite_risk_score
  ))
}


#' Calculate Cryptocurrency Volatility Metrics and Regimes
#'
#' @description
#' Computes realized volatility across multiple time horizons and identifies
#' volatility regimes to determine optimal trading strategies. This function
#' adapts trading approaches to market conditions, as different volatility
#' environments favor different strategies.
#'
#' @param data A data frame containing cryptocurrency price data, typically the
#'   output from \code{\link{get_onchain_indicators()}}. Must contain a column
#'   \code{log_returns} with logarithmic returns.
#' @param windows A list specifying the rolling windows for volatility calculation:
#'   \itemize{
#'     \item \code{short}: Window for short-term volatility (default: 7 days)
#'     \item \code{medium}: Window for medium-term volatility (default: 30 days)
#'     \item \code{long}: Window for long-term volatility (default: 90 days)
#'   }
#'
#' @return A list object of class \code{volatility_data} containing:
#'   \describe{
#'     \item{volatility}{List with short, medium, long-term volatility series and percentiles}
#'     \item{regime}{Categorical regime for each observation}
#'     \item{signals}{Trading signals including breakout/compression detection}
#'   }
#'
#' @details
#' Realized volatility is calculated as the standard deviation of log returns,
#' annualized by multiplying by the square root of 365 (crypto trades 24/7).
#'
#' Volatility regimes determine optimal strategies:
#' \itemize{
#'   \item \strong{Extreme Low}: Prepare for breakout, scale in gradually
#'   \item \strong{Low}: Trend following, full position sizes
#'   \item \strong{High}: Active trading, reduced position sizes
#'   \item \strong{Extreme High}: Capitulation - wait for stabilization
#' }
#'
#' @note
#' \itemize{
#'   \item First \code{max(windows)} observations will have NA values
#'   \item Annualization uses 365 days for crypto markets
#'   \item Percentile ranks calculated over entire dataset
#' }
#'
#' @examples
#' \dontrun{
#' market <- get_market_data("Bitcoin", "2018-01-01")
#' onchain <- get_onchain_indicators(market$data)
#' vol <- get_volatility(onchain)
#' }
#'
#' @seealso \code{\link{get_onchain_indicators}} for source log returns
#'
#' @references
#' \itemize{
#'   \item Andersen, T. G., & Bollerslev, T. (1998)
#'   \item CBOE (2023). VIX Index Rules of Construction
#' }
#'
#' @export
get_volatility <- function(data, windows = list(short = 7, medium = 30, long = 90)) {

  returns <- data$log_returns

  vol_short <- TTR::runSD(returns, n = windows$short) * sqrt(365)
  vol_medium <- TTR::runSD(returns, n = windows$medium) * sqrt(365)
  vol_long <- TTR::runSD(returns, n = windows$long) * sqrt(365)

  vol_percentile_short <- rank(vol_short, na.last = "keep") / sum(!is.na(vol_short))
  vol_percentile_medium <- rank(vol_medium, na.last = "keep") / sum(!is.na(vol_medium))

  vol_mean <- mean(vol_short, na.rm = TRUE)
  vol_sd <- sd(vol_short, na.rm = TRUE)

  vol_regime_short <- dplyr::case_when(
    is.na(vol_short) ~ NA_character_,
    vol_short > vol_mean + vol_sd ~ "Extreme High",
    vol_short < vol_mean - vol_sd ~ "Extreme Low",
    vol_short > vol_mean ~ "High",
    TRUE ~ "Low"
  )

  current_regime <- tail(vol_regime_short[!is.na(vol_regime_short)], 1)
  if (length(current_regime) == 0) current_regime <- "Unknown"

  current_regime_desc <- switch(current_regime,
                                "Extreme High" = "Risk-Off (Capitulation)",
                                "High" = "Active Trading",
                                "Low" = "Trend Following",
                                "Extreme Low" = "Complacency (Watch for Breakout)",
                                "Unknown")

  signals <- list(
    vol_breakout = !is.na(vol_short) & !is.na(vol_medium) & vol_short > vol_medium * 1.5,
    vol_compression = !is.na(vol_short) & !is.na(vol_medium) & vol_short < vol_medium * 0.5,
    current_regime = current_regime_desc
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


#' Fetch Positioning Data from Morpho Vaults
#'
#' @description
#' Retrieves and analyzes positioning data from Morpho vaults, specifically tracking
#' Bitcoin exposure through DeFi lending markets. This provides insights into
#' "smart money" positioning, whale concentration, and capital flows.
#'
#' @param include_depositors Logical. Whether to fetch top depositor data (slower). Default TRUE.
#' @param include_flows Logical. Whether to fetch transaction flow data (slower). Default TRUE.
#' @param top_n_vaults Integer. Number of top vaults to analyze for depositors/flows. Default 5.
#' @param max_depositors_per_vault Integer. Maximum depositors to fetch per vault. Default 10.
#'
#' @return A list object of class \code{positioning_data} containing:
#'   \describe{
#'     \item{exchange_positioning}{Flow summary and regime classification}
#'     \item{whale_activity}{Whale wallet counts and activity signals}
#'     \item{raw_positioning_data}{Complete Morpho data from \code{\link{get_morpho_positioning}}}
#'     \item{data_source}{Always "Morpho Vaults"}
#'   }
#'
#' @details
#' Morpho vaults reveal actual capital deployment in DeFi lending markets:
#' \itemize{
#'   \item \strong{Vault Inflows}: BTC entering productive use (bullish signal)
#'   \item \strong{Vault Outflows}: BTC returning to cold storage (bearish signal)
#'   \item \strong{Whale Concentration}: Identifies systemic risk through HHI
#'   \item \strong{Vault Composition}: Direct deposits vs collateralized lending
#' }
#'
#' @note
#' \itemize{
#'   \item No API key required - data from public Morpho endpoints
#'   \item Rate limits may apply for intensive queries
#' }
#'
#' @examples
#' \dontrun{
#' positioning <- get_positioning()
#' }
#'
#' @seealso
#' \code{\link{get_morpho_positioning}} for detailed Morpho data
#' \code{\link{get_derivatives}} for derivatives market positioning
#'
#' @export
get_positioning <- function(include_depositors = TRUE,
                            include_flows = TRUE,
                            top_n_vaults = 5,
                            max_depositors_per_vault = 10) {

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

  morpho_data <- tryCatch({
    get_morpho_positioning(
      include_depositors = include_depositors,
      include_flows = include_flows,
      top_n_vaults = top_n_vaults,
      max_depositors_per_vault = max_depositors_per_vault
    )
  }, error = function(e) {
    warning("Failed to fetch Morpho data: ", e$message)
    return(create_empty_positioning(source = "Morpho Error"))
  })

  if (is.null(morpho_data) || morpho_data$status != "success") {
    warning("Morpho data fetch was not successful")
    return(create_empty_positioning(source = "Morpho Failed"))
  }

  flow_regime_from_whale <- dplyr::case_when(
    grepl("High Whale Intensity", morpho_data$whale_activity$whale_signal) ~
      "High Whale Activity (Volatility Expected)",
    grepl("Moderate", morpho_data$whale_activity$whale_signal) ~
      "Moderate Whale Activity",
    grepl("Normal", morpho_data$whale_activity$whale_signal) ~
      "Normal Whale Activity",
    TRUE ~ morpho_data$whale_activity$whale_signal %||% "Unknown"
  )

  result <- list(
    exchange_positioning = list(
      netflow_summary = data.frame(
        inflow_usd = morpho_data$flow_summary$total_inflow_usd %||% 0,
        outflow_usd = morpho_data$flow_summary$total_outflow_usd %||% 0,
        tx_count = morpho_data$flow_summary$total_transactions %||%
          morpho_data$whale_concentration$total_whale_wallets %||% 0,
        pct = morpho_data$whale_concentration$top_5_share %||% 0,
        netflow_usd = morpho_data$flow_summary$net_flow_usd %||% 0
      ),
      flow_regime = morpho_data$flow_summary$flow_regime %||%
        flow_regime_from_whale %||% "Unknown",
      top_exchange_target = if (!is.null(morpho_data$top_vaults) && nrow(morpho_data$top_vaults) > 0)
        morpho_data$top_vaults$vault_display[1]
      else NA_character_
    ),
    whale_activity = list(
      total_tx = morpho_data$whale_concentration$total_whale_wallets %||%
        nrow(morpho_data$vault_summary) %||% 0,
      whale_signal = morpho_data$whale_activity$whale_signal %||% "Unknown",
      largest_move_usd = if (!is.null(morpho_data$vault_summary) && nrow(morpho_data$vault_summary) > 0)
        morpho_data$vault_summary$btc_exposure_usd[1]
      else 0
    ),
    raw_positioning_data = morpho_data,
    data_source = "Morpho Vaults"
  )

  result$interpretation <- morpho_data$interpretation %||%
    "Morpho positioning data collected successfully."

  class(result) <- "positioning_data"
  return(result)
}


#' Create empty positioning data structure
#'
#' @description
#' Internal function to create a standardized empty positioning structure
#' when data fetching fails. Ensures downstream functions receive a valid
#' object even when no data is available.
#'
#' @param source Character string indicating the source that failed
#'
#' @return A list with the same structure as \code{\link{get_positioning}} but with empty data
#'
#' @keywords internal
create_empty_positioning <- function(source = "Unknown") {

  result <- list(
    exchange_positioning = list(
      netflow_summary = data.frame(
        inflow_usd = 0,
        outflow_usd = 0,
        tx_count = 0,
        pct = 0,
        netflow_usd = 0
      ),
      flow_regime = paste("No Data -", source),
      top_exchange_target = NA_character_
    ),
    whale_activity = list(
      total_tx = 0,
      whale_signal = paste("No Data -", source),
      largest_move_usd = 0
    ),
    raw_positioning_data = list(
      status = source,
      timestamp = Sys.time()
    ),
    data_source = source
  )

  class(result) <- "positioning_data"
  return(result)
}


#' Print method for positioning_data
#'
#' @description
#' Custom print method for positioning_data objects, providing a formatted
#' summary of positioning data including flow regime, net flows, and whale activity.
#'
#' @param x An object of class \code{positioning_data}
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.positioning_data <- function(x, ...) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("🐋 POSITIONING DATA -", x$data_source, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  cat("\n📊 FLOW REGIME:", x$exchange_positioning$flow_regime, "\n")

  netflow <- x$exchange_positioning$netflow_summary$netflow_usd
  cat("   Net Flow: $", format(netflow, big.mark = ",", scientific = FALSE), "\n")
  cat("   Transactions Analyzed:", x$exchange_positioning$netflow_summary$tx_count, "\n")

  if (x$exchange_positioning$netflow_summary$pct > 0) {
    cat("   Top 5 Depositors Share:", round(x$exchange_positioning$netflow_summary$pct, 1), "%\n")
  }

  cat("\n🐋 WHALE ACTIVITY:", x$whale_activity$whale_signal, "\n")
  cat("   Unique Whale Wallets:", x$whale_activity$total_tx, "\n")
  cat("   Largest Vault: $", format(x$whale_activity$largest_move_usd,
                                    big.mark = ",", scientific = FALSE), "\n")

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  invisible(x)
}


#' Fetch Google Trends Data
#'
#' @description
#' Fetches Google Trends data for specified keywords to gauge retail sentiment
#' and public interest in cryptocurrency topics. Returns sentiment score and
#' signal based on 7-day average search interest.
#'
#' @param keywords Character vector of keywords to search for. Default "bitcoin".
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{current_sentiment}: Average search interest over last 7 days (0-100)
#'     \item \code{sentiment_signal}: Categorical interpretation of sentiment
#'     \item \code{interest_over_time}: Full historical data
#'     \item \code{keywords}: Keywords used
#'   }
#'
#' @details
#' Search interest is normalized to a 0-100 scale, where 100 represents peak
#' popularity for the selected time range. The function calculates the 7-day
#' average and classifies sentiment:
#' \itemize{
#'   \item \strong{>80}: Extreme Interest (Potential Top)
#'   \item \strong{60-80}: High Interest
#'   \item \strong{20-60}: Normal Interest
#'   \item \strong{<20}: Low Interest (Potential Bottom)
#' }
#'
#' @note
#' \itemize{
#'   \item Google Trends data may be subject to sampling and regional variations
#'   \item API rate limits apply; use responsibly
#' }
#'
#' @examples
#' \dontrun{
#' trends <- get_google_trends("bitcoin")
#' }
#'
#' @seealso \code{\link{get_fear_greed}} for alternative sentiment indicator
#'
#' @export
get_google_trends <- function(keywords = "bitcoin") {

  result <- tryCatch({
    trends <- gtrendsR::gtrends(
      keyword = keywords,
      time = "today 3-m",
      gprop = "web"
    )

    if (is.null(trends$interest_over_time)) {
      warning("No interest_over_time data returned from Google Trends")
      return(NULL)
    }

    interest_data <- trends$interest_over_time
    interest_data$date <- as.Date(interest_data$date)

    recent_interest <- interest_data[interest_data$date > Sys.Date() - 7, ]
    sentiment_score <- mean(recent_interest$hits, na.rm = TRUE)

    sentiment_signal <- dplyr::case_when(
      sentiment_score > 80 ~ "Extreme Interest (Potential Top)",
      sentiment_score > 60 ~ "High Interest",
      sentiment_score > 20 ~ "Normal Interest",
      TRUE ~ "Low Interest (Potential Bottom)"
    )

    return(list(
      interest_over_time = interest_data,
      related_queries = trends$related_queries,
      interest_by_region = trends$interest_by_region,
      current_sentiment = sentiment_score,
      sentiment_signal = sentiment_signal,
      keywords = keywords
    ))

  }, error = function(e) {
    warning("Google Trends error: ", e$message)
    return(NULL)
  })

  return(result)
}


#' Fetch Fear & Greed Index
#'
#' @description
#' Retrieves the Crypto Fear & Greed Index from alternative.me via the cryptoQuotes package.
#' The index ranges from 0 (Extreme Fear) to 100 (Extreme Greed), providing a contrarian
#' sentiment indicator for cryptocurrency markets.
#'
#' @param days Integer. Number of days of historical data to fetch. Default 30.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{current_value}: Current Fear & Greed Index value (0-100)
#'     \item \code{current_classification}: Text classification
#'     \item \code{timestamp}: When the data was fetched
#'     \item \code{history}: xts object with historical values
#'     \item \code{source}: Data source attribution
#'   }
#'
#' @details
#' Classification thresholds:
#' \itemize{
#'   \item \strong{0-25}: Extreme Fear (potential buying opportunity)
#'   \item \strong{25-45}: Fear
#'   \item \strong{45-55}: Neutral
#'   \item \strong{55-75}: Greed
#'   \item \strong{75-100}: Extreme Greed (potential selling opportunity)
#' }
#'
#' The index is a contrarian indicator - extreme fear often precedes market bottoms,
#' while extreme greed often precedes market tops.
#'
#' @examples
#' \dontrun{
#' fgi <- get_fear_greed()
#' }
#'
#' @seealso \code{\link{get_google_trends}} for retail search sentiment
#'
#' @export
get_fear_greed <- function(days = 30) {

  tryCatch({
    fgi <- cryptoQuotes::get_fgindex(from = Sys.Date() - days)

    if (is.null(fgi) || nrow(fgi) == 0) {
      warning("No Fear & Greed data returned")
      return(NULL)
    }

    current_value <- as.numeric(tail(fgi$fgi, 1))

    classification <- dplyr::case_when(
      current_value >= 75 ~ "Extreme Greed",
      current_value >= 55 ~ "Greed",
      current_value >= 45 ~ "Neutral",
      current_value >= 25 ~ "Fear",
      TRUE ~ "Extreme Fear"
    )

    return(list(
      current_value = current_value,
      current_classification = classification,
      timestamp = Sys.time(),
      history = fgi,
      source = "alternative.me via cryptoQuotes"
    ))

  }, error = function(e) {
    warning("Failed to fetch Fear & Greed Index: ", e$message)
    return(NULL)
  })
}
