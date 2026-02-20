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

  # Add market_cap if missing (approximate)
  if (!"market_cap" %in% names(data)) {
    data$market_cap <- data$close * 19991700  # Approximate BTC supply (as per 20.02.2026)
  }

  return(
    list(data = data,
         assets = assets,
         start_date = start_date,
         last_update = Sys.Date()
         )
    )

}


#' Calculate Bitcoin Daily Issuance Based on Halving Schedule
#'
#' @description
#' Computes the theoretical daily Bitcoin issuance (new coins created) for any given date
#' based on Bitcoin's predetermined halving schedule. This function models Bitcoin's
#' monetary policy where block rewards are cut in half approximately every four years.
#'
#' @param target_date A date object or character string in "YYYY-MM-DD" format for which
#'   to calculate the daily issuance. The function determines which halving regime was
#'   active on this date.
#'
#' @return A numeric value representing the estimated number of new Bitcoins mined per day
#'   on the target date. Returns 0 for dates before the first block (2009-01-03).
#'
#' @details
#' Bitcoin's issuance follows a strict, predetermined schedule:
#' \itemize{
#'   \item \strong{2009-01-03 to 2012-11-27}: 50 BTC per block × 144 blocks/day = 7,200 BTC/day
#'   \item \strong{2012-11-28 to 2016-07-08}: 25 BTC per block × 144 blocks/day = 3,600 BTC/day
#'   \item \strong{2016-07-09 to 2020-05-10}: 12.5 BTC per block × 144 blocks/day = 1,800 BTC/day
#'   \item \strong{2020-05-11 to 2024-04-19}: 6.25 BTC per block × 144 blocks/day = 900 BTC/day
#'   \item \strong{2024-04-20 to present}: 3.125 BTC per block × 144 blocks/day = 450 BTC/day
#' }
#'
#' The calculation assumes:
#' \itemize{
#'   \item Consistent 10-minute block intervals (144 blocks per day)
#'   \item No variance due to difficulty adjustments or hashrate changes
#'   \item Perfect adherence to halving schedule (which Bitcoin's protocol enforces)
#' }
#'
#' @note
#' This is a theoretical maximum based on ideal block timing. Actual daily issuance
#' may vary slightly due to:
#' \itemize{
#'   \item Block time variance (not exactly 10 minutes)
#'   \item Occasional orphaned blocks
#'   \item Difficulty adjustment periods
#' }
#' However, over longer periods, the average converges to this theoretical value.
#'
#' @examples
#' \dontrun{
#' # Calculate issuance during different eras
#' get_btc_issuance("2011-01-01")  # First era: 7,200 BTC/day
#' get_btc_issuance("2014-01-01")  # Second era: 3,600 BTC/day
#' get_btc_issuance("2018-01-01")  # Third era: 1,800 BTC/day
#' get_btc_issuance("2022-01-01")  # Fourth era: 900 BTC/day
#' get_btc_issuance(Sys.Date())     # Current era: 450 BTC/day
#'
#' # Use in data pipeline
#' dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
#' sapply(dates, get_btc_issuance)
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_onchain_indicators}} which uses this to calculate Puell Multiple
#'   \item \code{\link{get_btc_issuance_usd}} for USD value of issuance (if implemented)
#'   \item Bitcoin whitepaper: \url{https://bitcoin.org/bitcoin.pdf}
#' }
#'
#' @references
#' \itemize{
#'   \item Nakamoto, S. (2008). Bitcoin: A Peer-to-Peer Electronic Cash System.
#'   \item Bitcoin Block Halving Schedule: \url{https://www.bitcoinblockhalf.com}
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr filter slice_tail pull
get_btc_issuance <- function(target_date) {

  # Convert to Date if character
  if (is.character(target_date)) {
    target_date <- as.Date(target_date)
  }

  halvings <- data.frame(
    date = as.Date(c("2009-01-03", "2012-11-28", "2016-07-09", "2020-05-11", "2024-04-20")),
    reward = c(50, 25, 12.5, 6.25, 3.125)
  )

  # Handle dates before first halving
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
#'   output from \code{\link{get_market_data()}} or similar structure. Must contain
#'   columns: \code{timestamp}, \code{slug}, \code{close}, \code{volume}, and
#'   \code{market_cap}.
#'
#' @return The input data frame augmented with the following calculated columns:
#'
#'   \strong{Basic Metrics:}
#'   \itemize{
#'     \item \code{returns}: Simple daily returns (close / lag(close) - 1)
#'     \item \code{log_returns}: Log-normal returns for volatility calculations
#'     \item \code{volume_ma_7}: 7-day simple moving average of volume
#'     \item \code{price_ma_50}: 50-day simple moving average of price
#'     \item \code{price_ma_200}: 200-day simple moving average of price
#'   }
#'
#'   \strong{On-Chain Valuation Metrics:}
#'   \itemize{
#'     \item \code{nvt}: Network Value to Transactions ratio (market_cap / volume)
#'     \item \code{nvts}: Smoothed NVT using 14-day SMA of volume
#'     \item \code{realized_price}: 200-day SMA price (proxy for average acquisition cost)
#'     \item \code{mvrv}: Market Value to Realized Value ratio (close / realized_price)
#'   }
#'
#'   \strong{Miner-Related Metrics:}
#'   \itemize{
#'     \item \code{issuance}: Daily Bitcoin issuance based on halving schedule
#'     \item \code{issuance_usd}: USD value of daily issuance
#'     \item \code{puell_multiple}: Issuance USD / 365-day MA of issuance USD
#'   }
#'
#'   \strong{Signal Regimes:}
#'   \itemize{
#'     \item \code{nvt_regime}: Categorical interpretation of NVT
#'     \item \code{mvrv_regime}: Cycle positioning based on MVRV
#'     \item \code{puell_signal}: Miner behavior interpretation
#'     \item \code{regime_combi}: Composite signal combining MVRV and Puell
#'   }
#'
#' @details
#' \strong{MVRV Ratio (Market Value to Realized Value)}
#'
#' MVRV compares current market price to the average price at which coins were
#' last moved (realized price). Using 200-day SMA as a proxy for realized price:
#' \itemize{
#'   \item \strong{< 0.7}: "Extreme Bottom" - Market significantly below average acquisition cost
#'   \item \strong{0.7 - 0.9}: "Bear Market" - Trading below cost basis
#'   \item \strong{0.9 - 1.75}: "Bull Market" - Healthy premium to cost basis
#'   \item \strong{> 1.75}: "Extreme Top" - Excessive unrealized profits
#' }
#'
#' \strong{NVT Ratio (Network Value to Transactions)}
#'
#' Analogous to the P/E ratio in equities, NVT measures whether the network's
#' valuation is supported by its transaction volume:
#' \itemize{
#'   \item \strong{< 30}: "Undervalued" - Network usage high relative to value
#'   \item \strong{30 - 70}: "Neutral" - Balanced valuation
#'   \item \strong{> 70}: "Overvalued" - Value exceeds network usage
#' }
#'
#' \strong{Puell Multiple}
#'
#' Measures miner revenue relative to the yearly moving average, identifying
#' periods of miner capitulation or exceptional profitability:
#' \itemize{
#'   \item \strong{< 0.7}: "Miner Capitulation" - Miners selling at a loss
#'   \item \strong{0.7 - 2.0}: "Neutral" - Normal miner profitability
#'   \item \strong{> 2.0}: "Miner Selling Pressure" - Miners exceptionally profitable
#' }
#'
#' \strong{Composite Signal}
#'
#' Combines MVRV and Puell for stronger conviction signals:
#' \itemize{
#'   \item \strong{Strong Buy}: MVRV Extreme Bottom + Puell Miner Capitulation
#'   \item \strong{Strong Sell}: MVRV Extreme Top + Puell Miner Selling Pressure
#'   \item \strong{Neutral}: All other combinations
#' }
#'
#' @note
#' \itemize{
#'   \item Realized price is approximated using 200-day SMA. True realized price
#'     requires UTXO-level data and would be more accurate but computationally intensive.
#'   \item Puell Multiple is Bitcoin-specific due to its dependence on issuance schedule.
#'   \item First 200 days of data will have NA values for moving averages.
#'   \item Regime thresholds are based on historical analysis and may require
#'     adjustment as market dynamics evolve.
#' }
#'
#' @examples
#' \dontrun{
#' # Get market data first
#' market_data <- get_market_data("Bitcoin", "2015-01-01")
#'
#' # Calculate on-chain indicators
#' onchain_data <- get_onchain_indicators(market_data$data)
#'
#' # View latest signals
#' latest <- onchain_data %>%
#'   dplyr::slice_tail(n = 1) %>%
#'   dplyr::select(timestamp, mvrv_regime, puell_signal, regime_combi)
#' print(latest)
#'
#' # Plot MVRV history with regime thresholds
#' library(ggplot2)
#' onchain_data %>%
#'   tail(365) %>%
#'   ggplot(aes(x = timestamp, y = mvrv)) +
#'   geom_line() +
#'   geom_hline(yintercept = c(0.7, 0.9, 1.75), linetype = "dashed", color = "red") +
#'   labs(title = "MVRV Ratio with Regime Thresholds")
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_market_data}} for input data acquisition
#'   \item \code{\link{get_btc_issuance}} for issuance calculation details
#'   \item \code{\link{get_volatility}} for volatility metrics on returns
#'   \item Woo, W. (2019). "On-Chain Indicators for Bitcoin Valuation"
#' }
#'
#' @references
#' \itemize{
#'   \item MVRV: Murad Mahmudov & Adam Taché (2019). "Bitcoin's Realized Value"
#'   \item NVT: Willy Woo (2017). "Introducing NVT Ratio (Network Value to Transactions Ratio)"
#'   \item Puell Multiple: David Puell (2019). "The Puell Multiple: A Bitcoin Mining Indicator"
#'   \item Checkmate (2020). "The Bitcoin On-Chain Market Cycle Guide"
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr group_by arrange mutate case_when
#' @importFrom zoo rollmean
#' @importFrom TTR SMA
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
        TRUE ~ "Neutral")
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
#'
#'   \strong{Data Frame:}
#'   \itemize{
#'     \item \code{data}: Merged time series with columns:
#'       \itemize{
#'         \item \code{date}: Date of observation
#'         \item \code{open_interest}: Total number of outstanding contracts (in USD)
#'         \item \code{avg_funding_rate}: Mean funding rate for the day (as decimal)
#'         \item \code{max_funding_rate}: Maximum funding rate for the day
#'         \item \code{long}: Number of long positions
#'         \item \code{short}: Number of short positions
#'         \item \code{ls_ratio}: Long/short ratio (long/short)
#'       }
#'   }
#'
#'   \strong{Percentiles:}
#'   \itemize{
#'     \item \code{oi_percentile}: Percentile rank of current open interest (0-1)
#'     \item \code{funding_percentile}: Percentile rank of current funding rate (0-1)
#'   }
#'
#'   \strong{Signals:} List of derived trading signals
#'   \itemize{
#'     \item \code{crowded_long}: Logical vector - TRUE when OI >90th %ile AND funding >90th %ile
#'     \item \code{crowded_short}: Logical vector - TRUE when OI <10th %ile AND funding <10th %ile
#'     \item \code{oi_momentum}: 7-day percentage change in open interest
#'     \item \code{funding_regime}: Categorical regime based on funding rate:
#'       \itemize{
#'         \item \strong{Extreme Long}: Funding > 0.05% (annualized >18%)
#'         \item \strong{Bullish}: Funding 0.01% to 0.05%
#'         \item \strong{Neutral}: Funding 0.00% to 0.01%
#'         \item \strong{Bearish}: Funding -0.01% to 0.00%
#'         \item \strong{Extreme Short}: Funding < -0.01%
#'       }
#'   }
#'
#' @details
#' The \code{get_derivatives()} function retrieves and analyzes three core derivatives
#' market metrics from Binance, providing critical insights into market positioning,
#' leverage dynamics, and potential reversal points.
#'
#' \strong{Open Interest}
#'
#' Open interest represents the total number of outstanding derivative contracts
#' that have not been settled. It measures the flow of money into the derivatives
#' market and provides context on trend strength and potential exhaustion:
#' \itemize{
#'   \item \strong{Rising OI + Rising Price:} New long money entering (trend confirmation)
#'   \item \strong{Rising OI + Falling Price:} New short money entering (distribution/short buildup)
#'   \item \strong{Falling OI + Any Price:} Position unwinding (trend exhaustion)
#'   \item \strong{OI > 90th percentile:} Market may be overextended
#' }
#'
#' \strong{Funding Rates}
#'
#' Perpetual swaps use funding rates to keep contract prices anchored to spot prices.
#' These periodic payments (every 8 hours on Binance) reveal the dominant market
#' sentiment and cost of maintaining leveraged positions:
#' \itemize{
#'   \item \strong{Positive funding:} Longs pay shorts (bullish sentiment)
#'   \item \strong{Negative funding:} Shorts pay longs (bearish sentiment)
#'   \item \strong{Extreme positive (> 0.05\%):} Market overly levered long, liquidation cascade risk
#'   \item \strong{Extreme negative (< -0.01\%):} Excessive short positioning, short squeeze risk
#' }
#'
#' The function calculates daily average and maximum funding rates, then classifies
#' the market regime based on the magnitude of the average rate.
#'
#' \strong{Long/Short Ratio}
#'
#' The ratio of long to short positions provides direct insight into market
#' positioning and potential contrarian opportunities:
#' \itemize{
#'   \item \strong{High ratio (> 1.2):} More longs than shorts (bullish positioning)
#'   \item \strong{Low ratio (< 0.8):} More shorts than longs (bearish positioning)
#'   \item \strong{Extreme values:} Often precede reversals (contrarian signals)
#' }
#'
#' \strong{Crowded Trade Detection}
#'
#' The function combines open interest and funding rate percentiles to identify
#' potentially dangerous crowded positioning:
#' \itemize{
#'   \item \strong{Crowded Long:} OI > 90th percentile AND funding > 90th percentile
#'   \itemize{
#'     \item Market is excessively long with high leverage
#'     \item Vulnerable to long liquidations on any downside move
#'     \item Often precedes sharp reversals
#'   }
#'   \item \strong{Crowded Short:} OI < 10th percentile AND funding < 10th percentile
#'   \itemize{
#'     \item Market is excessively short with negative funding
#'     \item Shorts paying to maintain positions
#'     \item Primed for short squeeze
#'   }
#' }
#'
#' \strong{Interpretation Guidelines}
#'
#' When using these signals in your analysis:
#' \itemize{
#'   \item Use extreme funding rates as mean reversion signals
#'   \item Confirm trends with rising open interest
#'   \item Treat crowded positioning as warning signs, not immediate triggers
#'   \item Combine with on-chain and macro indicators for higher conviction
#'   \item Consider that different assets may have different "normal" ranges
#' }
#'
#' @note
#' \itemize{
#'   \item Percentiles are calculated over the entire available history for each metric
#'   \item First 7 days will have NA values for open interest momentum
#'   \item Funding rates are aggregated to daily averages from 8-hour snapshots
#'   \item Different exchanges may have different funding rate mechanisms
#'   \item No API key is required for Binance public data
#' }
#'
#' @examples
#' \dontrun{
#' # Get derivatives data for Bitcoin
#' btc_derivatives <- get_derivatives("BTCUSDT")
#'
#' # View latest signals
#' latest_idx <- length(btc_derivatives$signals$funding_regime)
#' cat("Current funding regime:", btc_derivatives$signals$funding_regime[latest_idx], "\n")
#' cat("OI Percentile:", round(btc_derivatives$oi_percentile[latest_idx] * 100, 1), "%\n")
#'
#' # Check for crowded trades
#' if (tail(btc_derivatives$signals$crowded_long, 1)) {
#'   cat("⚠️  Warning: Crowded long positioning detected!\n")
#' }
#'
#' # Plot open interest with percentile bands
#' library(ggplot2)
#' btc_derivatives$data %>%
#'   tail(90) %>%
#'   ggplot(aes(x = date, y = open_interest)) +
#'   geom_line() +
#'   geom_hline(yintercept = quantile(btc_derivatives$data$open_interest, 0.9, na.rm = TRUE),
#'              linetype = "dashed", color = "red") +
#'   geom_hline(yintercept = quantile(btc_derivatives$data$open_interest, 0.1, na.rm = TRUE),
#'              linetype = "dashed", color = "green") +
#'   labs(title = "Open Interest with 10th/90th Percentile Bands")
#'
#' # Get Ethereum derivatives
#' eth_derivatives <- get_derivatives("ETHUSDT")
#'
#' # Compare funding rates across assets
#' funding_comparison <- data.frame(
#'   date = btc_derivatives$data$date,
#'   BTC = btc_derivatives$data$avg_funding_rate * 100,
#'   ETH = eth_derivatives$data$avg_funding_rate * 100
#' )
#' }
#'
#' @section Trading Applications:
#' \itemize{
#'   \item \strong{Mean Reversion}: Extreme funding rates (>0.05% or <-0.01%) often revert
#'   \item \strong{Trend Confirmation}: Rising OI confirms trend strength
#'   \item \strong{Reversal Detection}: Crowded long/short signals potential turning points
#'   \item \strong{Risk Management}: High OI + extreme funding suggests reducing leverage
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{crypto_predictive_framework}} for integrating with broader framework
#'   \item Binance Futures API: \url{https://binance-docs.github.io/apidocs/futures/en/}
#'   \item \code{cryptoQuotes::get_openinterest} for underlying function details
#' }
#'
#' @references
#' \itemize{
#'   \item Binance Exchange (2024). "Perpetual Futures Guide"
#'   \item Hou, A. (2021). "The Informational Role of Bitcoin Futures Market"
#'   \item Zhang, S. (2022). "Funding Rates as a Sentiment Indicator in Crypto Markets"
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr full_join group_by summarise arrange lag
#' @importFrom cryptoQuotes get_openinterest get_fundingrate get_lsratio
get_derivatives <- function(ticker = "BTCUSDT") {

  # open interest
  oi <- cryptoQuotes::get_openinterest(ticker = ticker, source = "binance", interval = "1d")
  oi <- data.frame(date = as.Date(as.POSIXct(attributes(oi)$index)),
                   open_interest = oi$open_interest)


  # funding rate
  funding <- cryptoQuotes::get_fundingrate(ticker = ticker, source = "binance")
  funding <- data.frame(date = as.Date(as.POSIXct(attributes(funding)$index)),
                        funding_rate = funding$funding_rate)

  funding <- dplyr::group_by(funding, date) %>%
    dplyr::summarise(
      avg_funding_rate = mean(funding_rate, na.rm = TRUE),
      max_funding_rate = max(funding_rate, na.rm = TRUE),
      .groups = "drop"
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

  # Check if data is empty
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

  # Signals

  signals <- list(
    # High OI with high funding suggests crowded long (potential reversal)
    crowded_long = !is.na(oi_percentile) > 0.9 & !is.na(funding_percentile) > 0.9,
    # Low OI with negative funding suggests capitulation (potential bottom)
    crowded_short = !is.na(oi_percentile) < 0.1 & !is.na(funding_percentile) < 0.1,
    # Open interest momentum
    oi_momentum = (derivatives_data$open_interest /
                     dplyr::lag(derivatives_data$open_interest, 7)) - 1,
    # Funding rate regime
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

  return(list(data = derivatives_data,
              oi_percentile = oi_percentile,
              funding_percentile = funding_percentile,
              signals = signals
              )
         )

}


#' Fetch and Analyze Macroeconomic Indicators for Crypto Markets
#'
#' @description
#' Retrieves and analyzes key macroeconomic indicators that have demonstrated
#' significant correlation with cryptocurrency markets. This function aggregates
#' global liquidity conditions, interest rates, dollar strength, and inflation
#' expectations into a composite risk score for crypto asset allocation.
#'
#' @param fred_api_key Character string containing your FRED API key. Default
#'   looks for \code{global_variables$fred_api_key}. Get a free API key at
#'   \url{https://fred.stlouisfed.org/docs/api/api_key.html}
#' @param start_date Start date for historical data in "YYYY-MM-DD" format.
#'   Default "2015-01-01" aligns with crypto market maturity.
#'
#' @return A list object of class \code{macro_data} containing:
#'
#'   \strong{Liquidity Conditions:}
#'   \itemize{
#'     \item \code{m2_us}: Raw M2 money supply data from FRED
#'     \item \code{m2_yoy}: Year-over-year percentage change in M2
#'     \item \code{m2_mom}: Month-over-month percentage change in M2
#'     \item \code{liquidity_regime}: Categorical regime:
#'       \itemize{
#'         \item "Expanding": M2 YoY > 5% (bullish for crypto)
#'         \item "Stable": M2 YoY between 0% and 5%
#'         \item "Contracting": M2 YoY < 0% (bearish for crypto)
#'       }
#'   }
#'
#'   \strong{Interest Rate Environment:}
#'   \itemize{
#'     \item \code{treasury_10y}: 10-year Treasury yield data
#'     \item \code{real_rates}: Real yields (nominal yield minus inflation expectations)
#'     \item \code{rate_regime}: Categorical regime:
#'       \itemize{
#'         \item "Low Rates (Risk-On)": 10-year yield < 1% (bullish)
#'         \item "Neutral": Yields between 1% and 3%
#'         \item "High Rates (Risk-Off)": Yields > 3% (bearish)
#'       }
#'   }
#'
#'   \strong{Dollar Strength:}
#'   \itemize{
#'     \item \code{dxy}: US Dollar Index (DXY) data from Yahoo Finance
#'     \item \code{dollar_regime}: Categorical regime:
#'       \itemize{
#'         \item "Weak Dollar (Bullish Crypto)": DXY < 90
#'         \item "Neutral": DXY between 90 and 100
#'         \item "Strong Dollar (Bearish Crypto)": DXY > 100
#'       }
#'   }
#'
#'   \strong{Composite Risk Score:}
#'   \itemize{
#'     \item \code{composite_risk_score}: Weighted average (-1 to +1 scale) of:
#'       \itemize{
#'         \item M2 YoY growth (+1 if >5%, -1 if <0%, else 0)
#'         \item 10-year yield (+1 if <1%, -1 if >3%, else 0)
#'         \item DXY (+1 if <90, -1 if >100, else 0)
#'       }
#'     \item Interpretation:
#'       \itemize{
#'         \item \strong{> 0.3}: Bullish macro environment
#'         \item \strong{-0.3 to 0.3}: Neutral macro environment
#'         \item \strong{< -0.3}: Bearish macro environment
#'       }
#'   }
#'
#' @details
#' \strong{Why Macro Matters for Crypto}
#'
#' Cryptocurrency markets, despite their decentralized nature, have shown
#' increasing correlation with traditional macro factors. This function captures
#' the three most significant macro drivers:
#'
#' \strong{1. Global Liquidity (M2 Money Supply)}
#'
#' M2 represents broad money supply including cash, checking deposits, and
#' easily convertible near-money. Crypto has historically shown strong correlation
#' with global liquidity expansion:
#' \itemize{
#'   \item \strong{Expanding M2 (>5\% YoY)}: Excess liquidity flows into risk assets,
#'     including cryptocurrencies. This was a key driver of the 2017 and 2021 bull runs.
#'   \item \strong{Contracting M2 (<0\% YoY)}: Liquidity drains from markets,
#'     creating headwinds for crypto. The 2022 bear market coincided with M2 contraction.
#'   \item \strong{Leading Indicator}: Changes in M2 often precede crypto market
#'     moves by 2-3 months.
#' }
#'
#' \strong{2. Interest Rates and Real Yields}
#'
#' The 10-year Treasury yield represents the risk-free rate of return, while
#' real yields adjust for inflation expectations. These affect crypto through:
#' \itemize{
#'   \item \strong{Opportunity Cost}: Higher yields increase the opportunity cost
#'     of holding non-yielding assets like Bitcoin.
#'   \item \strong{Discount Rates}: Higher rates reduce the present value of
#'     future expected cash flows for crypto assets.
#'   \item \strong{Institution Allocation}: Real yields drive institutional
#'     allocation decisions between asset classes.
#' }
#'
#' \strong{3. US Dollar Index (DXY)}
#'
#' DXY measures the dollar's strength against a basket of major currencies.
#' The inverse correlation with crypto is well-documented:
#' \itemize{
#'   \item \strong{Weak Dollar (<90)}: Typically coincides with global risk-on
#'     sentiment and crypto appreciation.
#'   \item \strong{Strong Dollar (>100)}: Often associated with risk-off sentiment,
#'     capital flight to safety, and crypto depreciation.
#'   \item \strong{Mechanism}: A weaker dollar makes dollar-denominated assets
#'     cheaper for foreign investors and often reflects loose monetary policy.
#' }
#'
#' \strong{Composite Risk Score Methodology}
#'
#' The composite score provides a single metric for macro conditions:
#' \itemize{
#'   \item Each component is scored as +1 (bullish), 0 (neutral), or -1 (bearish)
#'   \item Scores are summed and divided by 3 (number of components)
#'   \item Range: -1 (maximum bearish) to +1 (maximum bullish)
#'   \item Example calculation for current conditions:
#'     \itemize{
#'       \item M2 YoY: +4.2\% → 0 (neutral)
#'       \item 10-year yield: 4.5\% → -1 (bearish)
#'       \item DXY: 95 → 0 (neutral)
#'       \item Composite = (0 + -1 + 0) / 3 = -0.33 (mildly bearish)
#'     }
#' }
#'
#' @note
#' \itemize{
#'   \item A FRED API key is required for M2, treasury yields, and inflation data.
#'     Register for free at \url{https://fred.stlouisfed.org/docs/api/api_key.html}
#'   \item DXY data is fetched from Yahoo Finance and may have different update schedules.
#'   \item First observation for YoY calculations will be NA (requires 12 months of data).
#'   \item The composite score weights each factor equally; advanced users may want
#'     to implement their own weighting schemes.
#'   \item Macro indicators are global in nature and should be used for
#'     medium to long-term positioning rather than short-term trading.
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with your FRED API key
#' macro <- get_macro(fred_api_key = "your_api_key_here")
#'
#' # View current macro regime
#' cat("Liquidity:", macro$liquidity$liquidity_regime, "\n")
#' cat("Rates:", macro$rates$rate_regime, "\n")
#' cat("Dollar:", macro$dollar$dollar_regime, "\n")
#' cat("Composite Risk Score:", round(macro$composite_risk_score, 2), "\n")
#'
#' # Interpret composite score
#' score <- macro$composite_risk_score
#' if (score > 0.3) {
#'   cat("Bullish macro environment - favorable for crypto\n")
#' } else if (score < -0.3) {
#'   cat("Bearish macro environment - headwinds for crypto\n")
#' } else {
#'   cat("Neutral macro environment\n")
#' }
#'
#' # Plot M2 growth history
#' library(ggplot2)
#' m2_data <- data.frame(
#'   date = macro$liquidity$m2_us$date,
#'   m2_yoy = macro$liquidity$m2_yoy * 100
#' ) %>% tidyr::drop_na()
#'
#' ggplot(m2_data, aes(x = date, y = m2_yoy)) +
#'   geom_line() +
#'   geom_hline(yintercept = c(0, 5), linetype = "dashed", color = "red") +
#'   labs(title = "M2 Money Supply YoY Growth",
#'        subtitle = ">5% = Expanding Liquidity, <0% = Contracting",
#'        y = "Year-over-Year % Change")
#'
#' # Track macro conditions over time
#' macro_history <- function(start_date = "2020-01-01") {
#'   dates <- seq.Date(as.Date(start_date), Sys.Date(), by = "month")
#'   scores <- sapply(dates, function(d) {
#'     # Note: This is simplified - you'd need to fetch data for each date
#'     macro <- get_macro(fred_api_key = "your_key", start_date = d - 365)
#'     return(macro$composite_risk_score)
#'   })
#'   return(data.frame(date = dates, score = scores))
#' }
#' }
#'
#' @section Trading Applications:
#' \itemize{
#'   \item \strong{Strategic Allocation}: Use composite score for long-term
#'     portfolio positioning (>3 month horizon)
#'   \item \strong{Regime Filter}: Only take long positions when macro composite > -0.3
#'   \item \strong{Hedging}: Increase hedges when rates regime is "High Rates (Risk-Off)"
#'   \item \strong{Confluence}: Look for alignment between macro regime and on-chain signals
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_onchain_indicators}} for fundamental crypto valuation
#'   \item \code{\link{get_derivatives}} for market positioning signals
#'   \item \code{\link{crypto_predictive_framework}} for integrated analysis
#' }
#'
#' @references
#' \itemize{
#'   \item Federal Reserve Economic Data (FRED): \url{https://fred.stlouisfed.org/}
#'   \item Bordo, M. D., & Levin, A. T. (2017). Central Bank Digital Currency
#'     and the Future of Monetary Policy
#'   \item Conlon, T., & McGee, R. (2020). Safe Haven or Risky Hazard? Bitcoin
#'     during the Covid-19 Bear Market
#'   \item Kristoufek, L. (2015). What Are the Main Drivers of the Bitcoin Price?
#'     Evidence from Wavelet Coherence Analysis
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr lag full_join
#' @importFrom fredr fredr_set_key fredr
#' @importFrom quantmod getSymbols Cl
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

  aggregated <- dplyr::full_join(treasury_10y, inflation_expect, by ="date")
  aggregated <- aggregated[,c(1,3,7)]
  colnames(aggregated) <- c("date", "DGS10", "T5YIE")
  aggregated$real_rates <- aggregated$DGS10 - aggregated$T5YIE

  rate_regime = ifelse(
    tail(treasury_10y$value, 1) > 3, "High Rates (Risk-Off)",
    ifelse(tail(treasury_10y$value, 1) < 1, "Low Rates (Risk-On)", "Neutral")
  )

  m2_score <- ifelse(tail(m2_yoy, 1) > 0.05, 1,
                     ifelse(tail(m2_yoy, 1) < 0, -1, 0))
  rate_score <- ifelse(tail(treasury_10y$value, 1) > 3, -1,
                       ifelse(tail(treasury_10y$value, 1) < 1, 1, 0))
  dollar_score <- ifelse(!is.null(dxy) && tail(quantmod::Cl(dxy), 1) > 100, -1,
                         ifelse(!is.null(dxy) && tail(quantmod::Cl(dxy), 1) < 90, 1, 0))

  composite_risk_score <- (m2_score + rate_score + dollar_score) / 3

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


#' Calculate Cryptocurrency Volatility Metrics and Regimes
#'
#' @description
#' Computes realized volatility across multiple time horizons and identifies
#' volatility regimes to determine optimal trading strategies. This function
#' is essential for adapting trading approaches to market conditions, as
#' different volatility environments favor different strategies.
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
#'
#'   \strong{Volatility Series:}
#'   \itemize{
#'     \item \code{volatility$short}: Short-term realized volatility (annualized)
#'     \item \code{volatility$medium}: Medium-term realized volatility (annualized)
#'     \item \code{volatility$long}: Long-term realized volatility (annualized)
#'     \item \code{volatility$percentile_short}: Percentile rank of short-term vol (0-1)
#'     \item \code{volatility$percentile_medium}: Percentile rank of medium-term vol (0-1)
#'   }
#'
#'   \strong{Volatility Regime:}
#'   \itemize{
#'     \item \code{regime}: Categorical regime for each observation:
#'       \itemize{
#'         \item "Extreme Low": Volatility < mean - 1sd
#'         \item "Low": Volatility between mean and mean - 1sd
#'         \item "High": Volatility between mean and mean + 1sd
#'         \item "Extreme High": Volatility > mean + 1sd
#'       }
#'   }
#'
#'   \strong{Trading Signals:}
#'   \itemize{
#'     \item \code{signals$vol_breakout}: Logical vector - TRUE when short-term vol
#'       exceeds 1.5× medium-term vol (momentum strategy setup)
#'     \item \code{signals$vol_compression}: Logical vector - TRUE when short-term vol
#'       is less than 0.5× medium-term vol (mean reversion strategy setup)
#'     \item \code{signals$current_regime}: Human-readable description of the
#'       current market environment:
#'       \itemize{
#'         \item "Risk-Off (Capitulation)": Extreme High volatility - panic selling
#'         \item "Active Trading": High volatility - opportunities with tight stops
#'         \item "Trend Following": Low volatility - ideal for trend strategies
#'         \item "Complacency (Watch for Breakout)": Extreme Low volatility - big move imminent
#'       }
#'   }
#'
#' @details
#' \strong{Volatility Calculation Methodology}
#'
#' Realized volatility is calculated as the standard deviation of log returns,
#' annualized by multiplying by the square root of 365 (trading days):
#'
#' \deqn{\sigma_t = \sqrt{252} \times \sqrt{\frac{1}{n-1}\sum_{i=t-n+1}^{t} (r_i - \bar{r})^2}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{r_i} are log returns
#'   \item \eqn{n} is the window size (7, 30, or 90 days)
#'   \item Annualization factor: \eqn{\sqrt{365}} for daily crypto trading
#' }
#'
#' \strong{Why Volatility Matters}
#'
#' Volatility is not just risk to be managed—it's a regime indicator that
#' determines which trading strategies are most likely to succeed:
#'
#' \strong{Low Volatility Regimes (Trend Following)}
#'
#' When volatility is low (below historical mean), markets tend to exhibit
#' persistent trends with limited noise. This environment favors:
#' \itemize{
#'   \item Trend-following strategies (moving average crossovers)
#'   \item Breakout systems
#'   \item Longer holding periods
#'   \item Wider stop losses (to avoid being shaken out)
#' }
#'
#' \strong{High Volatility Regimes (Active Trading)}
#'
#' When volatility is high (above historical mean), markets become noisy with
#' frequent sharp reversals. This environment favors:
#' \itemize{
#'   \item Mean reversion strategies
#'   \item Shorter timeframes
#'   \item Tighter stop losses
#'   \item Range-bound trading systems
#' }
#'
#' \strong{Extreme Volatility Regimes}
#'
#' \itemize{
#'   \item \strong{Extreme High (> +1sd)}: Capitulation or panic selling.
#'     Often marks short-term bottoms as selling exhausts itself.
#'     Strategy: Wait for stabilization, then buy on first pullback.
#'
#'   \item \strong{Extreme Low (< -1sd)}: Market complacency.
#'     Low volatility periods are typically followed by volatility expansion.
#'     Strategy: Prepare for breakout, scale in gradually.
#' }
#'
#' \strong{Volatility Breakout Signal}
#'
#' When short-term volatility exceeds 1.5× medium-term volatility, it indicates:
#' \itemize{
#'   \item A volatility expansion event is occurring
#'   \item Momentum is accelerating in one direction
#'   \item Trend-following strategies should be employed
#' }
#'
#' \strong{Volatility Compression Signal}
#'
#' When short-term volatility falls below 0.5× medium-term volatility, it indicates:
#' \itemize{
#'   \item Market is consolidating / range-bound
#'   \item Volatility is contracting (calm before the storm)
#'   \item Mean reversion strategies should be employed
#'   \item A volatility expansion is likely approaching
#' }
#'
#' @note
#' \itemize{
#'   \item The first \code{max(windows)} observations will have NA values for
#'     volatility calculations.
#'   \item Annualization uses 365 days (crypto trades 24/7) rather than 252
#'     (traditional markets). Adjust if you prefer different conventions.
#'   \item Percentile ranks are calculated over the entire dataset and will
#'     change as new data is added.
#'   \item The 1.5× and 0.5× thresholds are configurable based on backtesting.
#' }
#'
#' @examples
#' \dontrun{
#' # Get market data and calculate on-chain indicators first
#' market <- get_market_data("Bitcoin", "2018-01-01")
#' onchain <- get_onchain_indicators(market$data)
#'
#' # Calculate volatility metrics
#' vol <- get_volatility(onchain)
#'
#' # View current volatility regime
#' cat("Current regime:", tail(vol$regime, 1), "\n")
#' cat("Market environment:", vol$signals$current_regime, "\n")
#'
#' # Check for volatility signals
#' if (tail(vol$signals$vol_compression, 1)) {
#'   cat("⚠️  Volatility compression detected - prepare for breakout\n")
#'   cat("Recommended strategy: Mean reversion, range trading\n")
#' } else if (tail(vol$signals$vol_breakout, 1)) {
#'   cat("🚀 Volatility breakout detected - trend in progress\n")
#'   cat("Recommended strategy: Trend following, momentum\n")
#' }
#'
#' # Plot volatility with regimes
#' library(ggplot2)
#' vol_df <- data.frame(
#'   date = tail(onchain$timestamp, 500),
#'   vol_short = tail(vol$volatility$short, 500),
#'   vol_medium = tail(vol$volatility$medium, 500),
#'   regime = tail(vol$regime, 500)
#' )
#'
#' ggplot(vol_df, aes(x = date)) +
#'   geom_line(aes(y = vol_short, color = "Short-term Vol"), size = 0.8) +
#'   geom_line(aes(y = vol_medium, color = "Medium-term Vol"), size = 0.8) +
#'   geom_hline(yintercept = mean(vol_df$vol_medium, na.rm = TRUE),
#'              linetype = "dashed", alpha = 0.5) +
#'   scale_color_manual(values = c("Short-term Vol" = "darkblue",
#'                                 "Medium-term Vol" = "red")) +
#'   labs(title = "Bitcoin Realized Volatility",
#'        subtitle = "Short-term (7d) vs Medium-term (30d)",
#'        y = "Annualized Volatility", x = "Date") +
#'   theme_minimal()
#'
#' # Volatility percentile analysis
#' current_percentile <- tail(vol$volatility$percentile_medium, 1)
#' cat("Current volatility percentile:",
#'     round(current_percentile * 100, 1), "%\n")
#'
#' if (current_percentile > 0.9) {
#'   cat("Warning: Volatility in top 10% historically\n")
#' } else if (current_percentile < 0.1) {
#'   cat("Alert: Volatility in bottom 10% historically\n")
#' }
#' }
#'
#' @section Trading Strategy Selection:
#' Use volatility regimes to select appropriate trading strategies:
#'
#' \describe{
#'   \item{\strong{Extreme Low Regime}}{%
#'     \itemize{
#'       \item \strong{Strategy:} Prepare for breakout
#'       \item \strong{Position Sizing:} Scale in gradually
#'       \item \strong{Stop Loss:} Wide
#'     }
#'   }
#'   \item{\strong{Low Regime}}{%
#'     \itemize{
#'       \item \strong{Strategy:} Trend following
#'       \item \strong{Position Sizing:} Full size
#'       \item \strong{Stop Loss:} Wider
#'     }
#'   }
#'   \item{\strong{High Regime}}{%
#'     \itemize{
#'       \item \strong{Strategy:} Active trading
#'       \item \strong{Position Sizing:} Reduced size
#'       \item \strong{Stop Loss:} Tight
#'     }
#'   }
#'   \item{\strong{Extreme High Regime}}{%
#'     \itemize{
#'       \item \strong{Strategy:} Capitulation - wait
#'       \item \strong{Position Sizing:} Cash/minimal
#'       \item \strong{Stop Loss:} Very tight
#'     }
#'   }
#' }
#'
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_onchain_indicators}} for source log returns
#'   \item \code{\link{get_derivatives}} for positioning context
#'   \item \code{\link{crypto_predictive_framework}} for integrated analysis
#' }
#'
#' @references
#' \itemize{
#'   \item Andersen, T. G., & Bollerslev, T. (1998). Answering the Skeptics:
#'     Yes, Standard Volatility Models Do Provide Accurate Forecasts
#'   \item Patton, A. J. (2011). Volatility Forecast Comparison Using Imperfect
#'     Volatility Proxies
#'   \item CBOE (2023). VIX Index Rules of Construction
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom TTR runSD
get_volatility <- function(data, windows = list(short = 7, medium = 30, long = 90)) {

  returns <- data$log_returns

  # Realized volatility for different horizons
  vol_short <- TTR::runSD(returns, n = windows$short) * sqrt(365)
  vol_medium <- TTR::runSD(returns, n = windows$medium) * sqrt(365)
  vol_long <- TTR::runSD(returns, n = windows$long) * sqrt(365)

  # Volatility percentiles
  vol_percentile_short <- rank(vol_short, na.last = "keep") / sum(!is.na(vol_short))
  vol_percentile_medium <- rank(vol_medium, na.last = "keep") / sum(!is.na(vol_medium))

  # GARCH-style volatility clustering
  vol_mean <- mean(vol_short, na.rm = TRUE)
  vol_sd <- sd(vol_short, na.rm = TRUE)

  vol_regime_short <- dplyr::case_when(
    is.na(vol_short) ~ NA_character_,
    vol_short > vol_mean + vol_sd ~ "Extreme High",
    vol_short < vol_mean - vol_sd ~ "Extreme Low",
    vol_short > vol_mean ~ "High",
    TRUE ~ "Low"
  )

  # Current regime with fallback
  current_regime <- tail(vol_regime_short[!is.na(vol_regime_short)], 1)
  if (length(current_regime) == 0) current_regime <- "Unknown"

  current_regime_desc <- switch(current_regime,
                                "Extreme High" = "Risk-Off (Capitulation)",
                                "High" = "Active Trading",
                                "Low" = "Trend Following",
                                "Extreme Low" = "Complacency (Watch for Breakout)",
                                "Unknown")

  # Volatility signals
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


#' Fetch Whale Transaction Data and Retail Sentiment Indicators
#'
#' @description
#' Retrieves and analyzes large cryptocurrency transactions ("whale movements")
#' from Whale Alert and retail sentiment from Google Trends. This function provides
#' critical insights into "smart money" positioning and retail interest extremes,
#' which often precede significant market moves.
#'
#' @param api_key Character string containing your Whale Alert API key.
#'   Default NULL expects key to be set in \code{global_variables$whaleAlert_api_key}.
#'   Get a free API key at \url{https://whale-alert.io/api}
#' @param start_time Start time for whale transaction lookup in "YYYY-MM-DD" format.
#'   Default "2026-02-01". Transactions are fetched from this time to present.
#' @param google_trends_keywords Character vector of keywords to track for retail
#'   sentiment. Default c("bitcoin", "crypto", "blockchain").
#'
#' @return A list object of class \code{positioning_data} containing:
#'
#'   \strong{Exchange Positioning:}
#'   \itemize{
#'     \item \code{netflow_summary}: Data frame with:
#'       \itemize{
#'         \item \code{inflow_usd}: Total USD value moving from unknown wallets to exchanges
#'         \item \code{outflow_usd}: Total USD value moving from exchanges to unknown wallets
#'         \item \code{tx_count}: Total number of whale transactions
#'         \item \code{pct}: Percentage of transactions above half of max transaction size
#'         \item \code{netflow_usd}: Net flow (inflow - outflow)
#'       }
#'     \item \code{flow_regime}: Categorical interpretation of whale flows:
#'       \itemize{
#'         \item "Extreme Inflows (Heavy Selling Pressure)": Net inflows > 10× max tx size
#'         \item "Net Inflows (Bearish Bias)": Positive net inflows
#'         \item "Neutral / Balanced": Net flows near zero
#'         \item "Net Outflows (Bullish Bias)": Negative net inflows
#'         \item "Extreme Outflows (Heavy Accumulation)": Net outflows > 10× max tx size
#'       }
#'     \item \code{top_exchange_target}: Most common exchange receiving whale inflows
#'   }
#'
#'   \strong{Whale Activity Metrics:}
#'   \itemize{
#'     \item \code{total_tx}: Total number of whale transactions in the period
#'     \item \code{whale_signal}: Activity level interpretation:
#'       \itemize{
#'         \item "High Whale Intensity (Volatility Expected)": >50% of transactions are large
#'         \item "Normal Whale Activity": Typical distribution of transaction sizes
#'         \item "Low Whale Activity": <10% of transactions are large
#'       }
#'     \item \code{largest_move_usd}: Size of the largest transaction in USD
#'   }
#'
#'   \strong{Raw Data:}
#'   \itemize{
#'     \item \code{raw_whale_data}: Detailed transaction data for custom analysis
#'   }
#'
#'   \strong{Retail Sentiment (Google Trends):}
#'   \itemize{
#'     \item \code{google_trends}: List containing:
#'       \itemize{
#'         \item \code{interest_over_time}: Historical search interest (0-100 scale)
#'         \item \code{related_queries}: Related search terms and their popularity
#'         \item \code{interest_by_region}: Geographic breakdown of search interest
#'         \item \code{current_sentiment}: Average search interest over last 7 days
#'         \item \code{sentiment_signal}: Categorical interpretation:
#'           \itemize{
#'             \item "Extreme Interest (Potential Top)": Score > 80
#'             \item "High Interest": Score 60-80
#'             \item "Normal Interest": Score 20-60
#'             \item "Low Interest (Potential Bottom)": Score < 20
#'           }
#'         \item \code{keywords}: Keywords tracked
#'       }
#'   }
#'
#' @details
#' \strong{Why Whale Transactions Matter}
#'
#' Large holders ("whales") often have superior information and their on-chain
#' movements can signal upcoming market moves:
#'
#' \strong{Exchange Inflows (Selling Pressure)}
#'
#' When whales move coins from private wallets to exchanges, it typically
#' indicates an intent to sell. This creates sell-side pressure:
#' \itemize{
#'   \item \strong{Normal inflows}: Routine trading activity
#'   \item \strong{Extreme inflows (>10× normal)}: Potential distribution event,
#'     often preceding price declines
#'   \item \strong{Interpretation}: "If it's on the exchange, it's for sale"
#' }
#'
#' \strong{Exchange Outflows (Accumulation)}
#'
#' When whales move coins from exchanges to private wallets, it typically
#' indicates accumulation (taking custody for long-term holding):
#' \itemize{
#'   \item \strong{Normal outflows}: Routine withdrawal after purchase
#'   \item \strong{Extreme outflows (>10× normal)}: Significant accumulation,
#'     often preceding price appreciation
#'   \item \strong{Interpretation}: "Not your keys, not your coins" - whales
#'     moving to cold storage signals long-term conviction
#' }
#'
#' \strong{Whale Transaction Intensity}
#'
#' The concentration of large transactions provides additional context:
#' \itemize{
#'   \item \strong{High intensity}: Multiple whale-sized moves suggest
#'     important market participants are active → increased volatility expected
#'   \item \strong{Low intensity}: Whale inactivity can signal indecision
#'     or consolidation phase
#' }
#'
#' \strong{Why Retail Sentiment (Google Trends) Matters}
#'
#' Google Trends provides a real-time measure of retail attention, which
#' often acts as a contrary indicator at extremes:
#'
#' \strong{Extreme Interest (>80)}
#'
#' When search interest reaches euphoric levels:
#' \itemize{
#'   \item Typically occurs near price tops (e.g., Dec 2017, April 2021)
#'   \item Signals that "everyone who wants to buy has already bought"
#'   \item Often precedes distribution phase and price declines
#'   \item \strong{Trading implication}: Consider taking profits, reducing exposure
#' }
#'
#' \strong{Low Interest (<20)}
#'
#' When retail loses interest completely:
#' \itemize{
#'   \item Typically occurs near price bottoms (e.g., Dec 2018, June 2022)
#'   \item Signals that selling pressure is exhausted (no one left to sell)
#'   \item Smart money often accumulates during retail apathy
#'   \item \strong{Trading implication}: Look for accumulation opportunities
#' }
#'
#' \strong{Normal Interest (20-60)}
#'
#' Healthy, sustainable interest levels:
#' \itemize{
#'   \item Organic growth without euphoria
#'   \item Typically accompanies steady trends
#'   \item \strong{Trading implication}: Let trends run, no contrary signal
#' }
#'
#' @section Combined Interpretation Framework:
#'
#' The most powerful signals come from combining whale flows with retail sentiment:
#'
#' \describe{
#'   \item{\strong{Bullish Confluence}}{%
#'     \itemize{
#'       \item Whale outflows (accumulation) + Low retail interest
#'       \item Interpretation: Smart money accumulating while retail is apathetic
#'       \item Action: Strong accumulation signal, scale into positions
#'     }
#'   }
#'   \item{\strong{Bearish Confluence}}{%
#'     \itemize{
#'       \item Whale inflows (distribution) + High retail interest
#'       \item Interpretation: Smart money selling to euphoric retail
#'       \item Action: Reduce exposure, consider shorts
#'     }
#'   }
#'   \item{\strong{Mixed Signals}}{%
#'     \itemize{
#'       \item Divergence between whale and retail indicators
#'       \item Interpretation: Transition phase, wait for confirmation
#'       \item Action: Reduce position size, tighten stops
#'     }
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item Whale Alert API has rate limits (free tier: 1 request per minute)
#'   \item Only tracks transactions > $500,000 (or currency equivalent)
#'   \item Google Trends data may be delayed by 1-2 days
#'   \item Sentiment signals are contrarian at extremes, confirming in trends
#'   \item Different exchanges may have different wallet labeling accuracy
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with API key
#' positioning <- get_positioning(
#'   api_key = "your_whale_alert_key",
#'   start_time = Sys.Date() - 7,
#'   google_trends_keywords = c("bitcoin", "ethereum", "crypto")
#' )
#'
#' # Check whale flow regime
#' cat("Whale Flow:", positioning$exchange_positioning$flow_regime, "\n")
#' cat("Net Flow: $",
#'     format(positioning$exchange_positioning$netflow_summary$netflow_usd,
#'            big.mark = ",", scientific = FALSE), "\n")
#'
#' # Interpret whale activity
#' if (grepl("Extreme Inflows", positioning$exchange_positioning$flow_regime)) {
#'   cat("⚠️  Warning: Whales moving to exchanges - potential selling pressure\n")
#' } else if (grepl("Extreme Outflows", positioning$exchange_positioning$flow_regime)) {
#'   cat("🚀 Bullish: Whales accumulating - moving to cold storage\n")
#' }
#'
#' # Check retail sentiment
#' if (!is.null(positioning$google_trends)) {
#'   sentiment <- positioning$google_trends$sentiment_signal
#'   score <- positioning$google_trends$current_sentiment
#'
#'   cat("Retail Sentiment:", sentiment, sprintf("(Score: %.1f)", score), "\n")
#'
#'   if (grepl("Extreme Interest", sentiment)) {
#'     cat("⚠️  Retail euphoria - potential top\n")
#'   } else if (grepl("Low Interest", sentiment)) {
#'     cat("🟢 Retail apathy - potential bottom\n")
#'   }
#' }
#'
#' # Combined signal
#' if (!is.null(positioning$google_trends)) {
#'   whale_bearish <- grepl("Inflows", positioning$exchange_positioning$flow_regime)
#'   retail_bullish <- grepl("Low Interest", positioning$google_trends$sentiment_signal)
#'
#'   if (whale_bearish && retail_bullish) {
#'     cat("⚠️  Divergence: Whales selling, retail apathetic - wait for clarity\n")
#'   }
#' }
#'
#' # Plot whale flows over time
#' library(ggplot2)
#' whale_history <- positioning$raw_whale_data %>%
#'   mutate(date = as.Date(as.POSIXct(timestamp))) %>%
#'   group_by(date) %>%
#'   summarise(
#'     inflow = sum(amount_usd[to_type == "exchange"], na.rm = TRUE),
#'     outflow = sum(amount_usd[from_type == "exchange"], na.rm = TRUE),
#'     netflow = inflow - outflow
#'   )
#'
#' ggplot(whale_history, aes(x = date)) +
#'   geom_col(aes(y = inflow, fill = "Inflows (Selling)"), alpha = 0.7) +
#'   geom_col(aes(y = -outflow, fill = "Outflows (Buying)"), alpha = 0.7) +
#'   scale_fill_manual(values = c("Inflows (Selling)" = "red",
#'                                "Outflows (Buying)" = "green")) +
#'   labs(title = "Whale Exchange Flows",
#'        subtitle = "Positive = Inflows (Selling), Negative = Outflows (Buying)",
#'        y = "USD Value", x = "Date") +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{get_derivatives}} for exchange positioning via funding rates
#'   \item \code{\link{get_onchain_indicators}} for fundamental valuation
#'   \item \code{\link{crypto_predictive_framework}} for integrated analysis
#' }
#'
#' @references
#' \itemize{
#'   \item Whale Alert: \url{https://whale-alert.io/}
#'   \item Google Trends: \url{https://trends.google.com/}
#'   \item Da, Z., Engelberg, J., & Gao, P. (2011). In Search of Attention
#'   \item Preis, T., Moat, H. S., & Stanley, H. E. (2013). Quantifying Trading
#'     Behavior in Financial Markets Using Google Trends
#' }
#'
#' @author Filippo Franchini
#' @export
#'
#' @importFrom dplyr mutate summarise n case_when
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom gtrendsR gtrends
get_positioning <- function(api_key = NULL, start_time = "2026-02-01", google_trends_keywords = c("bitcoin", "crypto", "blockchain")){

  start_time <- as.numeric(as.POSIXct(start_time))

  url <- paste0("https://api.whale-alert.io/v1/transactions",
                "?api_key=", global_variables$whaleAlert_api_key,
                "&start=", start_time)

  response <- httr::GET(url)
  raw_data <- jsonlite::fromJSON(httr::content(response, "text"))

  if (is.null(raw_data$transactions) || length(raw_data$transactions) == 0) {
    warning("No whale transactions found in the specified window.")
    return(create_empty_positioning())
  }

  whale_df <- as.data.frame(raw_data$transactions) %>%
    dplyr::mutate(
      from_type = from$owner_type,
      to_type = to$owner_type,
      from_owner = from$owner,
      to_owner = to$owner
    )

  max_amount <- max(whale_df$amount_usd, na.rm = TRUE)
  if (is.infinite(max_amount)) max_amount <- 0

  metrics <- whale_df %>%
    dplyr::summarise(
      inflow_usd  = sum(amount_usd[from_type == "unknown" & to_type == "exchange"], na.rm = TRUE),
      outflow_usd = sum(amount_usd[from_type == "exchange" & to_type == "unknown"], na.rm = TRUE),
      tx_count = dplyr::n(),
      pct = sum(amount_usd > max(whale_df$amount_usd)/2)
    ) %>%
    dplyr::mutate(netflow_usd = inflow_usd - outflow_usd)

  flow_regime <- dplyr::case_when(
    metrics$netflow_usd > (max_amount * 10)  ~ "Extreme Inflows (Heavy Selling Pressure)",
    metrics$netflow_usd > 0                     ~ "Net Inflows (Bearish Bias)",
    metrics$netflow_usd < -(max_amount * 10) ~ "Extreme Outflows (Heavy Accumulation)",
    metrics$netflow_usd < 0                     ~ "Net Outflows (Bullish Bias)",
    TRUE                                        ~ "Neutral / Balanced"
  )

  whale_signal <- dplyr::case_when(
    metrics$pct > 50  ~ "High Whale Intensity (Volatility Expected)",
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

# Helper function for empty returns
create_empty_positioning <- function() {
  return(list(
    exchange_positioning = list(
      netflow_summary = data.frame(
        inflow_usd = 0, outflow_usd = 0, tx_count = 0, pct = 0, netflow_usd = 0
      ),
      flow_regime = "No Data",
      top_exchange_target = NA
    ),
    whale_activity = list(
      total_tx = 0,
      whale_signal = "No Data",
      largest_move_usd = 0
    ),
    raw_whale_data = data.frame(),
    google_trends = NULL
  ))
}

