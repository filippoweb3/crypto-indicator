---
editor_options: 
  markdown: 
    wrap: 72
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📈 CryptoIndicator: A Comprehensive Predictive Framework for Digital Assets

------------------------------------------------------------------------

## Overview

**CryptoIndicator** is an R package that provides a systematic,
research-grade framework for analyzing cryptocurrency markets. It
integrates five distinct categories of indicators into a coherent
decision system, generating actionable signals across multiple time
horizons.

The package is designed for:

- **Quantitative researchers** building systematic trading strategies 
- **Portfolio managers** seeking macro-driven asset allocation signals 
- **Individual traders** wanting data-driven edge in crypto markets 
- **Academic researchers** studying digital asset market dynamics

------------------------------------------------------------------------

## Key Features

| Module             | Indicators                                     | Primary Use Case                          |
|------------------|----------------------|---------------------------------|
| **⛓️ On-Chain**    | MVRV, NVT, Puell Multiple, Realized Price      | Cycle identification, valuation extremes  |
| **📈 Derivatives** | Open Interest, Funding Rates, Long/Short Ratio | Positioning analysis, crowded trades      |
| **🌎 Macro**       | M2 Liquidity, 10Y Treasury, DXY, Real Rates    | Global risk environment, asset allocation |
| **📊 Volatility**  | Realized Vol (7/30/90d), Volatility Regimes    | Strategy selection, position sizing       |
| **🐋 Positioning** | Whale Flows, Exchange Netflows, Google Trends  | Smart money tracking, sentiment extremes  |

The framework synthesizes these signals into:

- **Short-term momentum/mean reversion signals** (hours to days) 
- **Medium-term trend and valuation signals** (weeks to months)\
- **Long-term cycle positioning** (months to years) 
- **Real-time risk-off alerts** (GREEN/YELLOW/RED) 
- **Market regime classification** with specific tactical guidance

------------------------------------------------------------------------

## Installation

### From GitHub (development version)

``` r
# install.packages("devtools")
devtools::install_github("filippoweb3/crypto-indicator")
```

### From source

``` r
# Clone the repository and install
git clone https://github.com/filippoweb3/crypto-indicator.git
install.packages("path/to/crypto-indicator", repos = NULL, type = "source")
```

### Dependencies

The package relies on several well-maintained CRAN packages that will be
installed automatically: 

- `crypto2`, `cryptoQuotes` - Market and derivatives data 
- `fredr`, `quantmod` - Macroeconomic indicators 
- `gtrendsR` - Google Trends sentiment 
- `httr`, `jsonlite` - API interactions 
- `dplyr`, `tidyr`, `zoo`, `TTR` - Data manipulation

------------------------------------------------------------------------

## Quick Start

``` r
library(CryptoIndicator)

# Run the complete analysis pipeline
result <- crypto_predictive_framework(
  assets = "Bitcoin",
  start_date = "2015-01-01",
  include_google_trends = TRUE
)

# View the executive summary
print_crypto_summary(result)

# Access specific signals
result$decisions$risk_off$color           # Current risk level
result$signals$onchain$mvrv$regime        # Market valuation regime
result$decisions$regime_allocation$allocation$strategic  # Recommended allocation
```

------------------------------------------------------------------------

## API Configuration (Optional)

Some data sources require free API keys. Store them in your `.Renviron`
file for security:

``` r
# Edit your .Renviron file
usethis::edit_r_environ()

# Add these lines (replace with your actual keys)
FRED_API_KEY="your_fred_key_here"
WHALE_ALERT_API_KEY="your_whale_alert_key_here"
```

| Data Source          | Required | Rate Limits               | Registration                                                     |
|------------------|-----------------|------------------|-------------------|
| FRED (St. Louis Fed) | ✅ Yes   | 120 requests/min          | [Get API Key](https://fred.stlouisfed.org/docs/api/api_key.html) |
| Whale Alert          | ✅ Yes   | 1 request/min (free tier) | [Get API Key](https://whale-alert.io/ )                  |
| Binance              | ❌ No    | \-                        | \-                                                               |
| CoinMarketCap        | ❌ No    | \-                        | \-                                                               |
| Google Trends        | ❌ No    | \-                        | \-                                                               |

------------------------------------------------------------------------

## Example Output

When you run the framework, you'll see a progress indicator:

``` r
📊 Collecting market data...
  ✅ Retrieved 3,287 days of data
⛓️  Calculating on-chain indicators...
  ✅ MVRV: Bear Market
📈 Fetching derivatives data...
  ✅ Funding regime: Neutral
🌎 Fetching macro indicators...
  ✅ Liquidity: Stable
📊 Calculating volatility metrics...
  ✅ Volatility regime: Low
🐋 Fetching whale and sentiment data...
  ✅ Whale flow: Net Outflows (Bullish Bias)
🎯 Generating decision framework...

✅ Framework complete! Use print_crypto_summary() to view results
```

The `print_crypto_summary()` function produces a comprehensive,
self-explanatory report:

```         
============================================================
🚀 CRYPTO PREDICTIVE FRAMEWORK - COMPREHENSIVE SUMMARY
============================================================
Generated: 2024-01-15 14:30:25
Asset: Bitcoin
Data Range: 2015-01-01 to 2024-01-15
Status: SUCCESS

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🟡  RISK-OFF SIGNAL: [ YELLOW ]  🟡
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  What this means:
  • ⚠️ Elevated risk factors present
  • 📉 Consider reducing leverage and tightening stops

Risk Factors:
  • Negative macro composite

Recommended Actions:
  → Reduce leverage
  → Tighten stops
  → Take partial profits
```

------------------------------------------------------------------------

## Methodology

The package employs a hierarchical decision system that processes data
through five specialized modules.

### Flow Charts

- [Signal Aggregation Framework](https://mermaid.live/edit#pako:eNp9VVtv4jgU_itWHvapFEhSWtBqpJYE2hkuHWDodA0PbmKC1cRmHbs7Lep_32Obtni3mkg-iv195_jcbO-DTOQ06AWFJLstWiQrjuC7xGMiH6lCCVFkjRqNL-hqb_7RrRQZrWvGi1dHdfLKcvp4yhv9LWEcjUWuS7o-RhOcUMmeiGJPtP6MkMKumRSfQQO8FCVolkw9f4YP8a2omWKCg2c-wcm-c7CNx8vZEs3AlFh7SIgny8UB8HUifKtpWaKxLhXbvds9oDGeUVKyF5pDbljm75q4wNt4uqMc3XBFJa3V2gNDPNA8N27D5rT2wQiPBC-a862QyvPaydTlDaIK0Yj9rVkO-Vl7WIjbrXu0kJTUWv4Hi3Dy895fcuF4rjg5cHVo4_McvUcMRVl7cIij1u_wCHd_i8fHdZ7RglV-Qoeu2m18tyUlRYNS_PPm5gEKcfor2xJeUDShavN_QoSHQhSgDDnhuR9kv43-gFYwIjIitirXeECJ0pKilBeMU-hiXrzVyWgkRiOJHPmQTrOemvXUWErjY3BgwIEBBwYceODQgEMDDj2LTl7bpZt9QjNWQ7sfXPIO443lfMW2axoLKqs_H2Tzy7XQskZKwKF-fgvbUb_hMc2Zrj64d5Q-Wu5YcLX12SPbkx9cRzHke0qkn9CvVmOMx6KiXOkKzTMh30rqwAlsTjgU-4lKG9GcKr3zrHyzxCm2FfNMOOQWL0mpzengqC-qnbkKfMb39_sBzdnLR_mcHFnODPefM-iLo5tkfYzP8TUpn8xBval2JFMeuMBzJeHQFCxDl2UpMuuNt8sYSjqBMYVxC-M7jBmMOYyFNfJjP2P1I7qsa7hiTcK8sv6wnCU2nMZ0s4FICk5KW4PhLE0nzft0NJreNWdpsj5WucPuJKF-SeDq3jDnnFW80mXZvIK6NS-zTFe6tFAzYbWS7EHbyYRqiK30glmC03fW-k98mRkaeYDcOZfqdXACLwrLg56Smp4EFfQKMdNgb9RXgdrSiq6CHvzm8NKsghV_BZ0d4X8JUb2pSaGLbdDbgEWY6V0OCU4YgbfqgwIdQWVfaK6C3kXXmgh6--BX0GtE3c5pFHfO41Z8EXfOuucnwXPQi1unF3H3PIziVtQ9O4_aryfBi900PI3aYbvTjgDrtMKz8PVf-fwPwQ)

- [Risk Assessment Logic](https://mermaid.live/edit#pako:eNptk9Fu2jAUhl_lyLcDltgJhahiqoD1BrqJdqs64MJNTLAIceSYljYg7SF2X63PsCfaE-wRZieFkEKkWHHO-c_3n5M4Q74IGPLQLBKP_pxKBTe9SQz6ulZ6lw1FzJWQMOLpAj5TXz-nW6jXO9CdM39hZ99FRBWPuHqCDthp8GlbyIu1SDL5mzuWbuAiCOyxXuBAZkpPj9KvxKbY4mxIfSng2heSwTnUrQapQkzR0hI-4uMKH-f8ouZ7NK6iSdaV4jFgAQxEHB5BcQklR1BSgZIc-lWkXHE90Tg8QpMSLVaxGudrZezTd3hS4E1ehZ4LTajHfJ5qXJZXGbAHFlVa2MVzslW3d6ANXErG4vG_l5dXuBz1-1fTEwJcpt-xSP8-Jv833PUHgy-3pwTkQ6kYsUCn__oDo36v0ldOzt1f-GZU9vjvz1f9tXis9A2zVRRB8jbG9PxefuyY-JWQSxpBqkQCkUhTVoa-JaAEkDVEun9JQ_aGKzwfkHBO0sZWPtsn78vc8HCutDODKIvf0AWDRJ8TrumJFDOuyuCQrsFuuEdkjTjAkqJB8cCMz1TR-4j5gh8019UdlY6CE-3fUq5gpk-pketD9UxNfB-2DiygGgolD5Cn5IrV0JLpwZktyoy5CVJztmQT5OnHgMrFBE3irdYkNP4hxHInk2IVzpE3o1Gqd6skoIr1OA0lXe7fShYHTOZ_I_KcNs6LIC9Da-TZxGkQyyFNt3XmEGKRZg09Ia_eslsNq4Vb7bZLWti1nG0NPedc3LBd3HYcy226tmOTs-b2P2bAjxc)

- [Regime Classification Matrix](https://mermaid.live/edit#pako:eNpdkDFPwzAQhf-KdXNSuU4cpx6QaDswwMLAgLIciZtYOHZxbNFS9b-TJpRK9eR79713pztB7RoFEr4iNh5t2HToQ2XJ-IIORpEX9J8qkFfV6l6RjcFh0DtdY9DOztwhxYMeyLP7Jm9o4tQhafpAnnTb3aQZPs7wWqHXQzem195N8Doa86_M7HWndEm2eghef8Rb0H-TTdY7MZsm3Ik5eazr2Efztw8k0HrdgAw-qgR65Xu8lHC6GCsInepVBXL8NuMVKqjsefTs0b47119t3sW2A7lDM4xV3DcY1FZj6_GGKNsov3HRBpAiy6cMkCc4gCz5gq5YkbOcck5zkSVwHKEF5fmS0YwKkfGl4OcEfqahdFEyyrjgxUoUBSvK8vwLq_2Qvw)

- [Time Horizon Integration](https://mermaid.live/edit#pako:eNp1kttuGjEQhl9lZKlSowLywp7Yu4ooSqVuFQVE1Yoba3fYteID8iENIN49NtAAF_HVjP37n2_G3pNGt0gq4rhEwRWuFITluBMItReODxfhZG2YRJjzTjEBP5TDzjDHtTqpLTYxgXmvjRsu0Ej4-qi9seA03LOtvTvp4qq1ROW8hIVhLVcdVPDg1TEKlggbbXk0CxtXl5ApeMZXNDbWqWCpRagvuNtCo-XGoLUfNHE9nV0C8g6DfpwNM_oF9BoatuGOiVvwGlvu5Zn8N-LLkbzWyvX2DuDiuzCoWnjQQuh_J_h6-byEb1Czxmhw8fiiXjLhj2O6Yf-1XAC-BalE-zlxRodF9jnxT626M-8JMwL_QWauZz3bNuEVny4TDb6PTLzGyDY9tl7gRT138QE63sD30F5zAj83uNPqGnamzcU3aIpsmNCb-ZIB6QxvSeWMxwGRgZTFlOyjy4q4PrS_IlUIW2ZeVmSlDuHOhqm_Wsv_14z2XU-qNRM2ZH7TBsB7zsLnkx-7ceRoZtorR6qElvToQqo9eQv5JBvRpEzG0yQraJFn6YBsSVUmoyIdp5O8HKdFOZmWhwHZHevSUZHRssgpnaZ5Os4n08M7c9X9Gg)

- Signal Weighting by Regime
  - [Bull](https://mermaid.live/edit#pako:eNo9js1qwzAQhF9F7DkJ8r-sY5NraKHQQtFlqbf2UlkKspw2DXn3Kg6JTpr5Znb3DJ--I9BwYBLT4H92GNE4kV7kaEkYeOXeoRXvxP0QJ7EWT7O1Yo_hm6KBW9bAs1tvB2RnQGiRV3d7R4GPGPlI00IKeSd7_Az-ln54b96mrOV4WkD2GPPiJ47sHbv-RiSsoA_cgY5hphWMFEa8SjhfOwbiQCMZ0OnbpVOvh15S54Duw_vxXgt-7gfQX2inpOZDh5F2jH3A8eEGch2FrZ9dBJ1JVS5TQJ_hN-mi2shMZXmbVY1s6irRE2iVbZoyL4ta5WWjilZdVvC37JWbppKqqaVsy7rM66K9_ANBunMF)
  - [Bear](https://mermaid.live/edit#pako:eNo9TktvwjAM_iuRzwWl7zTH0SvapEmbNOViUa-11iYoTdkY4r8vwMAnfy_7O8HOdQQa9kxiHtx3iwGNFXECh5GEgVfuLY7inbgfwixW4onQiy36LwoGbl4Dz3a1GZCtAaFFXt7pljwfMPCB5quSPpQt7ry7ueWde3Nj9I4cjjfzQ3hxMwd2lm3_r0ACvecOdPALJTCRn_AC4XTJGAgDTWRAx7WLVS9FzzGzR_vh3HSPebf0A-hPHOeIln2HgVrG3uP0YD3ZjvzGLTaATqVS1yugT_ATcV6uZarSrEnLWtZVWSRwBK3SdV1kRV6prKhV3qhzAr_Xv3Jdl1LVlZRNURVZlTfnPyhpcvQ)
  - [Accumulation](https://mermaid.live/edit#pako:eNpdjk9PhDAQxb9KM2eWlP_Qm1muRhMTTQyXCcxCI7SklNWV8N0t6HKwp77fe29mFqh1QyBglMSmTn-WaLFSzD0rbU-sghfZKuzZG8m2sxM7sYe6noe5Ryu1quA3XMGTOp07lI4wwRJ-xyUZeXXRK027ExzOI9ZG7yw82KvexvbS3v6Fn_Ukt31StX8OeNAa2YCwZiYPBjIDbhKWrVOB7WigCoT7Nmg-tkNX1xlRvWs93GtGz20H4oL95NQ8NmiplNgaHA5qSDVkznpWFkTAi3CfAmKBL6ejxOdBHoRFkGQ8S5PYgxuIPPCzOIyjNA_jLI-KfPXge9_L_SzheZZyXsRpHKZRsf4AW29zzQ)

### Time Horizon Definitions

| Horizon         | Timeframe       | Primary Drivers                                           | Position Sizing   | Strategy                |
|--------------|--------------|---------------|---------------|--------------|
| **Short-term**  | Hours to days   | Funding rates, price momentum, volatility regime          | 25-50% of capital | Momentum/Mean Reversion |
| **Medium-term** | Weeks to months | MVRV valuation, macro composite, trend strength           | 50-75% of capital | Trend Following         |
| **Long-term**   | Months to years | Cycle positioning, halving schedule, strategic allocation | Core position     | Strategic Allocation    |

### Risk Scoring System

The framework continuously monitors three key risk factors:

1.  **Volatility Risk**: Realized volatility \> 1 standard deviation
    above mean
2.  **Macro Risk**: Composite risk score \< -0.3 (bearish macro
    environment)
3.  **Positioning Risk**: Crowded long trades (OI + funding both \>90th
    percentile)

Each factor contributes to an overall risk score that determines the
alert level:

| Level         | Score | Conditions                    | Max Recommended Leverage |
|---------------|-------|-------------------------------|--------------------------|
| 🟢 **GREEN**  | 0-1   | Normal conditions             | 3x                       |
| 🟡 **YELLOW** | 2     | Elevated risk in one category | 1.5x                     |
| 🔴 **RED**    | 3+    | Multiple risk factors         | 0x (cash)                |

------------------------------------------------------------------------

## Function Reference

### Core Analysis Functions

| Function                        | Description                 | Returns                    |
|-----------------------|-----------------------------|---------------------|
| `crypto_predictive_framework()` | Main analysis pipeline      | Complete framework object  |
| `print_crypto_summary()`        | Formatted executive summary | Console output (invisible) |

### Data Acquisition Modules

| Function            | Data Source                | Indicators                               |
|---------------------|---------------------------|-------------------------|
| `get_market_data()` | CoinMarketCap              | OHLCV, market cap                        |
| `get_derivatives()` | Binance Futures            | Open interest, funding rates, long/short |
| `get_macro()`       | FRED, Yahoo Finance        | M2, yields, DXY, inflation               |
| `get_positioning()` | Whale Alert, Google Trends | Whale flows, retail sentiment            |

### Indicator Calculations

| Function                   | Purpose                     | Dependencies          |
|----------------------|--------------------|------------------------------|
| `get_btc_issuance()`       | Daily Bitcoin supply        | Halving schedule      |
| `get_onchain_indicators()` | MVRV, NVT, Puell            | Market data, issuance |
| `get_volatility()`         | Realized volatility regimes | Price data            |

------------------------------------------------------------------------

## Academic & Industry References

The methodology draws from established research and industry best practices:

1. **On-Chain Analysis**
   - No peer-reviewed references with DOIs available for these industry metrics.
   - *Note: MVRV, NVT, and Puell Multiple were developed by industry practitioners and are widely cited in crypto analysis, but currently lack formal academic publications with DOIs.*

2. **Derivatives Analysis**
   - Hou, A., Corbet, S., Hu, Y., & Oxley, L. (2022). *Beyond the Noise – Information Discovery in Bitcoin Revisited*. SSRN Working Paper. DOI: [10.2139/ssrn.4145789](https://doi.org/10.2139/ssrn.4145789)

3. **Macroeconomic Factors**
   - Conlon, T., Corbet, S., & McGee, R. (2024). *Enduring relief or fleeting respite? Bitcoin as a hedge and safe haven for the US dollar*. Annals of Operations Research, 337(1), 45-73. DOI: [10.1007/s10479-024-05884-y](https://doi.org/10.1007/s10479-024-05884-y)
   - Kristoufek, L. (2015). *What Are the Main Drivers of the Bitcoin Price? Evidence from Wavelet Coherence Analysis*. PLOS ONE, 10(4), e0123923. DOI: [10.1371/journal.pone.0123923](https://doi.org/10.1371/journal.pone.0123923)

4. **Sentiment Indicators**
   - Da, Z., Engelberg, J., & Gao, P. (2011). *In Search of Attention*. Journal of Finance, 66(5), 1461-1499. DOI: [10.1111/j.1540-6261.2011.01679.x](https://doi.org/10.1111/j.1540-6261.2011.01679.x)
   - Preis, T., Moat, H. S., & Stanley, H. E. (2013). *Quantifying Trading Behavior in Financial Markets Using Google Trends*. Scientific Reports, 3, 1684. DOI: [10.1038/srep01684](https://doi.org/10.1038/srep01684)

------------------------------------------------------------------------

## Limitations & Risk Considerations

### Methodological Limitations

-   **On-chain proxies**: Realized price is approximated using 200-day
    SMA (true realized price requires UTXO-level data)
-   **Bitcoin-centric**: Puell Multiple and issuance metrics are
    specific to Bitcoin
-   **Regime dependence**: Thresholds may require calibration across
    different market regimes
-   **Look-ahead bias**: Some calculations use rolling windows not
    available in real-time

### Operational Risks

-   **API reliability**: External data sources may experience downtime
    or rate limiting
-   **Data latency**: Some indicators (e.g., Google Trends) have 1-2 day
    delay
-   **Exchange concentration**: Derivatives data sourced solely from
    Binance
-   **Backtesting limitations**: Historical performance does not
    guarantee future results

### Recommended Usage

✅ **DO**: - Use for directional bias and risk management - Combine with
your own analysis and risk tolerance - Scale position sizes based on
conviction levels - Monitor risk-off signals for capital preservation

❌ **DON'T**: - Use as a sole trading system without validation - Ignore
risk-off signals during extreme conditions - Apply Bitcoin-specific
indicators to other assets - Over-leverage based on signal strength
alone

------------------------------------------------------------------------

## Contributing

Contributions are welcome! Please follow these guidelines:

1.  **Open an issue** to discuss proposed changes
2.  **Fork the repository** and create a feature branch
3.  **Write tests** for new functionality
4.  **Update documentation** (roxygen2 format)
5.  **Submit a pull request** with a clear description

### Development Workflow

``` bash
# Clone your fork
git clone https://github.com/filippoweb3/crypto-indicator.git
cd crypto-indicator

# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Load the package
devtools::load_all()

# Run tests
devtools::test()

# Update documentation
devtools::document()

# Check package
devtools::check()
```

------------------------------------------------------------------------

## License

This package is licensed under the [MIT License](LICENSE). You are free
to use, modify, and distribute it for commercial and non-commercial
purposes with attribution.

------------------------------------------------------------------------

## Citation

If you use this package in academic work, please cite:

``` bibtex
@software{CryptoIndicator,
  author = {Franchini, Filippo},
  title = {CryptoIndicator: A Comprehensive Predictive Framework for Digital Assets},
  year = {2024},
  url = {https://github.com/filippoweb3/crypto-indicator}
}
```

------------------------------------------------------------------------

## Contact & Support

-   **Maintainer**: [Filippo
    Franchini](https://www.linkedin.com/in/filippoweb3/)
-   **Issues**: [GitHub
    Issues](https://github.com/filippoweb3/crypto-indicator/issues)
-   **Discussions**: [GitHub
    Discussions](https://github.com/filippoweb3/crypto-indicator/discussions)

------------------------------------------------------------------------

**Disclaimer**: This software is for informational and educational
purposes only. It is not financial advice. Cryptocurrency trading
carries substantial risk. Always conduct your own research and consult
with a qualified financial advisor before making investment decisions.
