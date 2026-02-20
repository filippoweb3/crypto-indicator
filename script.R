

# Packages ----

library(crypto2)
library(TTR)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cryptoQuotes)
library(fredr)
library(zoo)
library(scales)

# Get Market Data ----

data <- get_market_data()

# Compute Indicators ----

ind_data <- get_onchain_indicators(data$data)

## Charts ----

p1 <- ggplot(data = data$data, aes(x = time_close)) +
  geom_point(aes(y = close)) +
  geom_line(aes(y = price_ma_50, color = "MA 50")) +
  geom_line(aes(y = price_ma_200, color = "MA 200")) +
  theme_minimal() +
  labs(title = "Bitcoin", subtitle = "Prices, Volumes and Returns since 2020",
       x="Time", y="Price (USD)") +
  theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)),
       axis.text.x = element_text(angle=45, vjust=0.5), legend.title = element_text(size=12, face="bold")) +
  scale_color_manual(name = "Indicators",
                     values = c("MA 50" = "blue", "MA 200" = "red"))

ggplot(data = ind_data, aes(x = time_close)) +
  geom_point(aes(y = close, colour = regime_combi)) +
  theme_minimal() +
  labs(title = "Bitcoin", subtitle = "Prices, Volumes and Returns since 2020",
       x="Time", y="Price (USD)") +
  geom_line(aes(y = price_ma_50, color = "MA 50")) +
  geom_line(aes(y = price_ma_200, color = "MA 200")) +
  scale_color_manual(name = "Indicators",
                     values = c("MA 50" = "blue",
                                "MA 200" = "red",
                                "Neutral" = "orange",
                                "Strong Buy" = "blue",
                                "Strong Sell" = "red"), na.translate = FALSE) +
  labs(title = "Bitcoin", subtitle = "Prices, Volumes and Returns since 2020",
       x="Time", y="Price (USD)") +
  theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)),
        axis.text.x = element_text(angle=45, vjust=0.5), legend.title = element_text(size=12, face="bold"))


p2 <- ggplot(data = data$data, aes(x = time_close)) +
  geom_line(aes(y = volume_ma_7)) +
  theme_minimal() + labs(title = "", subtitle = "",
                         x="Time", y="Volume MA-7 (Billion USD)") +
  scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9))

p3 <- ggplot(data = data$data, aes(x = time_close)) +
  geom_line(aes(y = returns)) +
  theme_minimal() + labs(title = "", subtitle = "",
                         x="Time", y="Return", caption = "Source: crypto2 R-Package")

p1 / p2 / p3


deriv <- get_derivatives()

macro <- get_macro()

vol <- get_volatility(data = ind_data)

pos <- get_positioning(api_key = global_variables$whaleAlert_api_key)








