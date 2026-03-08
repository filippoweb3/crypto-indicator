#' Morpho Vault Data Acquisition (Official API)
#'
#' @description
#' A comprehensive set of functions to fetch and analyze Morpho Vault data using the official
#' Morpho GraphQL API. Supports both Vault V1 (MetaMorpho) and Vault V2.
#'
#' The package implements the exact queries documented in the official Morpho API documentation
#' at \url{https://docs.morpho.org/tools/offchain/api/morpho-vaults}.
#'
#' @section API Endpoint:
#' All functions use the GraphQL endpoint: \code{https://api.morpho.org/graphql}
#'
#' @section Authentication:
#' No authentication is required for public data access. Rate limits may apply for intensive usage.
#'
#' @section Chain IDs:
#' Common chain identifiers:
#' \itemize{
#'   \item{1}{: Ethereum Mainnet}
#'   \item{8453}{: Base}
#'   \item{137}{: Polygon}
#'   \item{42161}{: Arbitrum}
#'   \item{10}{: Optimism}
#' }
#'
#' @import httr jsonlite dplyr purrr
#' @name morpho_v1
#' @aliases morpho vaults
NULL

# Base API endpoint
MORPHO_API <- "https://api.morpho.org/graphql"

# Chain ID constants
#' @export
MORPHO_CHAINS <- list(
  ETHEREUM = 1,
  BASE = 8453,
  POLYGON = 137,
  ARBITRUM = 42161,
  OPTIMISM = 10,
  AVALANCHE = 43114,
  GNOSIS = 100
)

#' @export
CHAIN_NAMES <- c(
  "1" = "ethereum",
  "8453" = "base",
  "137" = "polygon",
  "42161" = "arbitrum",
  "10" = "optimism",
  "43114" = "avalanche",
  "100" = "gnosis"
)

# Asset type constants
STABLECOINS <- c("USDC", "USDT", "DAI", "PYUSD", "USDbC", "USDe", "sUSDe")
ETH_TOKENS <- c("WETH", "wstETH", "cbETH", "rETH", "weETH", "ETH", "stETH")
BTC_TOKENS <- c("WBTC", "cbBTC", "BTC", "wBTC")

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Helper function to validate Ethereum address
.is_valid_address <- function(address) {
  is.character(address) && nchar(address) == 42 && grepl("^0x", address)
}

# Helper function to validate chain IDs
.validate_chains <- function(chain_ids) {
  valid_chains <- c(1, 8453, 137, 42161, 10, 43114, 100)
  invalid <- setdiff(chain_ids, valid_chains)
  if (length(invalid) > 0) {
    warning("Invalid chain IDs: ", paste(invalid, collapse = ", "),
            "\nValid chains: ", paste(valid_chains, collapse = ", "))
  }
  intersect(chain_ids, valid_chains)
}

# Helper function to execute GraphQL queries with retry logic
.execute_query <- function(query, variables = list(), max_retries = 3) {

  for (attempt in 1:max_retries) {
    response <- tryCatch({
      POST(
        MORPHO_API,
        body = list(query = query, variables = variables),
        encode = "json",
        add_headers("Content-Type" = "application/json")
      )
    }, error = function(e) NULL)

    if (!is.null(response)) {
      if (status_code(response) == 200) {
        return(content(response, "parsed"))
      } else if (status_code(response) == 429) {
        # Rate limited - exponential backoff
        wait_time <- 2 ^ attempt
        message("Rate limited. Waiting ", wait_time, " seconds...")
        Sys.sleep(wait_time)
      } else {
        stop("API request failed with status ", status_code(response),
             "\nResponse: ", content(response, "text"))
      }
    } else {
      # Connection error - exponential backoff
      wait_time <- 2 ^ attempt
      message("Connection error. Retrying in ", wait_time, " seconds...")
      Sys.sleep(wait_time)
    }
  }

  stop("Failed to execute query after ", max_retries, " attempts")
}

# ============================================================================
# VAULT V1 FUNCTIONS
# ============================================================================

#' Get a paginated list of all Vault V1s (MetaMorpho)
#'
#' @description
#' Retrieves a paginated list of all Morpho Vault V1s (MetaMorpho) with basic information.
#' These vaults typically have the highest TVL (billions of dollars).
#'
#' @param first Integer. Number of items to return per page (default: 100)
#' @param skip Integer. Number of items to skip for pagination (default: 0)
#' @param chain_ids Integer vector. Filter vaults by chain IDs (default: c(1, 8453))
#' @param order_by Character. Field to order by (default: "TotalAssetsUsd")
#' @param order_direction Character. "Asc" or "Desc" (default: "Desc")
#' @param human_readable Logical. Add formatted columns (default: TRUE)
#'
#' @return A tibble with basic vault information including TVL and correct share prices
#' @export
get_vault_v1_list <- function(first = 100, skip = 0, chain_ids = c(1, 8453),
                              order_by = "TotalAssetsUsd", order_direction = "Desc",
                              human_readable = TRUE) {

  chain_ids <- .validate_chains(chain_ids)
  if (length(chain_ids) == 0) {
    stop("No valid chain IDs provided")
  }

  # Updated query to include liquidity
  query <- '
  query GetVaultV1List($first: Int, $skip: Int, $chainIds: [Int!], $orderBy: VaultOrderBy, $orderDirection: OrderDirection) {
    vaults(first: $first, skip: $skip, where: { chainId_in: $chainIds }, orderBy: $orderBy, orderDirection: $orderDirection) {
      items {
        address
        symbol
        name
        listed
        asset {
          id
          address
          decimals
          name
          symbol
        }
        chain {
          id
          network
        }
        state {
          totalAssets
          totalAssetsUsd
          totalSupply
        }
        liquidity {
          underlying
          usd
        }
        metadata {
          description
          image
        }
      }
      pageInfo {
        countTotal
        count
        skip
        limit
      }
    }
  }
  '

  variables <- list(
    first = first,
    skip = skip,
    chainIds = chain_ids,
    orderBy = order_by,
    orderDirection = order_direction
  )

  result <- .execute_query(query, variables)

  if (is.null(result$data$vaults$items)) {
    return(tibble())
  }

  items <- result$data$vaults$items

  # Create base tibble with liquidity
  result_tibble <- map_dfr(items, function(v) {
    tibble(
      vault_address = v$address,
      symbol = v$symbol %||% NA,
      name = v$name %||% NA,
      listed = v$listed %||% FALSE,
      asset_address = v$asset$address %||% NA,
      asset_id = v$asset$id %||% NA,
      asset_symbol = v$asset$symbol %||% NA,
      asset_name = v$asset$name %||% NA,
      asset_decimals = as.integer(v$asset$decimals %||% 18),
      chain_id = v$chain$id %||% NA,
      chain_network = v$chain$network %||% NA,
      total_assets = as.numeric(v$state$totalAssets %||% 0),
      total_assets_usd = as.numeric(v$state$totalAssetsUsd %||% 0),
      total_supply = as.numeric(v$state$totalSupply %||% 0),
      liquidity_underlying = as.numeric(v$liquidity$underlying %||% 0),
      liquidity_usd = as.numeric(v$liquidity$usd %||% 0),
      description = v$metadata$description %||% NA,
      image = v$metadata$image %||% NA,
      version = "V1",
      fetched_at = Sys.time()
    )
  })

  # Add calculated fields (always needed for correct share prices)
  result_tibble <- result_tibble %>%
    mutate(
      # Convert raw amounts to human-readable based on asset type
      total_assets_human = case_when(
        asset_symbol %in% STABLECOINS ~ total_assets / 1e6,
        asset_symbol %in% ETH_TOKENS ~ total_assets / 1e18,
        asset_symbol %in% BTC_TOKENS ~ total_assets / 1e8,
        TRUE ~ total_assets / 1e18  # Default to 18 decimals
      ),

      # Shares ALWAYS have 18 decimals in Morpho
      total_supply_human = total_supply / 1e18,

      # CORRECT share price calculations
      share_price = total_assets_human / total_supply_human,
      share_price_usd = total_assets_usd / total_supply_human,

      # Liquidity ratio
      liquidity_ratio = ifelse(total_assets_usd > 0, liquidity_usd / total_assets_usd, 0),

      # Asset type flags for filtering
      is_stablecoin = asset_symbol %in% STABLECOINS,
      is_eth = asset_symbol %in% ETH_TOKENS,
      is_btc = asset_symbol %in% BTC_TOKENS
    )

  # Add human-readable formatted columns if requested
  if (human_readable) {
    result_tibble <- result_tibble %>%
      mutate(
        # Format total assets
        total_assets_formatted = case_when(
          is_stablecoin ~ paste0("$", format(round(total_assets_human, 0), big.mark = ",")),
          is_eth ~ paste0(format(round(total_assets_human, 2), big.mark = ","), " ETH"),
          is_btc ~ paste0(format(round(total_assets_human, 4), big.mark = ","), " BTC"),
          TRUE ~ format(total_assets_human, scientific = FALSE)
        ),

        # TVL formatting
        tvl_millions = total_assets_usd / 1e6,
        tvl_formatted = paste0("$", format(round(tvl_millions, 1), big.mark = ","), "M"),
        tvl_billions = round(total_assets_usd / 1e9, 2),

        # Share price formatting
        share_price_formatted = case_when(
          is_stablecoin ~ paste0("$", round(share_price_usd, 4)),
          is_eth ~ paste0(round(share_price, 4), " ETH (",
                          round(share_price_usd, 2), " USD)"),
          is_btc ~ paste0(round(share_price, 6), " BTC (",
                          round(share_price_usd, 2), " USD)"),
          TRUE ~ paste0("$", round(share_price_usd, 4))
        ),

        # Total supply formatting
        total_supply_formatted = paste0(
          format(round(total_supply_human, 0), big.mark = ","),
          " shares"
        ),

        # Liquidity ratio formatting
        liquidity_ratio_formatted = paste0(round(liquidity_ratio * 100, 1), "%"),

        # Liquidity formatting
        liquidity_formatted = paste0("$", format(round(liquidity_usd / 1e6, 1), big.mark = ","), "M"),

        # Description snippet
        description_short = substr(description, 1, 100),

        # Add chain prefix for duplicate names
        display_name = case_when(
          chain_id == 8453 ~ paste0(name, " (Base)"),
          chain_id == 1 ~ paste0(name, " (Eth)"),
          chain_id == 137 ~ paste0(name, " (Polygon)"),
          TRUE ~ name
        ),

        # Curator flag (based on name)
        curator = case_when(
          grepl("Gauntlet", name, ignore.case = TRUE) ~ "Gauntlet",
          grepl("Steakhouse", name, ignore.case = TRUE) ~ "Steakhouse",
          grepl("Spark", name, ignore.case = TRUE) ~ "Spark",
          grepl("Sentora", name, ignore.case = TRUE) ~ "Sentora",
          grepl("Reservoir", name, ignore.case = TRUE) ~ "Reservoir",
          TRUE ~ "Other"
        )
      )
  }

  # Add attributes for reference
  attr(result_tibble, "chains_queried") <- chain_ids
  attr(result_tibble, "order_by") <- order_by
  attr(result_tibble, "order_direction") <- order_direction
  attr(result_tibble, "human_readable") <- human_readable
  attr(result_tibble, "pagination") <- list(
    count_total = result$data$vaults$pageInfo$countTotal %||% NA,
    count = result$data$vaults$pageInfo$count %||% NA,
    skip = result$data$vaults$pageInfo$skip %||% NA,
    limit = result$data$vaults$pageInfo$limit %||% NA
  )

  return(result_tibble)
}

#' Get pagination info from any vault list
#'
#' @param vault_list Result from get_vault_v1_list or get_vault_v2_list
#' @return List with pagination information
#' @export
get_pagination_info <- function(vault_list) {
  attr(vault_list, "pagination")
}

#' Get comprehensive metrics for a specific Vault V1 with automatic chain detection
#'
#' @param vault_address Character. Ethereum address of the vault
#' @param chain_id Integer. Optional specific chain ID. If NULL, scans all chains.
#' @param verbose Logical. Print scanning progress (default: TRUE)
#' @param human_readable Logical. Include formatted fields (default: TRUE)
#'
#' @return A list with vault metrics, including correct share prices
#' @export
get_vault_v1_by_address <- function(vault_address, chain_id = NULL, verbose = TRUE, human_readable = TRUE) {

  if (!.is_valid_address(vault_address)) {
    stop("Invalid Ethereum address format: ", vault_address)
  }

  # If chain_id is provided, just query that chain
  if (!is.null(chain_id)) {
    if (verbose) cat("🔍 Checking chain", chain_id, "...\n")

    result <- tryCatch({
      .fetch_vault_v1_by_address(vault_address, chain_id)
    }, error = function(e) NULL)

    if (!is.null(result)) {
      if (verbose) cat("✅ Found on chain", chain_id, "\n")
      # Enhance with calculated fields
      result <- .enhance_vault_data(result)
      if (human_readable) {
        result <- .add_human_readable_fields(result)
      }
      return(result)
    } else {
      stop("Vault V1 not found for address: ", vault_address, " on chain: ", chain_id)
    }
  }

  # No chain_id provided - scan all chains
  if (verbose) cat("🔍 Scanning all chains for vault:", vault_address, "\n")

  # Get all chain IDs from CHAIN_NAMES
  all_chains <- as.numeric(names(CHAIN_NAMES))

  for (i in seq_along(all_chains)) {
    cid <- all_chains[i]
    chain_name <- CHAIN_NAMES[as.character(cid)]

    if (verbose) cat("  ", i, "/", length(all_chains), ": Checking", chain_name, "...", sep="")

    result <- tryCatch({
      .fetch_vault_v1_by_address(vault_address, cid)
    }, error = function(e) NULL)

    if (!is.null(result)) {
      if (verbose) cat(" ✅ FOUND!\n")
      result$chain_id <- cid
      result$chain_name <- chain_name
      # Enhance with calculated fields
      result <- .enhance_vault_data(result)
      if (human_readable) {
        result <- .add_human_readable_fields(result)
      }
      return(result)
    }

    if (verbose) cat(" ❌\n")
  }

  stop("Vault V1 not found for address: ", vault_address, " on any chain")
}

#' Internal function to enhance vault data with calculated fields
#'
#' @param vault List. Raw vault data from .fetch_vault_v1_by_address
#' @return Enhanced vault list with share prices and flags
#' @keywords internal
.enhance_vault_data <- function(vault) {

  # Convert raw amounts to human-readable based on asset type
  vault$total_assets_human <- case_when(
    vault$asset_symbol %in% STABLECOINS ~ vault$total_assets / 1e6,
    vault$asset_symbol %in% ETH_TOKENS ~ vault$total_assets / 1e18,
    vault$asset_symbol %in% BTC_TOKENS ~ vault$total_assets / 1e8,
    TRUE ~ vault$total_assets / 1e18  # Default to 18 decimals
  )

  # Shares ALWAYS have 18 decimals in Morpho
  vault$total_supply_human <- vault$total_supply / 1e18

  # CORRECT share price calculations
  vault$share_price <- vault$total_assets_human / vault$total_supply_human
  vault$share_price_usd <- vault$total_assets_usd / vault$total_supply_human

  # Asset type flags
  vault$is_stablecoin <- vault$asset_symbol %in% STABLECOINS
  vault$is_eth <- vault$asset_symbol %in% ETH_TOKENS
  vault$is_btc <- vault$asset_symbol %in% BTC_TOKENS

  return(vault)
}

#' Internal function to add human-readable formatted fields
#'
#' @param vault List. Enhanced vault data from .enhance_vault_data
#' @return Vault list with additional formatted fields
#' @keywords internal
.add_human_readable_fields <- function(vault) {

  # Format total assets
  vault$total_assets_formatted <- case_when(
    vault$is_stablecoin ~ paste0("$", format(round(vault$total_assets_human, 0), big.mark = ",")),
    vault$is_eth ~ paste0(format(round(vault$total_assets_human, 2), big.mark = ","), " ETH"),
    vault$is_btc ~ paste0(format(round(vault$total_assets_human, 4), big.mark = ","), " BTC"),
    TRUE ~ format(vault$total_assets_human, scientific = FALSE)
  )

  # TVL formatting
  vault$tvl_millions <- vault$total_assets_usd / 1e6
  vault$tvl_formatted <- paste0("$", format(round(vault$tvl_millions, 1), big.mark = ","), "M")
  vault$tvl_billions <- round(vault$total_assets_usd / 1e9, 2)

  # Share price formatting
  vault$share_price_formatted <- case_when(
    vault$is_stablecoin ~ paste0("$", round(vault$share_price_usd, 4)),
    vault$is_eth ~ paste0(round(vault$share_price, 4), " ETH (",
                          round(vault$share_price_usd, 2), " USD)"),
    vault$is_btc ~ paste0(round(vault$share_price, 6), " BTC (",
                          round(vault$share_price_usd, 2), " USD)"),
    TRUE ~ paste0("$", round(vault$share_price_usd, 4))
  )

  # Total supply formatting
  vault$total_supply_formatted <- paste0(
    format(round(vault$total_supply_human, 0), big.mark = ","),
    " shares"
  )

  # Liquidity ratio
  vault$liquidity_ratio <- if (vault$total_assets_usd > 0) {
    vault$liquidity_usd / vault$total_assets_usd
  } else {
    0
  }
  vault$liquidity_ratio_formatted <- paste0(round(vault$liquidity_ratio * 100, 1), "%")

  return(vault)
}

#' Internal function to fetch vault data for a specific chain
#'
#' @param vault_address Character. Ethereum address of the vault
#' @param chain_id Integer. Chain ID
#' @return A list with vault metrics or NULL if not found
#' @keywords internal
.fetch_vault_v1_by_address <- function(vault_address, chain_id) {

  # Enhanced query to include asset info
  query <- '
  query GetVaultV1ByAddress($address: String!, $chainId: Int!) {
    vaultByAddress(address: $address, chainId: $chainId) {
      address
      name
      symbol
      asset {
        symbol
        address
        decimals
      }
      state {
        totalAssets
        totalAssetsUsd
        totalSupply
      }
      liquidity {
        underlying
        usd
      }
      chain {
        id
        network
      }
    }
  }
  '

  variables <- list(
    address = vault_address,
    chainId = chain_id
  )

  # Use tryCatch to handle API errors gracefully
  result <- tryCatch({
    .execute_query(query, variables)
  }, error = function(e) {
    return(NULL)
  })

  # Check if vault exists
  if (is.null(result) || is.null(result$data$vaultByAddress)) {
    return(NULL)
  }

  vault <- result$data$vaultByAddress

  # Return structured data
  list(
    address = vault$address,
    name = vault$name %||% NA,
    symbol = vault$symbol %||% NA,
    asset_symbol = vault$asset$symbol %||% "UNKNOWN",
    asset_address = vault$asset$address %||% NA,
    asset_decimals = as.integer(vault$asset$decimals %||% 18),
    chain_id = vault$chain$id,
    chain_name = vault$chain$network,
    total_assets = as.numeric(vault$state$totalAssets %||% 0),
    total_assets_usd = as.numeric(vault$state$totalAssetsUsd %||% 0),
    total_supply = as.numeric(vault$state$totalSupply %||% 0),
    liquidity_underlying = as.numeric(vault$liquidity$underlying %||% 0),
    liquidity_usd = as.numeric(vault$liquidity$usd %||% 0),
    fetched_at = Sys.time()
  )
}

#' Get metrics for multiple Vault V1s with automatic chain detection
#'
#' @param vault_addresses Character vector of vault addresses
#' @param verbose Logical. Print scanning progress (default: TRUE)
#' @param max_retries Integer. Maximum retries per vault (default: 1)
#' @param human_readable Logical. Include formatted fields (default: TRUE)
#'
#' @return A tibble with vault metrics for all found vaults, including share prices
#' @export
get_vaults_v1 <- function(vault_addresses, verbose = TRUE, max_retries = 1,
                          human_readable = TRUE) {

  results <- map_dfr(seq_along(vault_addresses), function(i) {
    addr <- vault_addresses[i]

    if (!.is_valid_address(addr)) {
      if (verbose) cat("  ❌ Invalid address format:", addr, "\n")
      return(tibble(vault_address = addr, found = FALSE))
    }

    if (verbose) cat("\n[", i, "/", length(vault_addresses), "] Processing: ", addr, "\n", sep="")

    # Try with retries
    for (attempt in 1:max_retries) {
      vault <- tryCatch({
        get_vault_v1_by_address(addr, chain_id = NULL, verbose = verbose,
                                human_readable = human_readable)
      }, error = function(e) {
        if (verbose) cat("  ❌ Error:", e$message, "\n")
        return(NULL)
      })

      if (!is.null(vault)) break

      if (attempt < max_retries) {
        if (verbose) cat("  Retrying (", attempt, "/", max_retries, ")...\n", sep="")
        Sys.sleep(1)
      }
    }

    if (is.null(vault)) {
      return(tibble(
        vault_address = addr,
        found = FALSE
      ))
    }

    # Build result tibble with all available fields
    result <- tibble(
      vault_address = vault$address,
      name = vault$name %||% NA,
      symbol = vault$symbol %||% NA,
      found = TRUE,
      chain_id = vault$chain_id,
      chain_name = vault$chain_name %||% CHAIN_NAMES[as.character(vault$chain_id)] %||% paste("Chain", vault$chain_id),
      asset_symbol = vault$asset_symbol %||% NA,
      total_assets = vault$total_assets,
      total_assets_usd = vault$total_assets_usd,
      total_supply = vault$total_supply,
      liquidity_usd = vault$liquidity_usd,
      fetched_at = vault$fetched_at
    )

    # Add calculated fields if available
    if (!is.null(vault$total_assets_human)) {
      result <- result %>%
        mutate(
          total_assets_human = vault$total_assets_human,
          total_supply_human = vault$total_supply_human,
          share_price = vault$share_price,
          share_price_usd = vault$share_price_usd,
          tvl_billions = vault$total_assets_usd / 1e9,
          tvl_formatted = paste0("$", format(round(vault$total_assets_usd / 1e6, 1), big.mark = ","), "M"),
          is_stablecoin = vault$is_stablecoin %||% FALSE,
          is_eth = vault$is_eth %||% FALSE,
          is_btc = vault$is_btc %||% FALSE
        )
    }

    # Add human-readable fields if available
    if (!is.null(vault$share_price_formatted)) {
      result <- result %>%
        mutate(
          total_assets_formatted = vault$total_assets_formatted,
          share_price_formatted = vault$share_price_formatted,
          total_supply_formatted = vault$total_supply_formatted,
          liquidity_ratio = vault$liquidity_ratio,
          liquidity_ratio_formatted = vault$liquidity_ratio_formatted
        )
    }

    return(result)
  })

  # Print summary
  found_count <- sum(results$found)
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("📊 Found", found_count, "of", length(vault_addresses), "vaults\n")
    if (found_count > 0 && "tvl_formatted" %in% names(results)) {
      cat("\nTop found vaults:\n")
      results %>%
        filter(found) %>%
        arrange(desc(total_assets_usd)) %>%
        head(5) %>%
        mutate(display = paste0("  • ", name, " (", chain_name, "): ", tvl_formatted)) %>%
        pull(display) %>%
        cat(sep = "\n")
    }
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  invisible(results)
}

#' Get APY data for all Vault V1s
#'
#' @description
#' Retrieves comprehensive yield-related metrics for multiple Vault V1s simultaneously,
#' including daily, weekly, monthly, and average APYs.
#'
#' @param first Integer. Number of vaults to return (default: 100)
#' @param chain_ids Integer vector. Filter vaults by chain IDs (default: c(1, 8453))
#' @param human_readable Logical. Format percentages (default: TRUE)
#'
#' @return A tibble with comprehensive APY metrics
#' @export
get_vault_v1_apys <- function(first = 100, chain_ids = c(1, 8453), human_readable = TRUE) {

  query <- '
  query GetVaultV1APYs($first: Int, $chainIds: [Int!]) {
    vaults(first: $first, where: { chainId_in: $chainIds }, orderBy: TotalAssetsUsd, orderDirection: Desc) {
      items {
        address
        symbol
        name
        asset {
          symbol
          decimals
          yield { apr }
        }
        state {
          apy
          netApy
          netApyWithoutRewards
          avgApy
          avgNetApy
          dailyApy
          dailyNetApy
          weeklyApy
          weeklyNetApy
          monthlyApy
          monthlyNetApy
          rewards {
            asset {
              address
              symbol
              chain { id }
            }
            supplyApr
            yearlySupplyTokens
          }
        }
      }
    }
  }
  '

  result <- .execute_query(query, list(first = first, chainIds = chain_ids))

  if (is.null(result$data$vaults$items)) {
    return(tibble())
  }

  items <- result$data$vaults$items

  result_tibble <- map_dfr(items, function(v) {
    tibble(
      vault_address = v$address,
      symbol = v$symbol %||% NA,
      name = v$name %||% NA,
      asset_symbol = v$asset$symbol %||% NA,
      asset_decimals = as.integer(v$asset$decimals %||% 18),
      base_asset_apr = as.numeric(v$asset$yield$apr %||% 0),
      apy = as.numeric(v$state$apy %||% 0),
      net_apy = as.numeric(v$state$netApy %||% 0),
      net_apy_without_rewards = as.numeric(v$state$netApyWithoutRewards %||% 0),
      avg_apy = as.numeric(v$state$avgApy %||% 0),
      avg_net_apy = as.numeric(v$state$avgNetApy %||% 0),
      daily_apy = as.numeric(v$state$dailyApy %||% 0),
      daily_net_apy = as.numeric(v$state$dailyNetApy %||% 0),
      weekly_apy = as.numeric(v$state$weeklyApy %||% 0),
      weekly_net_apy = as.numeric(v$state$weeklyNetApy %||% 0),
      monthly_apy = as.numeric(v$state$monthlyApy %||% 0),
      monthly_net_apy = as.numeric(v$state$monthlyNetApy %||% 0),
      reward_count = length(v$state$rewards %||% list()),
      fetched_at = Sys.time()
    )
  })

  if (human_readable) {
    result_tibble <- result_tibble %>%
      mutate(
        apy_formatted = paste0(round(apy * 100, 2), "%"),
        net_apy_formatted = paste0(round(net_apy * 100, 2), "%"),
        avg_net_apy_formatted = paste0(round(avg_net_apy * 100, 2), "%"),
        daily_net_apy_formatted = paste0(round(daily_net_apy * 100, 2), "%")
      )
  }

  return(result_tibble)
}

#' Get allocation data for all Vault V1s
#'
#' @description
#' Retrieves the current allocation of assets across markets for each Vault V1.
#' Each row represents one market allocation within a vault.
#'
#' @param first Integer. Number of vaults to return (default: 100)
#' @param chain_ids Integer vector. Filter by chain IDs (default: c(1, 8453))
#' @param human_readable Logical. Add formatted columns (default: FALSE)
#'
#' @return A tibble with vault allocations to markets
#' @export
get_vault_v1_allocations <- function(first = 100, chain_ids = c(1, 8453), human_readable = FALSE) {

  query <- '
  query GetVaultV1Allocations($first: Int, $chainIds: [Int!]) {
    vaults(first: $first, where: { chainId_in: $chainIds }, orderBy: TotalAssetsUsd, orderDirection: Desc) {
      items {
        address
        name
        symbol
        state {
          totalAssetsUsd
          allocation {
            market {
              uniqueKey
              loanAsset { symbol }
              collateralAsset { symbol }
              lltv
            }
            supplyAssets
            supplyAssetsUsd
            supplyQueueIndex
            withdrawQueueIndex
          }
        }
      }
    }
  }
  '

  result <- .execute_query(query, list(first = first, chainIds = chain_ids))

  if (is.null(result$data$vaults$items)) {
    return(tibble())
  }

  items <- result$data$vaults$items

  # Expand to one row per market allocation
  result_tibble <- map_dfr(items, function(v) {
    vault_address <- v$address
    vault_name <- v$name %||% NA
    vault_symbol <- v$symbol %||% NA
    total_usd <- as.numeric(v$state$totalAssetsUsd %||% 0)

    allocations <- v$state$allocation %||% list()

    if (length(allocations) == 0) {
      return(tibble())
    }

    map_dfr(allocations, function(a) {
      m <- a$market
      supply_usd <- as.numeric(a$supplyAssetsUsd %||% 0)
      allocation_pct <- if (total_usd > 0) (supply_usd / total_usd) * 100 else 0

      tibble(
        vault_address = vault_address,
        vault_name = vault_name,
        vault_symbol = vault_symbol,
        total_assets_usd = total_usd,
        market_key = m$uniqueKey %||% NA,
        loan_asset = m$loanAsset$symbol %||% NA,
        collateral_asset = m$collateralAsset$symbol %||% NA,
        lltv = as.numeric(m$lltv %||% 0) / 1e18,
        supply_assets = as.numeric(a$supplyAssets %||% 0),
        supply_assets_usd = supply_usd,
        allocation_pct = allocation_pct,
        supply_queue_index = as.integer(a$supplyQueueIndex %||% NA),
        withdraw_queue_index = as.integer(a$withdrawQueueIndex %||% NA)
      )
    })
  })

  if (human_readable) {
    result_tibble <- result_tibble %>%
      mutate(
        allocation_pct_formatted = paste0(round(allocation_pct, 1), "%"),
        lltv_formatted = paste0(round(lltv * 100, 1), "%"),
        exposure_formatted = paste0("$", format(round(supply_assets_usd / 1e6, 1), big.mark = ","), "M")
      )
  }

  return(result_tibble)
}

#' Get warnings for Vault V1s
#'
#' @description
#' Retrieves active warnings for Vault V1s, which indicate potential risk factors
#' or configuration issues.
#'
#' @return A tibble with vault warnings
#' @export
get_vault_v1_warnings <- function() {

  query <- '
  query GetVaultV1Warnings {
    vaults {
      items {
        address
        name
        symbol
        warnings {
          type
          level
        }
      }
    }
  }
  '

  result <- .execute_query(query, list())

  if (is.null(result$data$vaults$items)) {
    return(tibble())
  }

  items <- result$data$vaults$items

  # Expand warnings
  map_dfr(items, function(v) {
    vault_address <- v$address
    vault_name <- v$name %||% NA
    vault_symbol <- v$symbol %||% NA

    warnings <- v$warnings %||% list()

    if (length(warnings) == 0) {
      return(tibble(
        vault_address = vault_address,
        vault_name = vault_name,
        vault_symbol = vault_symbol,
        warning_type = NA_character_,
        warning_level = NA_character_
      ))
    }

    map_dfr(warnings, function(w) {
      tibble(
        vault_address = vault_address,
        vault_name = vault_name,
        vault_symbol = vault_symbol,
        warning_type = w$type %||% NA,
        warning_level = w$level %||% NA
      )
    })
  })
}

#' Get top depositors for a specific Vault V1 with automatic chain detection
#'
#' @param vault_address Character. Ethereum address of the vault
#' @param first Integer. Number of top depositors to return (default: 10)
#' @param chain_id Integer. Optional specific chain ID. If NULL, scans all chains.
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return A tibble with depositor information
#' @export
get_vault_v1_depositors <- function(vault_address, first = 10, chain_id = NULL, verbose = TRUE) {

  if (!.is_valid_address(vault_address)) {
    stop("Invalid Ethereum address format: ", vault_address)
  }

  # If chain_id is provided, use it directly
  if (!is.null(chain_id)) {
    if (verbose) cat("🔍 Checking chain", chain_id, "...\n")
    return(.fetch_vault_v1_depositors(vault_address, first, chain_id, verbose))
  }

  # Auto-scan all chains
  if (verbose) cat("🔍 Scanning all chains for depositors of:", vault_address, "\n")

  all_chains <- as.numeric(names(CHAIN_NAMES))

  for (cid in all_chains) {
    if (verbose) cat("  Checking chain", cid, "(", CHAIN_NAMES[as.character(cid)], ")...\n")

    result <- tryCatch({
      .fetch_vault_v1_depositors(vault_address, first, cid, verbose = FALSE)
    }, error = function(e) NULL)

    if (!is.null(result) && nrow(result) > 0) {
      if (verbose) cat("  ✅ Found on chain", cid, "\n")
      attr(result, "chain_id") <- cid
      attr(result, "chain_name") <- CHAIN_NAMES[as.character(cid)]
      return(result)
    }
  }

  if (verbose) cat("  ❌ No depositors found on any chain\n")
  tibble()
}

#' Internal function to fetch vault depositors for a specific chain
#'
#' @param vault_address Character. Ethereum address of the vault
#' @param first Integer. Number of depositors
#' @param chain_id Integer. Chain ID
#' @param verbose Logical. Print progress
#' @return A tibble with depositor information
#' @keywords internal
.fetch_vault_v1_depositors <- function(vault_address, first, chain_id, verbose) {

  # First, get vault info to know total supply and asset decimals
  vault_info <- tryCatch({
    .fetch_vault_v1_by_address(vault_address, chain_id)
  }, error = function(e) NULL)

  # Get total supply - this is crucial for share percentage
  total_supply_human <- if (!is.null(vault_info) && !is.null(vault_info$total_supply)) {
    as.numeric(vault_info$total_supply) / 1e18
  } else {
    NA_real_
  }

  # Determine asset decimals from vault info or use default
  asset_decimals <- if (!is.null(vault_info)) {
    vault_info$asset_decimals %||% 6
  } else {
    6  # Default to USDC-like
  }

  query <- '
  query GetVaultV1Depositors($address: String!, $first: Int) {
    vaultPositions(
      first: $first
      orderBy: Shares
      orderDirection: Desc
      where: { vaultAddress_in: [$address] }
    ) {
      items {
        user {
          address
        }
        state {
          shares
          assets
          assetsUsd
        }
      }
      pageInfo {
        countTotal
        count
      }
    }
  }
  '

  variables <- list(
    address = vault_address,
    first = first
  )

  result <- .execute_query(query, variables)

  if (is.null(result$data$vaultPositions$items)) {
    return(tibble())
  }

  items <- result$data$vaultPositions$items

  # Calculate total supply from the positions if vault_info failed
  if (is.na(total_supply_human) && length(items) > 0) {
    total_shares_raw <- sum(map_dbl(items, ~ as.numeric(.x$state$shares %||% 0)), na.rm = TRUE)
    total_supply_human <- total_shares_raw / 1e18
    if (verbose) message("Note: Using sum of positions as total supply")
  }

  depositors <- map_dfr(items, function(p) {
    shares_raw <- as.numeric(p$state$shares %||% 0)
    shares_human <- shares_raw / 1e18

    assets_raw <- as.numeric(p$state$assets %||% 0)
    assets_usd <- as.numeric(p$state$assetsUsd %||% 0)
    assets_human <- assets_raw / (10 ^ asset_decimals)

    # Calculate share percentage safely
    share_pct <- if (!is.na(total_supply_human) && total_supply_human > 0) {
      (shares_human / total_supply_human) * 100
    } else {
      NA_real_
    }

    tibble(
      user_address = p$user$address,
      shares = shares_human,
      assets = assets_human,
      assets_usd = assets_usd,
      share_pct = share_pct
    )
  }) %>%
    arrange(desc(assets_usd))

  # Add attributes
  attr(depositors, "vault_address") <- vault_address
  attr(depositors, "total_supply") <- total_supply_human
  attr(depositors, "asset_decimals") <- asset_decimals
  attr(depositors, "pagination") <- list(
    count_total = result$data$vaultPositions$pageInfo$countTotal %||% NA,
    count = result$data$vaultPositions$pageInfo$count %||% NA
  )
  attr(depositors, "chain_id") <- chain_id
  attr(depositors, "chain_name") <- CHAIN_NAMES[as.character(chain_id)]

  # Set class for custom printing
  class(depositors) <- c("vault_depositors", class(depositors))

  return(depositors)
}

#' Get transaction history for a specific Vault V1
#'
#' @description
#' Retrieves recent transactions involving a Vault V1, including deposits,
#' withdrawals, fee collections, and other vault operations.
#'
#' @param vault_address Character. Ethereum address of the vault
#' @param first Integer. Number of transactions to return (default: 10)
#' @param skip Integer. Number of transactions to skip for pagination (default: 0)
#' @param chain_id Integer. Chain ID (kept for compatibility but not used in V1 - see note)
#'
#' @note The chain_id parameter is not used in the V1 API but is kept for interface consistency with V2 functions.
#'
#' @return A tibble with transaction data
#' @export
get_vault_v1_transactions <- function(vault_address, first = 10, skip = 0, chain_id = NULL) {

  if (!.is_valid_address(vault_address)) {
    stop("Invalid Ethereum address format: ", vault_address)
  }

  # Note: chain_id is not used in V1 query but kept for API consistency

  query <- '
  query GetVaultV1Transactions($address: String!, $first: Int, $skip: Int) {
    transactions(
      first: $first
      skip: $skip
      orderBy: Timestamp
      orderDirection: Desc
      where: { vaultAddress_in: [$address] }
    ) {
      items {
        hash
        timestamp
        type
        blockNumber
        user {
          address
        }
        data {
          ... on VaultTransactionData {
            shares
            assets
            vault {
              address
            }
          }
        }
      }
      pageInfo {
        countTotal
        count
        skip
        limit
      }
    }
  }
  '

  variables <- list(
    address = vault_address,
    first = first,
    skip = skip
  )

  result <- .execute_query(query, variables)

  if (is.null(result$data$transactions$items)) {
    return(tibble())
  }

  items <- result$data$transactions$items

  transactions <- map_dfr(items, function(tx) {
    # Parse transaction data
    shares <- if (!is.null(tx$data$shares)) as.numeric(tx$data$shares) / 1e18 else NA_real_
    assets <- if (!is.null(tx$data$assets)) as.numeric(tx$data$assets) / 1e6 else NA_real_  # Assume 6 decimals

    tibble(
      tx_hash = tx$hash,
      timestamp = as.POSIXct(as.numeric(tx$timestamp %||% 0), origin = "1970-01-01"),
      type = tx$type %||% NA,
      block_number = as.integer(tx$blockNumber %||% 0),
      user_address = tx$user$address %||% NA,
      shares = shares,
      assets = assets,
      vault_address = tx$data$vault$address %||% vault_address
    )
  }) %>%
    arrange(desc(timestamp))

  # Add pagination info as attribute
  if (!is.null(result$data$transactions$pageInfo)) {
    attr(transactions, "pagination") <- list(
      count_total = result$data$transactions$pageInfo$countTotal %||% NA,
      count = result$data$transactions$pageInfo$count %||% NA,
      skip = result$data$transactions$pageInfo$skip %||% NA,
      limit = result$data$transactions$pageInfo$limit %||% NA
    )
  }

  return(transactions)
}

#' Get market positions for a specific user with automatic chain detection
#'
#' @description
#' Fetches all market positions for a specific user, including borrow and supply amounts.
#' This is critical for calculating actual Weighted Average LTV (WA LTV).
#'
#' @param user_address Character. Ethereum address of the user
#' @param chain_id Integer. Optional specific chain ID. If NULL, scans all chains.
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return A tibble with user's positions across markets
#' @export
get_user_market_positions_v1 <- function(user_address, chain_id = NULL, verbose = TRUE) {

  if (!.is_valid_address(user_address)) {
    stop("Invalid Ethereum address format: ", user_address)
  }

  # If chain_id is provided, use it directly
  if (!is.null(chain_id)) {
    if (verbose) cat("🔍 Checking chain", chain_id, "...\n")
    return(.fetch_user_market_positions_v1(user_address, chain_id))
  }

  # Auto-scan all chains
  if (verbose) cat("🔍 Scanning all chains for user:", user_address, "\n")

  all_chains <- as.numeric(names(CHAIN_NAMES))

  for (cid in all_chains) {
    if (verbose) cat("  Checking chain", cid, "(", CHAIN_NAMES[as.character(cid)], ")...\n")

    result <- tryCatch({
      .fetch_user_market_positions_v1(user_address, cid)
    }, error = function(e) NULL)

    if (!is.null(result) && nrow(result) > 0) {
      if (verbose) cat("  ✅ Found positions on chain", cid, "\n")
      attr(result, "chain_id") <- cid
      return(result)
    }
  }

  if (verbose) cat("  ❌ No positions found on any chain\n")
  tibble()
}

#' Internal function to fetch user market positions for a specific chain
#'
#' @param user_address Character. Ethereum address of the user
#' @param chain_id Integer. Chain ID
#' @return A tibble with user's positions
#' @keywords internal
.fetch_user_market_positions_v1 <- function(user_address, chain_id) {

  query <- '
  query GetUserMarketPositions($address: String!, $chainId: Int!) {
    userByAddress(address: $address, chainId: $chainId) {
      marketPositions {
        market {
          uniqueKey
        }
        borrowAssets
        borrowAssetsUsd
        supplyAssets
        supplyAssetsUsd
      }
    }
  }
  '

  variables <- list(
    address = user_address,
    chainId = chain_id
  )

  result <- tryCatch({
    .execute_query(query, variables)
  }, error = function(e) NULL)

  if (is.null(result) || is.null(result$data$userByAddress$marketPositions)) {
    return(tibble())
  }

  positions <- result$data$userByAddress$marketPositions

  if (length(positions) == 0) {
    return(tibble())
  }

  map_dfr(positions, function(p) {
    borrow_assets <- as.numeric(p$borrowAssets %||% 0)
    supply_assets <- as.numeric(p$supplyAssets %||% 0)

    tibble(
      market_key = p$market$uniqueKey,
      borrow_assets = borrow_assets,
      borrow_usd = as.numeric(p$borrowAssetsUsd %||% 0),
      supply_assets = supply_assets,
      supply_usd = as.numeric(p$supplyAssetsUsd %||% 0),
      position_ltv = ifelse(supply_assets > 0,
                            borrow_assets / supply_assets,
                            NA_real_)  # NA for borrow-only positions
    )
  })
}


#' Get all depositors for a vault and fetch their market positions (V1)
#'
#' @description
#' For a given vault, fetches all top depositors and then retrieves their
#' market positions to calculate actual Weighted Average LTV (WA LTV) per market.
#'
#' @param vault_address Character. Vault address
#' @param chain_id Integer. Optional specific chain ID. If NULL, scans all chains.
#' @param max_depositors Integer. Maximum number of depositors to process (default: 50)
#' @param verbose Logical. Print progress (default: TRUE)
#' @param min_supply_usd Numeric. Minimum supply amount to include (default: 1.0)
#'
#' @return A tibble with position-level LTV data across all markets
#' @export
get_vault_position_ltvs_v1 <- function(vault_address, chain_id = NULL, max_depositors = 20,
                                       verbose = TRUE, min_supply_usd = 1.0) {

  if (!.is_valid_address(vault_address)) {
    stop("Invalid Ethereum address format: ", vault_address)
  }

  if (verbose) cat("   Fetching depositors for vault", substr(vault_address, 1, 10), "...\n")

  # Get depositors (with auto-chain detection)
  depositors <- get_vault_v1_depositors(vault_address, first = max_depositors,
                                        chain_id = chain_id, verbose = verbose)

  if (nrow(depositors) == 0) {
    if (verbose) cat("   No depositors found\n")
    return(tibble())
  }

  # Extract the chain_id where depositors were found
  found_chain_id <- attr(depositors, "chain_id") %||% chain_id %||% 1

  if (verbose) cat("   Processing", nrow(depositors), "depositors on chain", found_chain_id, "...\n")

  all_positions <- map_dfr(1:nrow(depositors), function(i) {
    if (verbose && i %% 10 == 0) cat("     Processed", i, "depositors\n")

    user_addr <- depositors$user_address[i]

    positions <- tryCatch({
      get_user_market_positions_v1(user_addr, chain_id = found_chain_id, verbose = FALSE)
    }, error = function(e) NULL)

    if (!is.null(positions) && nrow(positions) > 0) {
      positions %>%
        mutate(
          user_address = user_addr,
          deposit_in_vault = depositors$assets_usd[i]
        ) %>%
        # Only include positions with meaningful supply
        filter(supply_usd >= min_supply_usd) %>%
        # Only include positions with valid LTV (not NA)
        filter(!is.na(position_ltv))
    } else {
      NULL
    }
  })

  if (verbose) cat("   Collected", nrow(all_positions), "meaningful positions (≥ $", min_supply_usd, ")\n")

  attr(all_positions, "chain_id") <- found_chain_id
  all_positions
}

#' Calculate Weighted Average LTV (WA LTV) for all markets in a vault
#'
#' @description
#' Calculates market-level WA LTV using ONLY positions with actual borrowing (LTV > 0).
#' Positions with LTV = 0 (supply-only) are excluded as they don't contribute to LTV risk.
#'
#' @param vault_address Character. Vault address
#' @param chain_id Integer. Optional specific chain ID. If NULL, scans all chains.
#' @param max_depositors Integer. Maximum number of depositors to process (default: 50)
#' @param verbose Logical. Print progress (default: TRUE)
#' @param min_supply_usd Numeric. Minimum supply to include in calculation (default: 1.0)
#' @param min_ltv_threshold Numeric. Minimum LTV to consider (default: 0.01, i.e., 1%)
#'
#' @return A tibble with WA LTV by market (ONLY markets with actual borrowing)
#' @export
calculate_market_wa_ltv <- function(vault_address, chain_id = NULL, max_depositors = 20,
                                    verbose = TRUE, min_supply_usd = 1.0,
                                    min_ltv_threshold = 0.01) {

  # Fetch positions with minimum supply threshold (auto-detects chain)
  positions <- get_vault_position_ltvs_v1(
    vault_address,
    chain_id = chain_id,
    max_depositors = max_depositors,
    verbose = verbose,
    min_supply_usd = min_supply_usd
  )

  if (nrow(positions) == 0) {
    if (verbose) cat("\n⚠️ No positions with meaningful supply (≥ $", min_supply_usd, ") found\n", sep = "")
    return(tibble(
      market_key = character(),
      n_borrowers = integer(),
      n_total_users = integer(),
      total_supply_usd = numeric(),
      total_borrow_usd = numeric(),
      wa_ltv = numeric(),
      utilization = numeric(),
      wa_ltv_formatted = character(),
      utilization_formatted = character(),
      confidence = character(),
      note = paste0("No positions with supply ≥ $", min_supply_usd)
    ))
  }

  # Step 1: Filter to ONLY positions with LTV > threshold (actual borrowing)
  positions_with_borrow <- positions %>%
    filter(position_ltv >= min_ltv_threshold)

  if (nrow(positions_with_borrow) == 0) {
    if (verbose) cat("\nℹ️ No borrowing positions found (all LTV = 0)\n")
    return(tibble(
      market_key = character(),
      n_borrowers = integer(),
      n_total_users = integer(),
      total_supply_usd = numeric(),
      total_borrow_usd = numeric(),
      wa_ltv = numeric(),
      utilization = numeric(),
      wa_ltv_formatted = character(),
      utilization_formatted = character(),
      confidence = character(),
      note = "No borrowing activity - all positions are supply-only"
    ))
  }

  # Step 2: Calculate user-level aggregates (only for users with borrowing)
  # But we need their TOTAL supply across ALL markets for correct LTV
  users_with_borrow <- unique(positions_with_borrow$user_address)

  user_totals <- positions %>%
    filter(user_address %in% users_with_borrow) %>%  # Include ALL positions of these users
    group_by(user_address) %>%
    summarise(
      total_user_supply_usd = sum(supply_usd, na.rm = TRUE),
      total_user_borrow_usd = sum(borrow_usd, na.rm = TRUE),
      total_user_deposit = sum(deposit_in_vault, na.rm = TRUE),
      user_ltv = total_user_borrow_usd / total_user_supply_usd,  # Will be >0 by construction
      .groups = "drop"
    )

  # Step 3: Join user LTV back to borrowing positions
  positions_with_user_ltv <- positions_with_borrow %>%
    left_join(user_totals %>% select(user_address, user_ltv), by = "user_address")

  # Step 4: Calculate market-level WA LTV (ONLY for markets with borrowing)
  market_wa_ltv <- positions_with_user_ltv %>%
    group_by(market_key) %>%
    summarise(
      n_borrowers = n_distinct(user_address),
      n_total_users = length(users_with_borrow),  # Total borrowers across all markets
      total_supply_usd = sum(supply_usd, na.rm = TRUE),  # Supply in THIS market from borrowers
      total_borrow_usd = sum(borrow_usd, na.rm = TRUE),  # Borrow in THIS market

      # WA LTV: weighted by supply in this market, using user's global LTV
      wa_ltv = sum(user_ltv * supply_usd, na.rm = TRUE) / sum(supply_usd, na.rm = TRUE),

      # Market utilization (borrow/supply in this market only)
      utilization = total_borrow_usd / total_supply_usd,
      .groups = "drop"
    ) %>%
    mutate(
      wa_ltv = round(wa_ltv, 4),
      utilization = round(utilization, 4),
      wa_ltv_formatted = paste0(round(wa_ltv * 100, 1), "%"),
      utilization_formatted = paste0(round(utilization * 100, 1), "%"),
      confidence = case_when(
        n_borrowers >= 10 ~ "High",
        n_borrowers >= 5 ~ "Medium",
        n_borrowers >= 1 ~ "Low",
        TRUE ~ "Very Low"
      ),
      risk_flag = case_when(
        wa_ltv > 0.9 ~ "🔴 CRITICAL",
        wa_ltv > 0.75 ~ "🟠 HIGH",
        wa_ltv > 0.6 ~ "🟡 MEDIUM",
        wa_ltv > 0 ~ "🟢 LOW",
        TRUE ~ "⚪ NONE"
      )
    ) %>%
    arrange(desc(wa_ltv))

  # Show summary
  if (verbose) {
    cat("\n📊 WA LTV Summary (BORROWING POSITIONS ONLY):\n")
    cat("   Total borrowers analyzed:", length(users_with_borrow), "\n")
    cat("   Markets with borrowing activity:", nrow(market_wa_ltv), "\n")
    cat("   Total supply from borrowers: $", format(round(sum(market_wa_ltv$total_supply_usd), 0), big.mark = ","), "\n")
    cat("   Total borrow: $", format(round(sum(market_wa_ltv$total_borrow_usd), 0), big.mark = ","), "\n")
    cat("   Average WA LTV (weighted by supply):",
        round(weighted.mean(market_wa_ltv$wa_ltv, market_wa_ltv$total_supply_usd, na.rm = TRUE) * 100, 1), "%\n")
  }

  attr(market_wa_ltv, "chain_id") <- attr(positions, "chain_id")
  market_wa_ltv
}

#' Get a summary of top Vault V1s by TVL
#'
#' @description
#' Retrieves a formatted summary of the largest Vault V1s by TVL,
#' including key metrics like share price and market count.
#'
#' @param n Integer. Number of top vaults to return (default: 10)
#' @param chain_ids Integer vector. Filter by chain IDs (default: c(1, 8453))
#' @param human_readable Logical. Include formatted columns (default: TRUE)
#'
#' @return A tibble with top V1 vaults (with print method)
#' @export
get_vault_v1_summary <- function(n = 10, chain_ids = c(1, 8453), human_readable = TRUE) {

  vaults <- get_vault_v1_list(first = 200, chain_ids = chain_ids,
                              human_readable = human_readable)

  # Get allocation data to add market counts
  allocations <- tryCatch({
    get_vault_v1_allocations(first = 200, chain_ids = chain_ids)
  }, error = function(e) NULL)

  # Calculate market counts per vault
  market_counts <- if (!is.null(allocations)) {
    allocations %>%
      group_by(vault_address) %>%
      summarise(n_markets = n(), .groups = "drop")
  } else {
    tibble(vault_address = character(), n_markets = integer())
  }

  result <- vaults %>%
    left_join(market_counts, by = "vault_address") %>%
    mutate(n_markets = coalesce(n_markets, 0L)) %>%
    select(
      vault_address,
      name,
      symbol,
      chain_network,
      tvl_formatted,
      tvl_billions,
      share_price_formatted,
      n_markets,
      curator
    ) %>%
    arrange(desc(tvl_billions)) %>%
    head(n) %>%
    mutate(rank = row_number()) %>%
    select(rank, everything())

  class(result) <- c("vault_v1_summary", class(result))
  return(result)
}

#' Print method for V1 vault summary
#' @export
print.vault_v1_summary <- function(x, ...) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("🏆 TOP", nrow(x), "VAULT V1s BY TVL\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")

  for (i in 1:nrow(x)) {
    cat(sprintf("%2d. %-35s: %s - %s (%d markets, %s)\n",
                x$rank[i],
                substr(x$name[i], 1, 35),
                x$tvl_formatted[i],
                x$share_price_formatted[i],
                x$n_markets[i],
                x$curator[i] %||% "Unknown"))
  }

  cat(paste(rep("=", 80), collapse = ""), "\n")
  invisible(x)
}

#' Get Positioning Data from Morpho Vaults (BTC Markets)
#'
#' @description
#' Retrieves and analyzes positioning data from Morpho vaults, specifically tracking
#' BTC exposure through two channels:
#' 1. Direct deposits of BTC tokens (WBTC, cbBTC) into vaults
#' 2. Vault allocations to markets where BTC tokens are used as collateral
#'
#' This provides a comprehensive view of "smart money" flows and whale activity
#' in Bitcoin DeFi lending markets.
#'
#' @param chain_ids Integer vector. Chain IDs to query (default: c(1, 8453) - Ethereum and Base)
#' @param min_exposure_usd Numeric. Minimum BTC exposure in USD to consider a vault (default: 1e5, i.e., $100k)
#' @param include_depositors Logical. Whether to fetch top depositors (slower) (default: FALSE)
#' @param top_n_vaults Integer. Number of top vaults to fetch depositors for (default: 3)
#' @param max_depositors_per_vault Integer. Maximum number of depositors to fetch per vault (default: 10)
#' @param human_readable Logical. Include formatted columns (default: TRUE)
#'
#' @return A list object of class \code{morpho_positioning_data}
#' @export
get_morpho_positioning <- function(chain_ids = c(1, 8453),
                                   min_exposure_usd = 1e7,
                                   include_depositors = FALSE,
                                   top_n_vaults = 5,
                                   max_depositors_per_vault = 20,
                                   human_readable = TRUE) {

  #---------------------------------------------------------------------------
  # INITIALIZATION
  #---------------------------------------------------------------------------
  message("🐋 Fetching Morpho vault positioning data...")

  result <- list(
    timestamp = Sys.time(),
    chain_ids = chain_ids,
    status = "pending",
    warnings = character(),
    errors = character()
  )

  #---------------------------------------------------------------------------
  # FETCH VAULT DATA
  #---------------------------------------------------------------------------
  tryCatch({
    # Get all vaults
    all_vaults <- get_vault_v1_list(
      first = 200,
      chain_ids = chain_ids,
      human_readable = human_readable
    )

    if (is.null(all_vaults) || nrow(all_vaults) == 0) {
      result$warnings <- c(result$warnings, "No vaults found on specified chains")
      result$status <- "no_data"
      return(result)
    }

    message("  ✅ Retrieved ", nrow(all_vaults), " total vaults")

    #---------------------------------------------------------------------------
    # FETCH ALLOCATION DATA (to see where vaults lend)
    #---------------------------------------------------------------------------
    allocations <- tryCatch({
      get_vault_v1_allocations(
        first = 200,
        chain_ids = chain_ids,
        human_readable = FALSE
      )
    }, error = function(e) {
      result$warnings <- c(result$warnings, paste("Could not fetch allocations:", e$message))
      NULL
    })

    # Define BTC tokens (case-insensitive)
    BTC_TOKENS <- c("WBTC", "cbBTC", "wBTC", "BTC", "wbtc", "cbbtc")

    #---------------------------------------------------------------------------
    # WAY 1: Direct BTC Deposits
    #---------------------------------------------------------------------------
    btc_deposit_vaults <- all_vaults %>%
      dplyr::filter(tolower(asset_symbol) %in% tolower(BTC_TOKENS)) %>%
      dplyr::mutate(
        exposure_type = "Direct BTC Deposits",
        deposit_asset = asset_symbol,
        btc_exposure_usd = total_assets_usd,
        btc_exposure_pct = 100,
        n_btc_markets = 0
      ) %>%
      dplyr::filter(btc_exposure_usd >= min_exposure_usd) %>%
      dplyr::select(
        vault_address, name, chain_network, exposure_type, deposit_asset,
        total_assets_usd, tvl_formatted, btc_exposure_usd, btc_exposure_pct, n_btc_markets
      )

    message("  ✅ Found ", nrow(btc_deposit_vaults), " direct BTC deposit vaults")

    #---------------------------------------------------------------------------
    # WAY 2: BTC-Collateralized Lending (via allocations)
    #---------------------------------------------------------------------------
    btc_collateral_vaults <- NULL

    if (!is.null(allocations) && nrow(allocations) > 0) {
      # Find markets where collateral is a BTC token
      btc_markets <- allocations %>%
        dplyr::filter(tolower(collateral_asset) %in% tolower(BTC_TOKENS)) %>%
        dplyr::distinct(market_key)

      if (nrow(btc_markets) > 0) {
        # Find vaults allocated to those markets
        btc_exposed_allocations <- allocations %>%
          dplyr::filter(market_key %in% btc_markets$market_key) %>%
          dplyr::group_by(vault_address) %>%
          dplyr::summarise(
            btc_exposure_usd = sum(supply_assets_usd, na.rm = TRUE),
            btc_exposure_pct = sum(allocation_pct, na.rm = TRUE),
            n_btc_markets = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::filter(btc_exposure_usd >= min_exposure_usd)

        if (nrow(btc_exposed_allocations) > 0) {
          # Join with vault details
          btc_collateral_vaults <- btc_exposed_allocations %>%
            dplyr::left_join(
              all_vaults %>%
                dplyr::select(vault_address, name, chain_network, total_assets_usd, tvl_formatted, asset_symbol),
              by = "vault_address"
            ) %>%
            dplyr::mutate(
              exposure_type = "BTC-Collateralized Lending",
              deposit_asset = asset_symbol,
              btc_exposure_pct = pmin(btc_exposure_pct, 100)  # Cap at 100%
            ) %>%
            dplyr::select(
              vault_address, name, chain_network, exposure_type, deposit_asset,
              total_assets_usd, tvl_formatted, btc_exposure_usd, btc_exposure_pct, n_btc_markets
            )
        }
      }
    }

    if (!is.null(btc_collateral_vaults)) {
      message("  ✅ Found ", nrow(btc_collateral_vaults), " BTC-collateral lending vaults")
    } else {
      message("  ✅ Found 0 BTC-collateral lending vaults")
    }

    #---------------------------------------------------------------------------
    # COMBINE ALL BTC-EXPOSED VAULTS
    #---------------------------------------------------------------------------
    all_btc_vaults <- dplyr::bind_rows(btc_deposit_vaults, btc_collateral_vaults) %>%
      dplyr::arrange(dplyr::desc(btc_exposure_usd))

    if (nrow(all_btc_vaults) == 0) {
      result$warnings <- c(result$warnings,
                           paste("No BTC-exposed vaults found with exposure ≥ $",
                                 format(min_exposure_usd, scientific = FALSE)))
      result$status <- "no_btc_data"
      return(result)
    }

    #---------------------------------------------------------------------------
    # CALCULATE AGGREGATE METRICS
    #---------------------------------------------------------------------------
    total_btc_exposure_usd <- sum(all_btc_vaults$btc_exposure_usd, na.rm = TRUE)

    # Categorize vaults by size
    n_large_vaults <- sum(all_btc_vaults$total_assets_usd > 100e6, na.rm = TRUE)      # > $100M
    n_medium_vaults <- sum(all_btc_vaults$total_assets_usd > 10e6 &
                             all_btc_vaults$total_assets_usd <= 100e6, na.rm = TRUE)    # $10M-$100M
    n_small_vaults <- sum(all_btc_vaults$total_assets_usd <= 10e6, na.rm = TRUE)       # < $10M

    # Determine whale signal
    whale_signal <- dplyr::case_when(
      n_large_vaults >= 1 ~ paste0("High Whale Intensity (", n_large_vaults,
                                   " vault", ifelse(n_large_vaults > 1, "s > $100M", " > $100M")),
      n_medium_vaults >= 3 ~ paste0("Moderate Whale Activity (", n_medium_vaults, " vaults $10M-$100M)"),
      n_medium_vaults >= 1 ~ "Normal Whale Activity",
      total_btc_exposure_usd > 1e6 ~ "Low Whale Activity",
      TRUE ~ "Minimal Activity"
    )

    #---------------------------------------------------------------------------
    # IDENTIFY TOP VAULTS
    #---------------------------------------------------------------------------
    top_vaults <- all_btc_vaults %>%
      utils::head(5) %>%
      dplyr::mutate(
        exposure_formatted = paste0("$", format(round(btc_exposure_usd / 1e6, 1), big.mark = ","), "M"),
        vault_display = paste0(name, " (", exposure_type, ")")
      ) %>%
      dplyr::select(
        vault_display,
        chain_network,
        deposit_asset,
        exposure_formatted,
        btc_exposure_pct,
        n_btc_markets
      )

    #---------------------------------------------------------------------------
    # FETCH TOP DEPOSITORS FOR MULTIPLE VAULTS (Optional, slower)
    #---------------------------------------------------------------------------
    all_top_depositors <- NULL
    if (include_depositors && nrow(all_btc_vaults) > 0) {

      # Determine how many vaults to process
      n_vaults_to_process <- min(top_n_vaults, nrow(all_btc_vaults))
      message("  🔍 Fetching top depositors for top ", n_vaults_to_process, " BTC vaults...")

      depositor_list <- list()

      for (i in 1:n_vaults_to_process) {
        vault_addr <- all_btc_vaults$vault_address[i]
        vault_name <- all_btc_vaults$name[i]
        vault_tvl <- all_btc_vaults$total_assets_usd[i]

        message("    Processing: ", vault_name, " ($", format(round(vault_tvl / 1e6, 1), big.mark = ","), "M)")

        depositors <- tryCatch({
          get_vault_v1_depositors(
            vault_addr,
            first = max_depositors_per_vault,
            verbose = FALSE
          )
        }, error = function(e) {
          message("      Error fetching depositors: ", e$message)
          NULL
        })

        if (!is.null(depositors) && nrow(depositors) > 0) {
          depositor_list[[i]] <- depositors %>%
            dplyr::mutate(
              vault_address = vault_addr,
              vault_name = vault_name,
              vault_tvl = vault_tvl,
              vault_rank = i
            ) %>%
            dplyr::select(
              vault_rank, vault_name, vault_tvl,
              user_address, assets_usd, share_pct
            ) %>%
            dplyr::arrange(dplyr::desc(assets_usd))

          message("      Found ", nrow(depositor_list[[i]]), " depositors")
        }
      }

      if (length(depositor_list) > 0) {
        all_top_depositors <- dplyr::bind_rows(depositor_list)

        # Add whale classification per depositor
        all_top_depositors <- all_top_depositors %>%
          dplyr::mutate(
            whale_size = dplyr::case_when(
              assets_usd > 10e6 ~ "🐋 Mega Whale (>$10M)",
              assets_usd > 1e6 ~ "🐋 Whale ($1M-$10M)",
              assets_usd > 100e3 ~ "🐟 Dolphin ($100k-$1M)",
              TRUE ~ "🦐 Retail (<$100k)"
            )
          )

        message("  ✅ Total depositors collected: ", nrow(all_top_depositors))
      }
    }

    #---------------------------------------------------------------------------
    # CALCULATE WHALE CONCENTRATION METRICS
    #---------------------------------------------------------------------------
    whale_concentration <- NULL
    if (!is.null(all_top_depositors) && nrow(all_top_depositors) > 0) {

      # Calculate Herfindahl-Hirschman Index (HHI) for market concentration
      # HHI = sum of squared market shares (0-10000 scale)
      shares_sq <- sum((all_top_depositors$share_pct)^2, na.rm = TRUE)
      hhi <- shares_sq * 100  # Scale to 0-10000

      whale_concentration <- list(
        total_whale_wallets = length(unique(all_top_depositors$user_address)),
        mega_whales = sum(all_top_depositors$whale_size == "🐋 Mega Whale (>$10M)", na.rm = TRUE),
        regular_whales = sum(all_top_depositors$whale_size == "🐋 Whale ($1M-$10M)", na.rm = TRUE),
        hhi = hhi,
        hhi_interpretation = dplyr::case_when(
          hhi > 2500 ~ "Highly Concentrated",
          hhi > 1500 ~ "Moderately Concentrated",
          TRUE ~ "Competitive"
        ),
        top_5_share = sum(head(all_top_depositors$share_pct, 5), na.rm = TRUE)
      )
    }

    #---------------------------------------------------------------------------
    # PREPARE RETURN STRUCTURE
    #---------------------------------------------------------------------------
    result$vault_summary <- all_btc_vaults %>%
      dplyr::mutate(
        btc_exposure_formatted = paste0("$", format(round(btc_exposure_usd / 1e6, 1), big.mark = ","), "M"),
        exposure_pct_formatted = paste0(round(btc_exposure_pct, 1), "%")
      )

    result$whale_activity <- list(
      total_btc_exposure_usd = total_btc_exposure_usd,
      total_btc_exposure_formatted = paste0("$", format(round(total_btc_exposure_usd / 1e6, 1), big.mark = ","), "M"),
      total_vaults = nrow(all_btc_vaults),
      whale_signal = whale_signal,
      largest_vault = paste0(all_btc_vaults$name[1], ": $",
                             format(round(all_btc_vaults$btc_exposure_usd[1] / 1e6, 1), big.mark = ","), "M"),
      large_vaults_count = n_large_vaults,
      medium_vaults_count = n_medium_vaults,
      small_vaults_count = n_small_vaults,
      direct_btc_vaults = nrow(btc_deposit_vaults),
      collateral_btc_vaults = if (!is.null(btc_collateral_vaults)) nrow(btc_collateral_vaults) else 0
    )

    result$top_vaults <- top_vaults
    result$top_depositors <- all_top_depositors
    result$whale_concentration <- whale_concentration
    result$raw_allocations <- allocations
    result$status <- "success"

    # Interpretation
    result$interpretation <- paste0(
      "Total BTC exposure: ", result$whale_activity$total_btc_exposure_formatted, " across ",
      nrow(all_btc_vaults), " vaults (",
      result$whale_activity$direct_btc_vaults, " direct, ",
      result$whale_activity$collateral_btc_vaults, " collateral). "
    )

    if (!is.null(whale_concentration)) {
      result$interpretation <- paste0(
        result$interpretation,
        "Identified ", whale_concentration$total_whale_wallets, " unique whale wallets across top vaults. ",
        "Market concentration: ", whale_concentration$hhi_interpretation, "."
      )
    }

  }, error = function(e) {
    result$errors <- c(result$errors, paste("Error fetching data:", e$message))
    result$status <- "failed"
  })

  #---------------------------------------------------------------------------
  # RETURN
  #---------------------------------------------------------------------------
  class(result) <- "morpho_positioning_data"
  return(result)
}

#' Print method for morpho_positioning_data
#'
#' @param x morpho_positioning_data object
#' @param ... Additional arguments
#' @export
print.morpho_positioning_data <- function(x, ...) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("🐋 MORPHO BTC EXPOSURE - VAULT POSITIONING\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Timestamp:", format(x$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Status:", toupper(x$status), "\n")

  if (x$status == "success") {
    cat("\n", paste(rep("━", 80), collapse = ""), "\n")
    cat("📊 BTC EXPOSURE SUMMARY\n")
    cat(paste(rep("━", 80), collapse = ""), "\n")
    cat(x$interpretation, "\n\n")

    cat("Whale Activity:", x$whale_activity$whale_signal, "\n")
    cat("Total BTC Exposure:", x$whale_activity$total_btc_exposure_formatted, "\n")
    cat("Number of BTC-Exposed Vaults:", x$whale_activity$total_vaults, "\n")
    cat("  • Direct Deposit Vaults:", x$whale_activity$direct_btc_vaults, "\n")
    cat("  • Collateral Lending Vaults:", x$whale_activity$collateral_btc_vaults, "\n")
    cat("Vault Size Distribution:\n")
    cat("  • Large (>$100M):", x$whale_activity$large_vaults_count, "\n")
    cat("  • Medium ($10M-$100M):", x$whale_activity$medium_vaults_count, "\n")
    cat("  • Small (<$10M):", x$whale_activity$small_vaults_count, "\n")
    cat("Largest Vault:", x$whale_activity$largest_vault, "\n")

    if (!is.null(x$whale_concentration)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🐋 WHALE CONCENTRATION ANALYSIS\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")
      cat("Unique Whale Wallets:", x$whale_concentration$total_whale_wallets, "\n")
      cat("Mega Whales (>$10M):", x$whale_concentration$mega_whales, "\n")
      cat("Regular Whales ($1M-$10M):", x$whale_concentration$regular_whales, "\n")
      cat("Market Concentration (HHI):", format(x$whale_concentration$hhi, scientific = FALSE),
          "(", x$whale_concentration$hhi_interpretation, ")\n")
      cat("Top 5 Depositors Share:", round(x$whale_concentration$top_5_share, 1), "%\n")
    }

    if (!is.null(x$top_vaults) && nrow(x$top_vaults) > 0) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🏆 TOP BTC-EXPOSED VAULTS\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      # Format for better display
      top_vaults_display <- x$top_vaults %>%
        dplyr::mutate(
          btc_exposure_pct = paste0(round(btc_exposure_pct, 1), "%")
        )
      print(top_vaults_display)
    }

    if (!is.null(x$top_depositors) && nrow(x$top_depositors) > 0) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("👥 TOP DEPOSITORS ACROSS VAULTS (Whale Wallets)\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      # Show summary by vault
      vault_summary <- x$top_depositors %>%
        dplyr::group_by(vault_rank, vault_name) %>%
        dplyr::summarise(
          depositors = dplyr::n(),
          total_whale_value = sum(assets_usd, na.rm = TRUE),
          top_share = max(share_pct, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          total_whale_value_formatted = paste0("$", format(round(total_whale_value / 1e6, 1), big.mark = ","), "M"),
          top_share = paste0(round(top_share, 1), "%")
        ) %>%
        dplyr::select(
          Rank = vault_rank,
          Vault = vault_name,
          Depositors = depositors,
          `Total Whale Value` = total_whale_value_formatted,
          `Top Share` = top_share
        )

      print(vault_summary)

      # Show top 10 depositors overall
      cat("\nTop 10 Largest Depositors Overall:\n")
      top_deps <- x$top_depositors %>%
        dplyr::arrange(dplyr::desc(assets_usd)) %>%
        utils::head(10) %>%
        dplyr::mutate(
          user_short = paste0(substr(user_address, 1, 6), "...", substr(user_address, nchar(user_address)-4, nchar(user_address))),
          assets_formatted = paste0("$", format(round(assets_usd / 1e6, 2), big.mark = ","), "M"),
          share_formatted = paste0(round(share_pct, 2), "%")
        ) %>%
        dplyr::select(
          Vault = vault_name,
          Whale = user_short,
          Amount = assets_formatted,
          Share = share_formatted,
          Size = whale_size
        )
      print(top_deps)
    }
  }

  if (length(x$warnings) > 0) {
    cat("\n⚠️ WARNINGS:\n")
    for (w in x$warnings) cat("  •", w, "\n")
  }

  if (length(x$errors) > 0) {
    cat("\n❌ ERRORS:\n")
    for (e in x$errors) cat("  •", e, "\n")
  }

  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  invisible(x)
}
