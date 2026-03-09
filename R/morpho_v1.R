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
#' @param include_flows Logical. Whether to fetch transaction data (up to 1000 tx) (default: FALSE)
#' @param top_n_vaults Integer. Number of top vaults to fetch depositors/flows for (default: 3)
#' @param max_depositors_per_vault Integer. Maximum number of depositors to fetch per vault (default: 10)
#' @param human_readable Logical. Include formatted columns (default: TRUE)
#'
#' @return A list object of class \code{morpho_positioning_data}
#' @export
get_morpho_positioning <- function(chain_ids = c(1, 8453),
                                   min_exposure_usd = 1e5,
                                   include_depositors = TRUE,
                                   include_flows = TRUE,
                                   top_n_vaults = 5,
                                   max_depositors_per_vault = 10,
                                   human_readable = TRUE,
                                   verbose = FALSE) {

  #---------------------------------------------------------------------------
  # INITIALIZATION
  #---------------------------------------------------------------------------
  message("🐋 Fetching Morpho vault positioning data...")

  result <- list(
    timestamp = Sys.time(),
    chain_ids = chain_ids,
    status = "success",
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
                dplyr::select(vault_address, name, chain_network, total_assets_usd, tvl_formatted, asset_symbol, share_price_usd),
              by = "vault_address"
            ) %>%
            dplyr::mutate(
              exposure_type = "BTC-Collateralized Lending",
              deposit_asset = asset_symbol,
              btc_exposure_pct = pmin(btc_exposure_pct, 100)
            ) %>%
            dplyr::select(
              vault_address, name, chain_network, exposure_type, deposit_asset,
              total_assets_usd, tvl_formatted, btc_exposure_usd, btc_exposure_pct, n_btc_markets,
              share_price_usd
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
    n_large_vaults <- sum(all_btc_vaults$total_assets_usd > 100e6, na.rm = TRUE)
    n_medium_vaults <- sum(all_btc_vaults$total_assets_usd > 10e6 &
                             all_btc_vaults$total_assets_usd <= 100e6, na.rm = TRUE)
    n_small_vaults <- sum(all_btc_vaults$total_assets_usd <= 10e6, na.rm = TRUE)

    # Determine whale signal
    whale_signal <- dplyr::case_when(
      n_large_vaults >= 1 ~ paste0("High Whale Intensity (", n_large_vaults,
                                   " vault", ifelse(n_large_vaults > 1, "s > $100M)", " > $100M)")),
      n_medium_vaults >= 3 ~ paste0("Moderate Whale Activity (", n_medium_vaults, " vaults $10M-$100M)"),
      n_medium_vaults >= 1 ~ "Normal Whale Activity",
      total_btc_exposure_usd > 1e6 ~ "Low Whale Activity",
      TRUE ~ "Minimal Activity"
    )

    #---------------------------------------------------------------------------
    # FETCH TOP DEPOSITORS FOR MULTIPLE VAULTS (Optional, slower)
    #---------------------------------------------------------------------------
    all_top_depositors <- NULL
    if (include_depositors && nrow(all_btc_vaults) > 0) {

      n_vaults_to_process <- min(top_n_vaults, nrow(all_btc_vaults))
      message("  🔍 Fetching top depositors for top ", n_vaults_to_process, " BTC vaults...")

      depositor_list <- list()

      for (i in 1:n_vaults_to_process) {
        vault_addr <- all_btc_vaults$vault_address[i]
        vault_name <- all_btc_vaults$name[i]
        vault_tvl <- all_btc_vaults$total_assets_usd[i]

        depositors <- tryCatch({
          get_vault_v1_depositors(
            vault_addr,
            first = max_depositors_per_vault,
            verbose = FALSE
          )
        }, error = function(e) {
          message("      Error fetching depositors for ", vault_name, ": ", e$message)
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

          if (verbose) message("      Found ", nrow(depositor_list[[i]]), " depositors for ", vault_name)
        }
      }

      if (length(depositor_list) > 0) {
        all_top_depositors <- dplyr::bind_rows(depositor_list)

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
    # FETCH TRANSACTIONS FOR TOP VAULTS (Optional, slower)
    #---------------------------------------------------------------------------
    vault_flows <- NULL
    if (include_flows && nrow(all_btc_vaults) > 0) {

      n_vaults_to_process <- min(top_n_vaults, nrow(all_btc_vaults))
      message("  📊 Fetching up to 1000 transactions for top ", n_vaults_to_process, " BTC vaults...")

      flow_list <- list()

      for (i in 1:n_vaults_to_process) {
        vault_addr <- all_btc_vaults$vault_address[i]
        vault_name <- all_btc_vaults$name[i]
        vault_tvl <- all_btc_vaults$total_assets_usd[i]
        share_price <- all_btc_vaults$share_price_usd[i]

        if (verbose) message("    Processing: ", vault_name, " ($", format(round(vault_tvl / 1e6, 1), big.mark = ","), "M)")

        # Fetch up to 1000 transactions
        all_txns <- tryCatch({
          get_vault_v1_transactions(
            vault_address = vault_addr,
            first = 1000,  # Max allowed
            skip = 0
          )
        }, error = function(e) {
          message("      Error fetching transactions: ", e$message)
          NULL
        })

        if (!is.null(all_txns) && nrow(all_txns) > 0) {

          # Get the actual date range of the transactions
          oldest_tx <- min(all_txns$timestamp, na.rm = TRUE)
          newest_tx <- max(all_txns$timestamp, na.rm = TRUE)
          days_covered <- as.numeric(difftime(newest_tx, oldest_tx, units = "days"))

          # Calculate flows
          inflow <- all_txns %>%
            dplyr::filter(grepl("Deposit", type, ignore.case = TRUE)) %>%
            dplyr::pull(assets) %>%
            sum(na.rm = TRUE)

          outflow <- all_txns %>%
            dplyr::filter(grepl("Withdraw", type, ignore.case = TRUE)) %>%
            dplyr::pull(assets) %>%
            sum(na.rm = TRUE)

          deposit_count <- all_txns %>%
            dplyr::filter(grepl("Deposit", type, ignore.case = TRUE)) %>%
            nrow()

          withdraw_count <- all_txns %>%
            dplyr::filter(grepl("Withdraw", type, ignore.case = TRUE)) %>%
            nrow()

          if (!is.na(share_price) && share_price > 0) {
            inflow_usd <- inflow * share_price
            outflow_usd <- outflow * share_price
            netflow_usd <- inflow_usd - outflow_usd
            netflow_pct <- (netflow_usd / vault_tvl) * 100
          } else {
            inflow_usd <- NA_real_
            outflow_usd <- NA_real_
            netflow_usd <- NA_real_
            netflow_pct <- NA_real_
          }

          flow_list[[i]] <- data.frame(
            vault_rank = i,
            vault_address = vault_addr,
            vault_name = vault_name,
            tx_count = nrow(all_txns),
            deposit_count = deposit_count,
            withdraw_count = withdraw_count,
            oldest_tx_date = oldest_tx,
            newest_tx_date = newest_tx,
            days_covered = round(days_covered, 1),
            inflow_units = inflow,
            outflow_units = outflow,
            netflow_units = inflow - outflow,
            inflow_usd = inflow_usd,
            outflow_usd = outflow_usd,
            netflow_usd = netflow_usd,
            netflow_pct = netflow_pct,
            vault_tvl_usd = vault_tvl,
            share_price_usd = share_price,
            stringsAsFactors = FALSE
          )

          if (verbose) {

            if (!is.na(netflow_usd)) {
              message("      Fetched ", nrow(all_txns), " transactions (", days_covered, " days) | Net flow: $",
                      format(round(netflow_usd / 1e6, 2), big.mark = ","), "M (",
                      round(netflow_pct, 1), "%)")
            } else {
              message("      Fetched ", nrow(all_txns), " transactions (", days_covered, " days) | Net flow: ",
                      round((inflow - outflow) / 1e6, 2), "M units")
            }

          }

        } else {
          message("      No transaction history found")
        }
      }

      if (length(flow_list) > 0) {
        vault_flows <- dplyr::bind_rows(flow_list) %>%
          dplyr::mutate(
            flow_direction = dplyr::case_when(
              !is.na(netflow_pct) & netflow_pct > 10 ~ "🚀 Strong Inflow",
              !is.na(netflow_pct) & netflow_pct > 2 ~ "📈 Inflow",
              !is.na(netflow_pct) & netflow_pct < -10 ~ "💥 Strong Outflow",
              !is.na(netflow_pct) & netflow_pct < -2 ~ "📉 Outflow",
              !is.na(netflow_pct) ~ "⚖️ Neutral",
              TRUE ~ "📊 Units Only"
            ),
            inflow_formatted = dplyr::case_when(
              !is.na(inflow_usd) ~ paste0("$", format(round(inflow_usd / 1e6, 2), big.mark = ","), "M"),
              TRUE ~ paste0(format(round(inflow_units / 1e6, 2), big.mark = ","), "M units")
            ),
            outflow_formatted = dplyr::case_when(
              !is.na(outflow_usd) ~ paste0("$", format(round(outflow_usd / 1e6, 2), big.mark = ","), "M"),
              TRUE ~ paste0(format(round(outflow_units / 1e6, 2), big.mark = ","), "M units")
            ),
            netflow_formatted = dplyr::case_when(
              !is.na(netflow_usd) ~ paste0(
                ifelse(netflow_usd > 0, "+", ""),
                "$", format(round(netflow_usd / 1e6, 2), big.mark = ","), "M"
              ),
              TRUE ~ paste0(
                ifelse(netflow_units > 0, "+", ""),
                format(round(netflow_units / 1e6, 2), big.mark = ","), "M units"
              )
            ),
            netflow_pct_formatted = ifelse(
              !is.na(netflow_pct),
              paste0(ifelse(netflow_pct > 0, "+", ""), round(netflow_pct, 1), "%"),
              "N/A"
            ),
            date_range = paste0(
              format(oldest_tx_date, "%Y-%m-%d"), " to ",
              format(newest_tx_date, "%Y-%m-%d"), " (", days_covered, " days)"
            )
          )

        message("  ✅ Flow data collected for ", nrow(vault_flows), " vaults")
      }
    }

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
    # CALCULATE WHALE CONCENTRATION METRICS
    #---------------------------------------------------------------------------
    whale_concentration <- NULL
    if (!is.null(all_top_depositors) && nrow(all_top_depositors) > 0) {

      shares_sq <- sum((all_top_depositors$share_pct / 100)^2, na.rm = TRUE)
      hhi <- shares_sq * 10000

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
    result$vault_flows <- vault_flows
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

    if (!is.null(vault_flows)) {
      vaults_with_usd <- vault_flows %>% dplyr::filter(!is.na(netflow_usd))

      if (nrow(vaults_with_usd) > 0) {
        total_inflow <- sum(vaults_with_usd$inflow_usd, na.rm = TRUE)
        total_outflow <- sum(vaults_with_usd$outflow_usd, na.rm = TRUE)
        net_flow_usd <- total_inflow - total_outflow
        total_tvl <- sum(vaults_with_usd$vault_tvl_usd, na.rm = TRUE)
        net_flow_pct <- (net_flow_usd / total_tvl) * 100

        # Get date range for the flow data
        min_date <- min(vault_flows$oldest_tx_date, na.rm = TRUE)
        max_date <- max(vault_flows$newest_tx_date, na.rm = TRUE)

        flow_regime <- dplyr::case_when(
          net_flow_pct > 10 ~ "Strong Inflows (Accumulation)",
          net_flow_pct > 2 ~ "Moderate Inflows",
          net_flow_pct < -10 ~ "Strong Outflows (Distribution)",
          net_flow_pct < -2 ~ "Moderate Outflows",
          TRUE ~ "Neutral"
        )

        result$flow_summary <- list(
          total_inflow_usd = total_inflow,
          total_outflow_usd = total_outflow,
          net_flow_usd = net_flow_usd,
          net_flow_pct = net_flow_pct,
          flow_regime = flow_regime,
          min_date = min_date,
          max_date = max_date,
          total_transactions = sum(vault_flows$tx_count),
          vaults_with_usd = nrow(vaults_with_usd)
        )

        result$interpretation <- paste0(
          result$interpretation, " Transaction analysis covers ",
          format(min_date, "%Y-%m-%d"), " to ", format(max_date, "%Y-%m-%d"), ". ",
          "Net flow: ", if (net_flow_usd > 0) "+" else "",
          "$", format(round(net_flow_usd / 1e6, 1), big.mark = ","), "M (",
          round(net_flow_pct, 1), "%, ", flow_regime, ")."
        )
      } else {
        # Get date range even for units-only data
        min_date <- min(vault_flows$oldest_tx_date, na.rm = TRUE)
        max_date <- max(vault_flows$newest_tx_date, na.rm = TRUE)

        result$flow_summary <- list(
          note = "Flow data available in units only (no share price)",
          min_date = min_date,
          max_date = max_date,
          total_transactions = sum(vault_flows$tx_count),
          vaults_with_units = nrow(vault_flows)
        )

        result$interpretation <- paste0(
          result$interpretation, " Transaction analysis covers ",
          format(min_date, "%Y-%m-%d"), " to ", format(max_date, "%Y-%m-%d"), ". ",
          "Net flow data available in units only (share price unavailable)."
        )
      }
    }

  }, error = function(e) {
    result$errors <- c(result$errors, paste("Error fetching data:", e$message))
    result$status <- "failed"
    message("❌ Error: ", e$message)
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

    #---------------------------------------------------------------------------
    # FLOW ANALYSIS SECTION
    #---------------------------------------------------------------------------
    if (!is.null(x$vault_flows) && nrow(x$vault_flows) > 0) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("📈 TRANSACTION ANALYSIS\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      # Show date range
      min_date <- min(x$vault_flows$oldest_tx_date)
      max_date <- max(x$vault_flows$newest_tx_date)
      cat("Date Range:", format(min_date, "%Y-%m-%d"), "to", format(max_date, "%Y-%m-%d"), "\n")
      cat("Total Transactions:", sum(x$vault_flows$tx_count), "\n\n")

      if (!is.null(x$flow_summary$net_flow_usd)) {
        cat("Aggregate Net Flow:",
            ifelse(x$flow_summary$net_flow_usd > 0, "+", ""),
            "$", format(round(x$flow_summary$net_flow_usd / 1e6, 2), big.mark = ","), "M\n", sep = "")
        cat("Flow Regime:", x$flow_summary$flow_regime, "\n\n")
      }

      cat("Flow by Vault:\n")
      flow_display <- x$vault_flows %>%
        dplyr::mutate(
          vault_display = paste0(vault_name, " (", flow_direction, ")"),
          tx_info = paste0(tx_count, " tx (", days_covered, " days)")
        ) %>%
        dplyr::select(
          Vault = vault_display,
          `Net Flow` = netflow_formatted,
          `% of TVL` = netflow_pct_formatted,
          Transactions = tx_info
        )
      print(flow_display)
    }

    #---------------------------------------------------------------------------
    # WHALE CONCENTRATION SECTION
    #---------------------------------------------------------------------------
    if (!is.null(x$whale_concentration)) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🐋 WHALE CONCENTRATION ANALYSIS\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")
      cat("Unique Whale Wallets:", x$whale_concentration$total_whale_wallets, "\n")
      cat("Mega Whales (>$10M):", x$whale_concentration$mega_whales, "\n")
      cat("Regular Whales ($1M-$10M):", x$whale_concentration$regular_whales, "\n")
      cat("Market Concentration (HHI):", format(round(x$whale_concentration$hhi, 1), big.mark = ","),
          "(", x$whale_concentration$hhi_interpretation, ")\n")
      cat("Top 5 Depositors Share:", round(x$whale_concentration$top_5_share, 1), "%\n")
    }

    #---------------------------------------------------------------------------
    # TOP VAULTS SECTION
    #---------------------------------------------------------------------------
    if (!is.null(x$top_vaults) && nrow(x$top_vaults) > 0) {
      cat("\n", paste(rep("━", 80), collapse = ""), "\n")
      cat("🏆 TOP BTC-EXPOSED VAULTS\n")
      cat(paste(rep("━", 80), collapse = ""), "\n")

      top_vaults_display <- x$top_vaults %>%
        dplyr::mutate(
          btc_exposure_pct = paste0(round(btc_exposure_pct, 1), "%")
        )
      print(top_vaults_display)
    }

    #---------------------------------------------------------------------------
    # TOP DEPOSITORS SECTION
    #---------------------------------------------------------------------------
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
