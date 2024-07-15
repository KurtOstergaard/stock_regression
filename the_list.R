# make the list
#   BB version
watchlist_export <- function(tdf, ticker_lookup) {
  # Long ideas
  dflong <- tdf %>%
    filter(pctB > 0.95) %>%
    mutate(heading = "Long Ideas") %>%
    select(heading, ticker, pctB)
  
  dflong <- merge(dflong, ticker_lookup, by = "ticker", all.x = TRUE) %>%
    distinct(ticker, .keep_all = TRUE) %>%
    arrange(desc(pctB)) %>%
    select(heading, exchange, ticker)
  
  # Short ideas
  dfshort <- tdf %>%
    filter(pctB < 0.05) %>%
    mutate(heading = "Short Ideas") %>%
    select(heading, ticker, pctB)
  
  dfshort <- merge(dfshort, ticker_lookup, by = "ticker", all.x = TRUE) %>%
    distinct(ticker, .keep_all = TRUE) %>%
    arrange(pctB) %>%
    select(heading, exchange, ticker)
  
  # Combo export df
  dfexp <- bind_rows(dflong, dfshort)
  
  return(dfexp)
}

create_watchlist_file <- function(df, file_suffix) {
  # create the file path
  asofd <- as.character(asof)
  month_day <- substr(asofd, nchar(asofd) - 4, nchar(asofd))
  file_path <- paste0(month_day, " ", file_suffix)
  file_conn <- file(file_path, "w")
  prev_heading <- ""
  first_heading <- TRUE
  
  # Write the data to the file
  for (i in 1:nrow(df)) {
    heading <- df$heading[i]
    exchange <- df$exchange[i]
    ticker <- df$ticker[i]
    
    if (heading != prev_heading) {
      if (!first_heading) {
        writeLines("", file_conn)  # Write an empty line before the new header
      }
      writeLines(paste0("###", heading, ","), file_conn)
      prev_heading <- heading
      first_heading <- FALSE
    }
    
    # Write the exchange and ticker
    writeLines(paste0(exchange, ":", ticker, ","), file_conn)
  }
  close(file_conn)  # Close the file connection
  cat("Yo!", file_path, "File created successfully, you badass!")
}

# Generate the export dataframes
dfexp <- watchlist_export(tdf, ticker_lookup)
dfexpsfp <- watchlist_export(tdfsfp, ticker_lookup)

# Create the Watchlist files
create_watchlist_file(dfexp, "equities.txt")
create_watchlist_file(dfexpsfp, "funds.txt")


create_watchlist_file(dflong, "zarbon.txt")




####  Rolling Window version

watchlist_export <- function(tdf, ticker_lookup) {
  # Long ideas
  dflong <- tdfup %>%
    mutate(heading = "Long Ideas") %>%
    select(heading, ticker, rank20)
  
  dflong <- merge(dflong, ticker_lookup, by = "ticker", all.x = TRUE) %>%
    distinct(ticker, .keep_all = TRUE) %>%
    arrange(desc(rank20)) %>%
    select(heading, exchange, ticker)
  
  # Short ideas
  dfshort <- tdfdn %>%
    mutate(heading = "Short Ideas") %>%
    select(heading, ticker, rank20)
  
  dfshort <- merge(dfshort, ticker_lookup, by = "ticker", all.x = TRUE) %>%
    distinct(ticker, .keep_all = TRUE) %>%
    arrange(rank20) %>%
    select(heading, exchange, ticker)
  
  # Combo export df
  dfexp <- bind_rows(dflong, dfshort)
  
  return(dfexp)
}


# Generate the export dataframes
dfexp <- watchlist_export(tdf, ticker_lookup)
dfexpsfp <- watchlist_export(tdfsfp, ticker_lookup)

# Create the Watchlist files
create_watchlist_file(dfexp, "equityz.txt")
create_watchlist_file(dfexpsfp, "fundz.txt")






