# database update; get new data since last close into the db
# rm(list=ls())
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(NasdaqDataLink, Quandl, tidyverse, tidyquant, here, gptstudio, broom, 
       timetk, scales, gridExtra, grid, reshape2, RPostgres, DBI)

source(here::here("key.R"))
Quandl::Quandl.api_key(key)
NasdaqDataLink::NasdaqDataLink.api_key(key)

con <- dbConnect(RPostgres::Postgres())
# New equity and fund tables from scratch  
# Sharadar data https://data.nasdaq.com/databases/SFA
# download file, unzip, and name it SEP.csv, SFP.csv in the directory
getwd()
sep2 <- read_csv(here("SEP.csv"), col_names = TRUE)
# dbExecute(con, "DROP TABLE sep")
# dbWriteTable(con, "sep", sep, overwrite=TRUE)    # this line dogs it

# sfp <- read_csv(here("SFP.csv"), col_names = TRUE)
# dbExecute(con, "DROP TABLE sfp")
# dbWriteTable(con, "sfp", sfp, overwrite=TRUE)    # this line dogs it

# Function to update price tables
update_table <- function(con, table_name, datatable_name) {
  if (RPostgres::dbExistsTable(con, table_name, check.names = TRUE, 
                               row.names = FALSE)) {
    cutoff <- dbGetQuery(con, paste0("SELECT MAX(lastupdated) FROM ", table_name))[1, 1]
    print(paste("Last day of", table_name, "table data", cutoff))
  } else {
    warning(paste("Egad! Missing", table_name, "table! Most unfortunate!"))
    return()
  }
  cat("Getting new Quandl data. ")
  new_data <- Quandl.datatable(datatable_name, lastupdated.gt = cutoff,
                               paginate = TRUE)
  if (nrow(new_data) == 0) {
    warning(paste0("WTF? No data to update here!!! Skip rest of ", table_name))
    return()
  }
  new_closes <- new_data |>
    filter(date > cutoff)
  splits <- new_data |>
    filter(date <= cutoff)

  # check must be zero or problems, of an existential, catastrophic nature loom
  check <- nrow(splits) + nrow(new_closes) - nrow(new_data)
  if (!check == 0) warning("Egad! Fix the closes/splits/new_data problem")
  
  # add new closes and splits
  cat(nrow(new_closes), "closes. ")
  dbWriteTable(con, table_name, new_closes, append = TRUE, row.names = FALSE)
  cat(nrow(splits), "splits. ")
  dbExecute(con, 
            paste0("CREATE TEMPORARY TABLE temp_update (LIKE ", 
                   table_name, " INCLUDING CONSTRAINTS)"))
  dbWriteTable(con, "temp_update", splits, append = TRUE, row.names = FALSE)
  
  # Update existing columns in the main table
  dbExecute(con, paste0("
    UPDATE ", table_name, "
    SET open = temp_update.open,
        high = temp_update.high,
        low = temp_update.low,
        close = temp_update.close,
        volume = temp_update.volume,
        closeadj = temp_update.closeadj,
        lastupdated = temp_update.lastupdated
    FROM temp_update
    WHERE ", table_name, ".ticker = temp_update.ticker
      AND ", table_name, ".date = temp_update.date
  "))
  
  # Clean up temporary table
  dbExecute(con, "DROP TABLE temp_update")
  cat("Done.", "\n")
}

# SEP table update
update_table(con, "sep", 'SHARADAR/SEP')

# SFP table update
update_table(con, "sfp", 'SHARADAR/SFP')

# S&P500 table
########################
# Check if "sp500" table doesn't exist or if the maximum date is > a week old
if (!RPostgres::dbExistsTable(con, "sp500", check.names = TRUE, 
                              row.names = FALSE) ||
    (RPostgres::dbGetQuery(con, 
          "SELECT MAX(date) FROM sp500")$max <= (Sys.Date() - 5))) {
  # Download a new version of the "sp500" table
  cat("Updating S&P500 table.")
  SP500 <- Quandl.datatable('SHARADAR/SP500', paginate=TRUE) %>%
    filter(action == "current") %>%
    arrange() %>%
    select(ticker, name, date)
  dbWriteTable(con, "sp500", SP500, overwrite=TRUE)
} else {
  print("S&P500 is close enough")
}

# TICKERS table
########################
# load tickers; if no tickers table, make a new one
if (!RPostgres::dbExistsTable(con, "tickers", check.names = TRUE, 
                              row.names = FALSE)  ||
    (RPostgres::dbGetQuery(con,
           "SELECT MAX(lastupdated) FROM tickers")$max <= (Sys.Date() - 5))) {
  cat("Updating Tickers table.")
  tickers <- Quandl.datatable('SHARADAR/TICKERS', paginate = TRUE) %>%
    arrange()
  dbWriteTable(con, "tickers", tickers, overwrite = TRUE)
} else {
  print("Tickers close enough.")
} 
# dbListFields(con, "tickers")

