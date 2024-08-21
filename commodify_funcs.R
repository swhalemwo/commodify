library(httr)
library(jsonlite)
library(purrr)
library(data.table)


gd_eod_ff <- function(FILE_EOD_FULL) {
    #' get the full EOD data from the file,
    #' if it doesn't exist, return empty data.table

    if (file.exists(FILE_EOD_FULL)) {
        dt_eod_full <- fread(FILE_EOD_FULL)
    } else {
        dt_eod_full <- data.table(date = character(), open = numeric(),
                                  high = numeric(), low = numeric(), close = numeric(), adjusted_closed = numeric(),
                                  volume = integer(), ticker = character())
    }

    return(dt_eod_full)
    
}

gd_eod_single <- function(ticker) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}
    1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;
    #' get the EOD (end-of-day) data for a stock/ETF
    #' for now uses XETRA
    #' @param ticker the symbol to identify the stock/etf
    

    url <- sprintf('https://eodhd.com/api/eod/%s.XETRA?api_token=%s&fmt=json', ticker, API_KEY)
    response <- GET(url)

    if (http_type(response) == "application/json") {
        content <- content(response, "text", encoding = "UTF-8")
        ## cat(content)
        dt_eod <- fromJSON(content) %>% adt %>%
            .[, ticker := ticker] %>%
            .[, date := as.IDate(date)]

        return(dt_eod)
        
    } else {
        cat("Error while receiving data\n")
    }

}



gd_eod <- function(dt_stock_ids) {
    if (as.character(match.call()[[1]]) %in% fstd){browser()}

    #' get existing data from file
    dt_eod_full <- gd_eod_ff(FILE_EOD_FULL)
    
    ## get new data from web
    dt_eod <- map(dt_stock_ids$ticker, ~gd_eod_single(ticker = .x)) %>% rbindlist

    ## remove entries that are already there
    dt_eod_tofile <- dt_eod[!dt_eod_full, on=.(date, ticker)]

    ## write new entries to file
    fwrite(dt_eod_tofile, FILE_EOD_FULL, append = TRUE)

    ## generate prices.db from FILE_EOD_FULL
    dt_pricehist <- gd_eod_ff(FILE_EOD_FULL) %>%
        merge(dt_stock_ids, by = "ticker") %>%
        .[, .(ledger_str = sprintf("P %s %s EUR %.2f",
                                   format(date, "%Y/%m/%d"),
                                   fifelse(grepl("[0-9]", name), sprintf('"%s"', name), name),
                                   close))]

    ## write to prices.db
    fwrite(dt_pricehist, FILE_EOD_PRICEHIST, append = FALSE, quote = FALSE, col.names = FALSE)
    

}


