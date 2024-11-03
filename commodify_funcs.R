library(httr)
library(jsonlite)
library(purrr)
library(data.table)
library(magrittr)
library(ledger)    

gd_eod_ff <- function(FILE_EOD_FULL) {
    #' get the full EOD data from the file,
    #' if it doesn't exist, return empty data.table

    if (file.exists(FILE_EOD_FULL)) {
        dt_eod_prep <- fread(FILE_EOD_FULL)
    } else {
        dt_eod_prep <- data.table(date = character(), open = numeric(),
                                  high = numeric(), low = numeric(), close = numeric(), adjusted_closed = numeric(),
                                  volume = integer(), ticker = character())
    }

    return(dt_eod_prep)
    
}

gd_pricehist_manual <- function(dt_stock_ids) {
    #' generate price db entries for stockmarket entries (not in API)
    
    adt <- as.data.table
    dt_ledger <- register(FILE_LEDGER) %>% adt

    ## use the fact that transactions with stockmarket all have the same 4-line structure to generate transaction id
    dt_cmdt_prep <- dt_ledger[account == "stockmarket"] %>%
        .[, .(date, amount, commodity, kappa = "j", transaction_id = rep(1:(.N/2), each = 2))] %>%
        dcast(transaction_id + date ~ commodity, value.var = c("amount")) %>% # to wide
        # get commodity counts and names in one column
        .[, `:=`(cmdt_name = names(.SD)[!is.na(.SD)], cmdt_cnt = na.omit(t((.SD)))[1]), transaction_id,
          .SDcols = setdiff(names(.), c("transaction_id", "date", "EUR"))] %>%         
        .[, price := (EUR/cmdt_cnt)*-1]
        
    ## prepare manual price entries for price db
    dt_ledger_str_mnl <- dt_cmdt_prep[, .(price = min(price)), .(date, cmdt_name = gsub("\\\\", "", cmdt_name))] %>%
        ## filter out those that are already covered by api 
        .[!dt_stock_ids, on = .(cmdt_name = name)] %>% 
        .[, .(ledger_str = sprintf("P %s %s EUR %.2f", date,
                                   fifelse(grepl("[0-9]", cmdt_name), sprintf('"%s"', cmdt_name), cmdt_name),
                                   price))]

    return(dt_ledger_str_mnl)
}


gd_eod_single <- function(ticker) {

    #' get the EOD (end-of-day) data for a stock/ETF via EODHD
    #' for now uses XETRA
    #' @param ticker the symbol to identify the stock/etf
    

    url <- sprintf('https://eodhd.com/api/eod/%s.XETRA?api_token=%s&fmt=json', ticker, API_KEY)
    response <- GET(url)

    if (http_type(response) == "application/json") {
        content <- content(response, "text", encoding = "UTF-8")
        ## cat(content)
        dt_eod <- fromJSON(content) %>% as.data.table %>%
            .[, ticker := ticker] %>%
            .[, date := as.IDate(date)]

        return(dt_eod)
        
    } else {
        cat("Error while receiving data\n")
    }

}


## gd_yf_single <- function(ticker) {
##     if (as.character(match.call()[[1]]) %in% fstd){browser()}
##     #' get the EOD data for a stock via yahoo finance

##     dt_yf <- getSymbols(ticker, auto.assign = F) %>% adt %>% 
##         setnames(old = names(.), new = gsub(paste0(ticker, "."), "", names(.))) %>%
##         .[, .(date = as.Date(index), open = Open, high = High, low = Low, close= Close, adjusted_close = Adjusted,
##               volume = Volume, ticker = ticker)]

##     return(dt_yf)
## }

gd_eod <- function(df_stock_ids) {
    
        
    ## download and parse stock data 

    dt_stock_ids <- as.data.table(df_stock_ids)
    
    ## get existing data from file
    dt_eod_full <- gd_eod_ff(FILE_EOD_FULL)
    
    ## get new data from web
    dt_eod <- map(dt_stock_ids[src == 'eodhd', ticker], ~gd_eod_single(ticker = .x)) %>% rbindlist

    ## dt_yf <- map(dt_stock_ids[src == 'yf', ticker], ~gd_yf_single(ticker = .x)) %>% rbindlist
    ## ## dt_stock_ids[src == 'yf', ticker]
    ## gd_yf_single("QSL5.DE")
    ## gd_yf_single("XS2771643025.SG")
    
    
    ## remove entries that are already there
    dt_eod_tofile <- dt_eod[!dt_eod_full, on=.(date, ticker)]

    ## write new entries to file
    fwrite(dt_eod_tofile, FILE_EOD_FULL, append = TRUE)

    ## generate prices.db from FILE_EOD_FULL
    dt_pricehist_api <- gd_eod_ff(FILE_EOD_FULL) %>%
        merge(dt_stock_ids, by = "ticker") %>%
        ## .[src == 'yf'] %>% 
        .[, .(ledger_str = sprintf("P %s %s EUR %.2f",
                                   format(date, "%Y/%m/%d"),
                                   fifelse(grepl("[0-9]", name), sprintf('"%s"', name), name),
                                   close))]


    dt_pricehist_manual <- gd_pricehist_manual(dt_stock_ids)
    dt_pricehist <- rbind(dt_pricehist_api, dt_pricehist_manual)

    ## write to prices.db
    fwrite(dt_pricehist, FILE_EOD_PRICEHIST, append = FALSE, quote = FALSE, col.names = FALSE)

    

}


