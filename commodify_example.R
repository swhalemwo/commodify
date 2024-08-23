API_KEY = "your-api-key"
FILE_EOD_FULL <- "/home/user/link/to/prices_eod_full.csv"
FILE_EOD_PRICEHIST <- "/home/link/to/prices.db"

## defining stocks to track
## ISINs are not needed for EODHD, but I added them anyways (should not be necessary)
dt_stock_ids <- list(
    list(isin = "LU1681048804", ticker = "AUM5", name = "SP500"),
    list(isin = "DE000A0H08J9", ticker = "EXH4", name = "STX600")
) %>% rbindlist

source("/path/to/commodify_funcs.R")
gd_eod(dt_stock_ids)
