API_KEY = "your-api-key"
FILE_EOD_FULL <- "/home/user/link/to/prices_eod_full.csv"
FILE_EOD_PRICEHIST <- "/home/link/to/prices.db"

## defining stocks to track
## ISINs are not needed for EODHD, but I added them anyways (should not be necessary)
l_stock_ids <- list(
    list(isin = "LU1681048804", ticker = "AUM5", name = "SP500"),
    list(isin = "DE000A0H08J9", ticker = "EXH4", name = "STX600")
)

df_stock_ids <- do.call(rbind, lapply(l_stock_ids, as.data.frame))

source("/path/to/commodify_funcs.R")
gd_eod(df_stock_ids)
