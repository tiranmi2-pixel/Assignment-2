

library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')

install.packages(c("RPostgres", "DBI", "tidyverse", "data.table", 
                   "lubridate", "stringr", "text2vec", "tm"))

library(RPostgres)
library(DBI)
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

# Query 8-K filings with Item 2.05 (restructuring/layoff related)
item_205_query <- "
SELECT f.cik, f.gvkey, f.cusip, f.ticker, f.fname, f.fdate, f.rdate,
       f.coname, f.sic, f.secatime, i.item, i.itemdesc
FROM wrdssec.wrds_forms f
INNER JOIN wrdssec.wrds_8k_items i ON f.fname = i.fname
WHERE i.item = '2.05'
  AND f.fdate >= '2020-01-01'
  AND f.fdate <= '2025-12-31'
  AND f.ticker IS NOT NULL
ORDER BY f.fdate DESC
"

item_205_filings <- dbGetQuery(wrds, item_205_query)

# Also query other 8-K items that might contain layoff information
other_items_query <- "
SELECT f.cik, f.gvkey, f.cusip, f.ticker, f.fname, f.fdate, f.rdate,
       f.coname, f.sic, f.secatime, i.item, i.itemdesc
FROM wrdssec.wrds_forms f
INNER JOIN wrdssec.wrds_8k_items i ON f.fname = i.fname
WHERE i.item IN ('1.01', '1.02', '7.01', '8.01')  -- Other items that may contain layoffs
  AND f.fdate >= '2020-01-01'
  AND f.fdate <= '2025-12-31'
  AND f.ticker IS NOT NULL
ORDER BY f.fdate DESC
"

other_items_filings <- dbGetQuery(wrds, other_items_query)
