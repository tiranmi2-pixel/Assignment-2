library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')
library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(readr)
res <- dbSendQuery(wrds, "
  select gvkey, datadate, fyear, emp
  from comp_na_daily_all.funda
  where fyear between '2020' and '2025'
    and exchg = '11'
    and datafmt = 'STD'
    and consol  = 'C'
    and indfmt  = 'INDL'
    and emp is not null
  order by gvkey, datadate
")

emp_nyse <- dbFetch(res, n = -1)
dbClearResult(res)

View(emp_nyse)
write_csv(emp_nyse, "emp_nyse.csv")
----------------------------------------------------
