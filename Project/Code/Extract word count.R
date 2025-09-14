library(DBI)
library(dplyr)
library(lubridate)


res_link <- dbSendQuery(wrds, "
  select gvkey, cik
  from wrdssec.wciklink_gvkey
")
gvkey_cik <- dbFetch(res_link, n = -1)
dbClearResult(res_link)

# Add cik to your emp_nyse table using left_join (dplyr)
emp_nyse_cik <- emp_nyse %>%
  left_join(gvkey_cik, by = "gvkey")

emp_nyse_cik <- emp_nyse_cik %>%
  select(cik, gvkey, datadate, fyear, emp)

----------------------------------------------------------
  
# --- Get CIK list from my existing data ---
  cik_list <- emp_nyse_cik %>% 
  filter(!is.na(cik)) %>% 
  distinct(cik) %>% 
  pull(cik)

# Create IN clause string for SQL
cik_string <- paste0("('", paste(cik_list, collapse = "','"), "')")

# --- Extract yearly total word count from wrdssec_secsa.bow_filingsummary ---
sql_word_count <- paste0("
  select 
    fs.cik,
    extract(year from fs.fdate)::int as fyear,
    sum(fs.wcnt)::bigint as total_word_count,
    count(*)::int as filing_count
  from wrdssec_secsa.bow_filingsummary fs
  where fs.cik in ", cik_string, "
    and fs.fdate >= date '2020-01-01' 
    and fs.fdate <= date '2025-12-31'
    and fs.wcnt is not null
    and fs.wcnt > 0
  group by fs.cik, extract(year from fs.fdate)
  order by fs.cik, fyear
")

# Execute query
yearly_word_count <- dbGetQuery(wrds, sql_word_count)

# --- Join to your existing table ---
emp_nyse_cik_with_wcnt <- emp_nyse_cik %>%
  left_join(yearly_word_count, by = c("cik", "fyear")) %>%
  select(cik, gvkey, datadate, fyear, emp, total_word_count, filing_count)

# Display results
head(emp_nyse_cik_with_wcnt)

--------------------------------------------------------------------------------