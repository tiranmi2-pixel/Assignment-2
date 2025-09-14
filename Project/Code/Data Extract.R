

library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')
con <- wrds

install.packages(c("RPostgres", "DBI", "tidyverse", "data.table", 
                   "lubridate", "stringr", "text2vec", "tm"))
install.packages("tidytext")

library(DBI)
library(RPostgres)
library(dplyr)
library(stringr)
library(tidytext)
library(lubridate)



# Query 8-K filings for January 2024
query <- "
SELECT 
    f.cik,
    f.accession_number,
    f.filed_date,
    f.form_type,
    f.item_list,
    ft.text as filing_text,
    c.company_name
FROM sec.filings f
LEFT JOIN sec.filing_text ft ON f.accession_number = ft.accession_number
LEFT JOIN sec.company_info c ON f.cik = c.cik
WHERE f.form_type = '8-K' 
    AND f.filed_date BETWEEN '2024-01-01' AND '2024-01-31'
    AND (f.item_list LIKE '%2.05%' OR f.item_list LIKE '%7.01%')
    AND ft.text IS NOT NULL
"

# Execute query
sec_8k_data <- dbGetQuery(con, query)

# Define keyword sets for classification
layoff_keywords <- c(
  "layoff", "lay off", "workforce reduction", "workforce restructuring",
  "rightsizing", "downsizing", "staff reduction", "job cut", "job cuts",
  "elimination of positions", "redundancy", "separation", "termination"
)

ai_keywords <- c(
  "artificial intelligence", "AI", "machine learning", "automation",
  "algorithmic", "robotic process automation", "RPA", "digital transformation",
  "AI-driven", "AI-powered", "intelligent automation", "cognitive computing",
  "neural network", "deep learning", "natural language processing"
)

traditional_keywords <- c(
  "market conditions", "economic pressures", "declining demand",
  "cost reduction", "budget constraints", "competitive pressure",
  "operational challenges", "restructuring due to", "financial performance"
)

# Function to identify layoff announcements
identify_layoffs <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  
  text_lower <- tolower(text)
  layoff_present <- any(sapply(layoff_keywords, function(keyword) {
    str_detect(text_lower, fixed(keyword, ignore_case = TRUE))
  }))
  
  return(layoff_present)
}

# Function to classify AI-driven vs traditional layoffs
classify_layoff_type <- function(text) {
  if (is.na(text) || text == "") return("unknown")
  
  text_lower <- tolower(text)
  
  # Check for AI keywords
  ai_score <- sum(sapply(ai_keywords, function(keyword) {
    str_count(text_lower, fixed(keyword, ignore_case = TRUE))
  }))
  
  # Check for traditional keywords
  traditional_score <- sum(sapply(traditional_keywords, function(keyword) {
    str_count(text_lower, fixed(keyword, ignore_case = TRUE))
  }))
  
  # Classification logic
  if (ai_score > 0 && ai_score >= traditional_score) {
    return("ai_driven")
  } else if (traditional_score > 0) {
    return("traditional")
  } else {
    return("unclear")
  }
}

# Function to extract key phrases for verification
extract_key_phrases <- function(text, n_words = 10) {
  if (is.na(text) || text == "") return("")
  
  # Find sentences containing layoff keywords
  sentences <- str_split(text, "\\.")[[1]]
  relevant_sentences <- sentences[sapply(sentences, function(sent) {
    any(sapply(layoff_keywords, function(kw) str_detect(tolower(sent), fixed(kw))))
  })]
  
  if (length(relevant_sentences) > 0) {
    return(paste(str_trim(relevant_sentences[1:min(2, length(relevant_sentences))]), collapse = ". "))
  } else {
    return("")
  }
}

# Apply classification functions
sec_8k_data <- sec_8k_data %>%
  mutate(
    is_layoff = sapply(filing_text, identify_layoffs),
    layoff_type = sapply(filing_text, classify_layoff_type),
    key_phrases = sapply(filing_text, extract_key_phrases)
  ) %>%
  filter(is_layoff == TRUE)

# Count results
results <- sec_8k_data %>%
  group_by(layoff_type) %>%
  summarise(
    count = n(),
    companies = paste(company_name, collapse = "; ")
  )

# Display results
print("SEC 8-K Layoff Filings Analysis - January 2024")
print("=" * 50)
print(results)

# Detailed breakdown
ai_driven_count <- sum(sec_8k_data$layoff_type == "ai_driven", na.rm = TRUE)
traditional_count <- sum(sec_8k_data$layoff_type == "traditional", na.rm = TRUE)
unclear_count <- sum(sec_8k_data$layoff_type == "unclear", na.rm = TRUE)

cat("\nSUMMARY:\n")
cat("AI-driven layoff filings:", ai_driven_count, "\n")
cat("Traditional layoff filings:", traditional_count, "\n")
cat("Unclear classification:", unclear_count, "\n")
cat("Total layoff filings:", nrow(sec_8k_data), "\n")

# Export detailed results
write.csv(sec_8k_data, "sec_8k_layoffs_jan_2024.csv", row.names = FALSE)

# Show sample of AI-driven filings for verification
if (ai_driven_count > 0) {
  cat("\nSample AI-driven layoff announcements:\n")
  ai_samples <- sec_8k_data[sec_8k_data$layoff_type == "ai_driven", ] %>%
    select(company_name, filed_date, key_phrases) %>%
    head(3)
  
  for (i in 1:nrow(ai_samples)) {
    cat("\n", i, ". ", ai_samples$company_name[i], " (", ai_samples$filed_date[i], "):\n")
    cat("   ", str_wrap(ai_samples$key_phrases[i], width = 70), "\n")
  }
}

# Disconnect from WRDS
dbDisconnect(con)

# Additional validation function
validate_classifications <- function(data, sample_size = 10) {
  if (nrow(data) < sample_size) sample_size <- nrow(data)
  
  sample_data <- data %>% sample_n(sample_size)
  
  cat("\nValidation Sample (review manually):\n")
  cat("=" * 40, "\n")
  
  for (i in 1:nrow(sample_data)) {
    cat("Company:", sample_data$company_name[i], "\n")
    cat("Classification:", sample_data$layoff_type[i], "\n")
    cat("Key Phrases:", str_wrap(sample_data$key_phrases[i], width = 60), "\n")
    cat("-" * 40, "\n")
  }
}

# Run validation
if (nrow(sec_8k_data) > 0) {
  validate_classifications(sec_8k_data)
}
