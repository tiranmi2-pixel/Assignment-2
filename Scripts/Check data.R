# Check available schemas
schemas <- dbGetQuery(wrds, "SELECT schema_name FROM information_schema.schemata WHERE schema_name LIKE '%sec%'")
print(schemas)

# List tables in the SEC-related schemas
sec_tables <- dbGetQuery(wrds, "
  SELECT table_schema, table_name 
  FROM information_schema.tables 
  WHERE table_schema LIKE '%sec%' 
  ORDER BY table_schema, table_name
")
print(sec_tables)

----------
# Check the actual column structure of secsamp.wrds_forms
  forms_columns <- dbGetQuery(wrds, "
  SELECT column_name, data_type 
  FROM information_schema.columns 
  WHERE table_schema = 'secsamp' 
    AND table_name = 'wrds_forms'
  ORDER BY ordinal_position
")
print("Available columns in secsamp.wrds_forms:")
print(forms_columns)

# Check the items8k table structure
items_columns <- dbGetQuery(wrds, "
  SELECT column_name, data_type 
  FROM information_schema.columns 
  WHERE table_schema = 'secsamp' 
    AND table_name = 'items8k'
  ORDER BY ordinal_position
")
print("Available columns in secsamp.items8k:")
print(items_columns)
