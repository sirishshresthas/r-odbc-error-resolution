source("r-sql-odbc-error-fix.R")

## USAGE
table_name <- 'sql_table' 

## will create query inside the function
rs <- run_sql_query(table_name=table_name)

rs
