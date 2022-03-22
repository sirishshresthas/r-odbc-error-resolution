source("r-sql-odbc-error-fix.R")

## USAGE
table_name <- 'DSI_360BDView_Items' 

## will create query inside the function
# rs <- run_sql_query(table_name=table_name)
rs <- run_sql_query(query = sql("SELECT Top 10 * FROM DSI_360BDView_Items"))