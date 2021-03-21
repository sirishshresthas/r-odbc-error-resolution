library(tidyverse)
library(odbc)


##' Function to create connection to the data. This is set as a function so that multiple concurrent requests to the database can be made within a process
##' @param driver database driver name
##' @param host database host (server) name
##' @param dbname database name
##' @param uid username to connect to the database
##' @param pwd password to connect to the database
##' @return connection string that establishes the connection to the database
##' @examples
##' conn <- get_sql_conn()

get_sql_conn <- function(){
  
  
  sqlConfig <- config::get('sqldb_config')
  
  conn <- dbConnect(odbc::odbc(), 
                    Driver = sqlConfig$Driver, 
                    Server = sqlConfig$Server,
                    Port = sqlConfig$Port, 
                    Database = sqlConfig$Database,
                    Encrypt = sqlConfig$Encrypt,
                    UID = sqlConfig$UID,
                    PWD = sqlConfig$PWD)
  return(conn)

}


##' Get all columns and their data types available in the table 
##' @param table_name name of the table to get the column names and data types
##' @return data frame with column names and data type
##'
get_column_names <- function(table_name){
  
  ## create a connection to the database
  conn <- get_sql_conn()
  
  ## fetch the column names for the given table_name schema
  columns <- dbGetQuery(conn, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS ", 
                                     "WHERE TABLE_NAME = '", table_name, "'"))
  
  return (columns)
}


##' ODBC connection has a restrictions where any *long data*, defined as any character or binary data over a certain size - usually 255 characters. Since these data can be too long to store in memory, such as long text documents or bitmaps, it is by design retrieved from the driver in parts after the other data in the row has been fetched. Thus, the ODBC connection expects that the larger data types be called at last otherwise it'll complain. The application is responsible for putting the long data together. Ideally, column names should be stated during query, but it is not always feasible. 
##' Therefore, this function resolves this issue by identifying all columns from the schema, ordering them according to the size of the data. Once the columns are available, it then builds the query to be used to fetch the data from the database server. 
##' @param table_name name of the table to create the query
##' @return The structured query with the column names
##' @examples 
##' query <- create_query_with_column_names('table_name')
##' print(query)

create_query_with_column_names <- function(table_name){
  
  if(missing(table_name)){
    stop("Table name cannot be null.")
  }
  
  columns <- get_column_names(table_name)
  
  
  ## order column names in descending order for the maximum length and ascending order of it's ordinal position number. Any text data will have the length size set to -1.
  ## this will generate either the maximum length allowed for the column, or will have -1 or NULL which indicates
  ## that the column doesn't have a "character maximum length". This is true for data types such as 
  ## varchar(max), nvarchar(max), text, int etc. 
  columns <- columns %>% arrange(desc(CHARACTER_MAXIMUM_LENGTH), ORDINAL_POSITION) 
  
  ## extract column names only
  column_names <- columns$COLUMN_NAME
  
  ## wrap the column names in square bracket for querying
  column_names <- str_c("[", column_names, "]", collapse = ",")
  
  ## Create a query to retrieve data from the database
  query <- paste0(toString(sprintf("SELECT %s FROM ", column_names)), table_name)
  
  return(query)
  
}



##' Fetch the data from the database. If query string is not provided, it'll generate it's own query which will retrieve all columns from the database
##' @param query (optional) Query to retrieve the data from the database
##' @param table_name Name of the table to retrieve the data from
##' @return data frame of the data retrieved from the database
##' @examples
##' run_sql_query(table_name='table_name')
##' run_sql_query(query= "Select [StartDate],[EndDate],[Status] from 'table_name'", table_name = 'table_name')

run_sql_query <- function(query=NULL, table_name=NULL){
  
  if(missing(table_name)){
    stop("Table name is missing.")
  }
  
  if(missing(query)){
    query <- str_c("SELECT * FROM ", table_name)
  }
  
  conn <- get_sql_conn()
  
  result <- tryCatch(
    ## attempt to run the query
    dbGetQuery(conn, query), 
    error = function(e){
      message(e)
      message("Attempting to run by sorting the columns")
      
      new_query <- create_query_with_column_names(table_name)
      
      data <- dbGetQuery(conn, new_query)
      return(data)
    }
    
  )
  
}





