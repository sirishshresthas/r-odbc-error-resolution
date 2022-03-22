
construct_query <- function(column_names=NULL, table_name=NULL, sql_clause=NULL){
  
  ## wrap the column names in square bracket for querying
  column_names <- str_c("[", column_names, "]", collapse = ",")
  
  if(!is.null(sql_clause) || !missing(sql_clause)){
    
    column_names <- paste0(sql_clause, " ", column_names)
    
  }
  
  ## Create a query to retrieve data from the database
  query <- paste0(toString(sprintf("SELECT %s FROM ", column_names)), table_name)
  
  return(sql(query))
}


##' Get all columns and their data types available in the table 
##' @param table_name name of the table to get the column names and data types
##' @return data frame with column names and data type
##'
get_column_names_for_table <- function(conn=NULL, table_name=NULL){
  
  if(missing(conn)){
    stop("There is no connection to the database. Please connect to the database and try again.")
  }
  
  if(missing(table_name)){
    stop("Table name must be provided to fetch the column names from the database.")
  }
  
  ## fetch the column names for the given table_name schema
  columns <- dbGetQuery(conn, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS ", 
                                     "WHERE TABLE_NAME = '", table_name, "'"))
  
  columns <- columns %>% mutate(CHARACTER_MAXIMUM_LENGTH = ifelse(is.na(CHARACTER_MAXIMUM_LENGTH), 0, CHARACTER_MAXIMUM_LENGTH))
  
  ## order column names in descending order for the maximum length and ascending order of it's ordinal position number. Any text data will have the length size set to -1.
  ## this will generate either the maximum length allowed for the column, or will have -1 or NULL which indicates
  ## that the column doesn't have a "character maximum length". This is true for data types such as 
  ## varchar(max), nvarchar(max), text, int etc. 
  columns <- columns %>% arrange(desc(CHARACTER_MAXIMUM_LENGTH), ORDINAL_POSITION) 
  
  return (columns)
  
}


##' Check if the table name exists in the database.
##' @keywords internal
##' @param conn The connection to the database
##' @param table_name (optional) The table name for fetching the data from the database. 
##' @return The Boolean indicating whether the table name exists or not
table_exists <- function(conn=NULL, table_name=NULL){
  
  query <- paste0("SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = '", table_name, "'")

  rows <- dbGetQuery(conn, query)
  
  
  if(nrow(rows) > 0){
    return(TRUE)
  } 
  
  return(FALSE)
    
}



get_query_with_rearranged_columns <- function(conn=NULL, table_name=NULL){
  
  columns <- get_column_names_for_table(conn=conn, table_name=table_name)
  
  query <- construct_query(column_names=columns$COLUMN_NAME, table_name=table_name) 
  
  return(query)
}




##' ODBC connection has a restrictions where any *long data*, defined as any character or binary data over a certain size - usually 255 characters. Since these data can be too long to store in memory, such as long text documents or bitmaps, it is by design retrieved from the driver in parts after the other data in the row has been fetched. Thus, the ODBC connection expects that the larger data types be called at last otherwise it'll complain. The application is responsible for putting the long data together. Ideally, column names should be stated during query, but it is not always feasible. 
##' Therefore, this function resolves this issue by identifying all columns from the schema, ordering them according to the size of the data. Once the columns are available, it then builds the query to be used to fetch the data from the database server. 
##' @keywords internal
##' @param conn Connection to the database
##' @param query Query to run on the database
##' @param table_name Name of the table to create the query
##' @return string: The structured query with the column names

create_query_with_column_names <- function(conn=NULL, query=NULL, table_name=NULL){
  
  if(missing(query) || missing(table_name)){
    stop("Either the table name in the database or the query to pull the data from the database is required. Neither are provided.")
  }
  
  if(!table_exists(conn=conn, table_name=table_name)){
    stop("The table name does not exist in the database. Please check the name and try again.")
  }
  
  
  ## if only the table name is provided:
  if(!missing(table_name)){  
    
    query <- get_query_with_rearranged_columns(conn=conn, table_name=table_name)

  }
  
  
  # 
  # 
  # remaining_query_string <- ""
  # 
  # ## extract the columns from the actual SQL statement
  # if(!missing(query) && str_detect(tolower(query), 'select')){
  #   
  #   print(str_c("The query consists of SQL Statement - ", query))
  #   
  #   print("Removing brackets if exist")
  #   query <- query %>% str_replace_all(., "\\[|\\]", "") %>% trimws()
  #   
  #   
  #   t_name <- str_extract(string=tolower(query), pattern="(?<=from)\\s\\w+") %>% trimws()
  #   cat("Identified the table name - ", t_name, "\n")
  #   
  #   ## to add any other criteria back in the query, extract the rest of the statements
  #   remaining_query_string <- str_extract(string=tolower(query), 
  #                                         pattern=paste0("(?<=",t_name,")(.*)")) %>%
  #     trimws()
  #   
  #   print("Now fetching column names for the table")
  #   columns <- get_column_names_for_table(t_name)
  #   
  #   if(str_detect(query, "\\s\\*\\s")){
  #     
  #     print("Looks like the SQL statement is attempting to pull all columns..")
  #     sql_clause <- str_extract(string=tolower(query), 
  #                               pattern="(?<=select)(.*)(?=\\*)") %>% trimws()
  #     
  #     ## extract column names only
  #     column_names <- columns$COLUMN_NAME
  #     query <- construct_query(column_names, t_name, sql_clause) 
  #     cat("Proper query generated: ", query, "\n")
  #     
  #   } else {
  #     
  #     print("Extracting column names from the Query")
  #     column_names_list <- extract_column_names_from_query(query)
  #     
  #     ## columns from get_column_names_for_table contains columns in proper order for ODBC. 
  #     ## From this list, we need to select the columns from column_names_list
  #     
  #     columns_names <- columns$COLUMN_NAME[tolower(columns$COLUMN_NAME %in% column_names_list$column_names)]
  #     query <- construct_query(column_names, t_name, column_names_list$sql_clause) 
  #     
  #   }
  #   
  # }
  # 
  # 
  # 
  # query <- paste0(query, remaining_query_string, sep=" ")
  # message("Query: '", query, "'")
  
  return(query)
  
}


extract_column_names_from_query <- function(query = NULL){
  
  columns_from_query <- str_extract(string=tolower(query), 
                                    pattern="(?<=select)(.*)(?=from)") 
  
  columns_from_query_list <- columns_from_query %>% strsplit(., ',') %>% unlist()
  
  column_with_sql_clause <- columns_from_query_list[1] %>% strsplit(., " ") %>% unlist()
  
  if(length(column_with_sql_clause) > 1){
    ## The query consists of SQL clause like TOP, DISTINCT, COUNT, etc.
    
    first_column_name <- column_with_sql_clause[length(column_with_sql_clause)]
    sql_clause <- str_c(column_with_sql_clause[-length(column_with_sql_clause)], collapse=" ")
    
    column_names <- c(first_column_name, columns_from_query_list[-1])
    
    return(list(column_names, sql_clause), set_names("column_names", "sql_clause"))
    
  } else {
    return(list(columns_from_query_list), set_names("column_names"))
  }
  
}
