

##' Fetch the data from the database. If query string is not provided, it'll generate it's own query which will retrieve all columns from the database
##' @param query (optional) Query to retrieve the data from the database
##' @param table_name Name of the table to retrieve the data from
##' @return data frame of the data retrieved from the database
##' @examples
##' run_sql_query(table_name='table_name')
##' run_sql_query(query= "Select [StartDate],[EndDate],[Status] from 'table_name'", table_name = 'table_name')

run_sql_query <- function(query=NULL, table_name=NULL, 
                          lazy_loading=TRUE, rearrange_columns=FALSE){
  
  message("Lazy loading is ", lazy_loading, ".")
  
  if(!lazy_loading){
    message("This may take several minutes to fetch the data from the server.")
  }
  
  if(missing(table_name) && missing(query)){
    stop("Table name and query are both missing. You must either provide the table name or the query to fetch the data.")
  }
  
  
  conn <- init_connection()
  
  
  ## process the sql statement
  
  if(!missing(table_name)){
    ## create query
    j <- create_query_with_column_names(conn=conn, query=query,table_name=table_name)

  } else {
    ## manage query
  }
  
  
  
  
  ## end sql statement processing
  
  
  
  
  # count total rows in the table
  # c <- get_record_count(data, query)
  
  # print("Fetch data from the database")
  # 
  # if(missing(query)){
  #   data <- fetch_data(conn=conn, table_name=table_name, lazy_loading=lazy_loading, rearrange_columns=rearrange_columns)
  #   
  # } else {
  #   cat("I have query instead of table name: ", query)
  #   data <- fetch_data(conn=conn, query=sql(query), lazy_loading=lazy_loading, rearrange_columns=rearrange_columns)
  # }
  # 
  #   
  # return(data)
  
}



##' Fetch the data from the database
##' @keywords internal
##' @param conn connection to the database
##' @param query SQL query or table name to fetch the data from the database
##' @param lazy_loading (optional) boolean value to either load the data on the memory or from the database. Default is TRUE

fetch_data <- function(conn=NULL, query=NULL, table_name=NULL, lazy_loading=TRUE, rearrange_columns=FALSE){
  
  print("Connection and data pointer is setup")
  
  if(missing(table_name) && missing(query)){
    stop("Query and table name are both missing.")
  }
  
  if(!rearrange_columns){
    message("Attempting to read the data as is. If you encounter an error, try fetching the data with setting `rearrange_columns` to TRUE.")
    data <- tbl(conn, query)

    return(data)

  } else {
    
    start_time <- Sys.time()
    print("Attempting to create query with re-arraned column names")
    new_query <- create_query_with_column_names(query=query) %>%  str_replace_all(.,"\\s+", "")

    print("Attempting to fetch the data with the new query")
    cat("New query: '", new_query, "'")
    data <- tbl(conn, new_query)

    end_time <- Sys.time()


    show_query_runtime(total_time=end_time-start_time, caller="fetch data without lazy loading.")

    return(data)
  }
  # 
  # 
  # result <- tryCatch(
  #     
  #     data <- tbl(conn, query),
  #     
  #     error = function(e){
  #       
  #       print("Error encountered: ", e)
  #       print("Attempting to run by sorting the columns")
  # 
  #       ##' ODBC connection has a restrictions where any long data, defined as
  #       ##' any character or binary data over a certain size - usually 255 chars.
  #       ##' Since these data can be too long to store in memory, such as long text
  #       ##' documents or bitmaps, it is by design retrieved from the driver in parts
  #       ##' after the other data in the row has been fetched. Thus, the ODBC
  #       ##' connection expects that the larger data types be called at last
  #       ##' otherwise it'll error out. The application is responsible for putting
  #       ##' the long data together. Ideally, column names should be stated during
  #       ##' query, but it is not always feasible.
  #       ##'
  #       ##' Therefore, this function resolves this issue by identifying all columns
  #       ##' from the schema, ordering them according to the size of the data.
  #       ##' Once the columns are available, it then builds the query to be used to
  #       ##' fetch the data from the database server.
  # 
  #       print("Attempting to create query with re-arraned column names")
  #       new_query <- create_query_with_column_names(query)
  # 
  #       print("Attempting to fetch the data with the new query")
  #       data <- tbl(conn, new_query)
  # 
  #       end_time <- Sys.time()
  # 
  # 
  #       show_query_runtime(total_time=end_time-start_time, caller="fetch data without lazy loading.")
  # 
  #     }
  #   )
  # 
  # return(result)
  
}


##' @keywords internal
##' Get the total record count from the database table
##' @param data a data source to fetch the data
##' @return total row count from the database table
##' @example
##' get_table_count(data = NULL, table_name = NULL)
get_record_count <- function(data = NULL, table_name = NULL){
  start_time <- Sys.time()
  
  if(missing(data)){
    stop("The database connection is missing.")
  }
  
  if(missing(table_name)){
    stop("The table name is missing. I need to know which table to count the records for.")
  }
  
  nrows <- data %>% dplyr::tally() %>% dplyr::collect()
  
  end_time <- Sys.time()
  show_query_runtime(total_time = end_time-start_time, caller="count records.")
  
  
  message("There are ", nrows, " rows in ", table_name, ". ")
  
  
  return(nrows)
  
}


##' Display total query run time for the request. 
##' @keywords internal
##' @param total_time total time to run the query. It is end_time - start_time
##' @param caller parameter to identify which function called the method.
##' This is optional; will only append the text to the end of the message
##' @return string
##' @examples
##' start_time <- Sys.time()
##' Sys.sleep(60)
##' end_time <- Sys.time()
##' show_query_runtime(total_time=end_time-start_time, caller="sleep time")
show_query_runtime <- function(total_time = NULL, caller = NULL){

  if(missing(total_time)){
    return(missing())
  } else{
    message("The query took ", str_c(round(total_time, 3), "mins to", caller, sep = " "))
  }
  
}


