##' Function to create connection to the data. This is set as a function so that multiple concurrent requests to the database can be made within a process
##' @param driver database driver name
##' @param host database host (server) name
##' @param dbname database name
##' @param uid username to connect to the database
##' @param pwd password to connect to the database
##' @return connection string that establishes the connection to the database
##' @examples
##' conn <- init_connection()

init_connection <- function(){
  
  sqlConfig <- config::get('sqldb_config')
  
  conn <- dbConnect(odbc::odbc(), 
                    Driver = "SQL Server", 
                    Server = sqlConfig$Server,
                    Port = sqlConfig$Port, 
                    Database = sqlConfig$Database,
                    Trusted_Connection = sqlConfig$Trusted_Connection,
                    # UID = rstudioapi::askForPassword("Database User: "),
                    UID = sqlConfig$UID,
                    PWD = sqlConfig$PWD)
  # PWD = rstudioapi::askForPassword("Database Password: "))
  return(conn)
  
}


end_connection <- function(conn=NULL){
  
  if(missing(conn)){
    stop("No connection is found")
  }
  
  DBI::dbDisconnect(conn)
  
}
