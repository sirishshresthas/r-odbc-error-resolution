# r-odbc-error-resolution

In some cases, when we attemtp to connect to the SQL Server via ODBC connection, we may encounter an error that reads

`nanodbc/nanodbc.cpp:3011: 07009: [Microsoft][SQL Server Native Client 11.0]Invalid Descriptor Index Attempting to run by sorting the columns`

This is my attempt to create a function that I can frequently use so that I don't have this problem to worry about anymore. 
