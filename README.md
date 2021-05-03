# r-odbc-error-resolution

In some cases, when we attempt to connect to the SQL Server via ODBC connection, we may encounter an error that reads

`nanodbc/nanodbc.cpp:3011: 07009: [Microsoft][SQL Server Native Client 11.0]Invalid Descriptor Index Attempting to run by sorting the columns`

It's a simple function to mitigate the problem.
