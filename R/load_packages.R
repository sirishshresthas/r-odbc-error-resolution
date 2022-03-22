## load packages and source files

if(!require("config")) install.packages("config")
if(!require("testthat")) install.packages("testthat")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("odbc")) install.packages("odbc")

library("config")
library("testthat")
library("tidyverse")
library("odbc")

source("R/init_connection.R")
source("R/r-sql-odbc-error-fix.R")
source("R/process-column-names.R")
