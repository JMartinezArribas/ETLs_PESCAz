#!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Contact: javimartinezarribas@gmail.com
# Date:    02-02-2023
# Resume:  This script loads the data from "Calculos de RPN-LF (area).xlsx" file,
#          and "Calculos de RPN-LF (subarea)" selects and transforms the fields
#          that will be used in its subsequent analysis and loads 
#          the "tblRPN-LF" table with the result dataset.
#
#          This data will be used in:
#            - Size Structure Analysis: index_size_structure.Rmd
###############################################################################


library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(varhandle)
library(DBI)

dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                         db = Sys.getenv('POSTGRES_DB'), 
                         host = Sys.getenv('POSTGRES_HOST'), 
                         port = Sys.getenv('POSTGRES_PORT'), 
                         user = Sys.getenv('POSTGRES_USER'),
                         password = Sys.getenv('POSTGRES_PASSWORD'))

#"Calculos de RPN-LF (area).xlsx"
file1 <- "../../../DATA/input/Calculos de RPN-LF (subarea).xlsx"
df1 = read_excel(file1)

##Creo que falta crear la tabla si fuese necesario.
#DBI::dbWriteTable(dbicon, "tblRPN-LF_subarea", df2, row.names=FALSE, overwrite=T) #, append=TRUE

#"Calculos de RPN-LF (subarea).xlsx"
#file2 <- "../../../DATA/input/Calculos de RPN-LF (area).xlsx"
#df2 = read_excel(file2)
#df2 <- add_column(df2, Subarea='', .after=5)

DBI::dbWriteTable(dbicon, "tblRPN-LF", df1, row.names=FALSE, overwrite=T) #, append=TRUE

dbDisconnect(dbicon)

