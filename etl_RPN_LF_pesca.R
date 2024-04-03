#!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Date:    02-02-2023
# Resume:  This script loads the data from "Nome_especie-ComprimentosPesca.csv" file,
#          selects and transforms the fields that will be used in its subsequent 
#          analysis and loads the tblRPN-LF_pesca table with the result dataset.
#           
#          
#          This data will be used in:
#            - Size-Structure-Analysis: index_size_structure_pesca.Rmd
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

#"Pagellus_bogaraveo-ComprimentosPesca.csv"
file1 <- "../../../DATA/input/Pagellus_bogaraveo-ComprimentosPesca.csv"
df1 = read.csv(file1, sep=',',fileEncoding="latin1")
cols <- c("Dia_amostra","Mes_amostra","Ano_amostra","Especie","LF","n")
df1 <- df1 %>% select(all_of(cols))

DBI::dbWriteTable(dbicon, "tblRPN-LF_pesca", df1, row.names=FALSE, overwrite=T)

#"Pagrus_pagrus-ComprimentosPesca.csv"
file2 <- "../../../DATA/input/Pagrus_pagrus-ComprimentosPesca.csv"
df2 = read.csv(file2, sep=',',fileEncoding="latin1")
df2 <- df2 %>% select(all_of(cols))

DBI::dbWriteTable(dbicon, "tblRPN-LF_pesca", df2, row.names=FALSE, append=T) #, append=TRUE


#sql <- 'SELECT * FROM "tblRPN-LF_pesca"'
#df <- dbGetQuery(dbicon, sql)


dbDisconnect(dbicon)
