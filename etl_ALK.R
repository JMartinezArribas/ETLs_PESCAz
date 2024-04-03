#!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Contact: javimartinezarribas@gmail.com
# Date:    03-02-2023
# Resume:  This script loads the data from "ALK.xlsx" file,
#          selects and transforms the fields that will be used in its subsequent 
#          analysis and loads the tblALK table with the result dataset.
#           
#          
#          This data will be used in:
#            - Age-Length-Keys:  index_alk.Rmd
###############################################################################

library(tidyverse)
library(readxl)
library(DBI)

dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'), 
                          host = Sys.getenv('POSTGRES_HOST'), 
                          port = Sys.getenv('POSTGRES_PORT'), 
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

#"ALK.xlsx"
file <- "../../../DATA/input/ALK.xlsx"
df = read_excel(file)

#df <- df %>% 
#  select(Ano,Sexoi,LF...8,Idade) %>% 
#  rename(LF = LF...8, Sexo = Sexoi)

df <- df %>%
  select(Ano,Sexoi,LF...8,Idade) %>%
  rename(LF = LF...8, Sexo = Sexoi) %>%
  mutate(Sexo = lapply(apply(df['Sexoi'],2,function(x) gsub("\\s+", "", x)),unlist),
         Sexo = toupper(Sexo),
         Hermafrodita = case_when(Sexo %in% c('HFM','HMF','HM','HF') ~ TRUE,
                                  TRUE ~ FALSE),
        ) 


for (row in 1:nrow(df)) {
  if(df[row,'Hermafrodita']==TRUE){
    df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
  }
  else{
    df[row,'SexoH1'] <- substr(df[row,'Sexo'],1,1)
  }
}    
    


DBI::dbWriteTable(dbicon, "tblALK", df, row.names=FALSE, overwrite=T)

dbDisconnect(dbicon)
