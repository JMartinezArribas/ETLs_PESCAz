!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Contact: javimartinezarribas@gmail.com
# Date:    12-12-2022
# Resume:  This script loads the data from the UNTITLED.XLSX file, 
#          selects and transforms the fields that will be used in its subsequent 
#          analysis and loads the tblCruzeiro table with the result dataset.
#
#         This data will be used in:
#         - Matura-Comprimento:  index.Rmd
###############################################################################
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(varhandle)
library(DBI)


dir = '../../../DATA/input/'
f1 = paste(dir,'UNTITLED.XLSX',sep="")
df = read_excel(f1)



df <- df %>%
  select(Data,Lance,Estrato,Especie,Complt,
         Complf,Sexo,Matura,Peso,Gonadagr) %>%
  mutate(Ano = lubridate::year(df$Data),
         Mes = lubridate::month(df$Data, label = FALSE),
         Trimestre = lubridate::quarter(df$Data),
         Area = substr(word(df$Lance, -3, sep="-"),2,4),
         Area_group = case_when(
           Area %in% c('FCO') ~ "WG",
           Area %in% c('GRA','SJO','FPI','TER') ~ "CG",
           Area %in% c('SMI','SMA') ~ "EG",
           Area %in% c('PAL','AÇO') ~ "CB",
           Area %in% c('MPR') ~ "EB",
           Area %in% c('MAR') ~ "Mid-Atlantic Ridge",
           Area %in% c('CONDOR','MEIO','FOR','OMS','DJC') ~ "OUTROS"),
         Matura = apply(df['Matura'],2, function(x) gsub("\\s+", "", x)),
         Matura = str_replace(Matura, "/", ""),
         Matura = replace_na(Matura,""),
         Matura_len = str_count(Matura),
         Matura_len = replace_na(Matura_len,0),
         Sexo = lapply(apply(df['Sexo'],2,function(x) gsub("\\s+", "", x)),unlist),
         Sexo = toupper(Sexo),
         Hermafrodita = case_when(
           Sexo %in% c('HFM','HMF','HM','HF') ~ TRUE,
           TRUE ~ FALSE),
         ) %>%
  rename(Peso_gonada = Gonadagr, Peso_total = Peso, Total_length = Complt,
         Furcal_length = Complf) %>%
  filter(Sexo %in% c('F','M','HF','HM','HFM','HMF'))



for (row in 1:nrow(df)) {
  if(df[row,'Hermafrodita']==TRUE){
    
    if (df[row,'Matura_len'] == 1){
      # Sexo HM, HF
      if (df[row,'Matura'] %in% c('0','1','2','3','4','5','6','7','8') & 
          (substr(df[row,'Sexo'],2,2) %in% c('F','M')) &
          (substr(df[row,'Sexo'],1,1) %in% c('H'))) {
        
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- df[row,'Matura']
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- TRUE
        
      }
      #Sexo H o M y Matura 0,1,2,3...
      else if (df[row,'Matura'] %in% c('0','1','2','3','4','5','6','7','8') & 
          (substr(df[row,'Sexo'],1,1) %in% c('F','M'))) {
        
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],1,1)
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- df[row,'Matura']
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- TRUE
        
      }
      else{
        df[row,'SexoH1'] <- ''
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- ''
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- FALSE
      }
    }
    #  Matura='F1','M3','21','15' etc ...
    else if(df[row,'Matura_len'] == 2){
      if(substr(df[row,'Matura'],1,1) %in% c('M','F') & 
         substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8')){
        df[row,'SexoH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],2,2)
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- TRUE  
      }
      # Sexo HMF o HFM y Matura dois digitos
      else if(substr(df[row,'Matura'],1,1) %in% c('0','1','2','3','4','5','6','7','8') & 
              substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
              substr(df[row,'Sexo'],2,2) %in% c('M','F') &
              substr(df[row,'Sexo'],3,3) %in% c('M','F')) {
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
        df[row,'SexoH2'] <- substr(df[row,'Sexo'],3,3) 
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'MaturaH2'] <- substr(df[row,'Matura'],2,2)
        df[row,'Updated'] <- TRUE
      }
      # Sexo HM o HF y Matura dois digitos
      else if(substr(df[row,'Matura'],1,1) %in% c('0','1','2','3','4','5','6','7','8') & 
              substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
              substr(df[row,'Sexo'],1,1) %in% c('H') &
              substr(df[row,'Sexo'],2,2) %in% c('M','F')) {
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
        df[row,'SexoH2'] <- ""
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'MaturaH2'] <- ""
        df[row,'Updated'] <- TRUE
      }
      #Matura 02 e Sexo F o M
      else if(substr(df[row,'Matura'],1,1) %in% c('0','1','2','3','4','5','6','7','8') & 
              substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
              substr(df[row,'Sexo'],1,1) %in% c('M','F')) {
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],1,1)
        df[row,'SexoH2'] <- "" 
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'MaturaH2'] <- ""
        df[row,'Updated'] <- TRUE
      }
      else{
        df[row,'SexoH1'] <- ''
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- ''
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- FALSE
      }
      
    }
    #Matura de la forma 1/3 y Sexo HMF o HFM (No vale 'HM' o 'HF')
    else if(df[row,'Matura_len'] == 3){
      
      if(substr(df[row,'Matura'],1,1) %in% c('M','F') &
         substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
         substr(df[row,'Matura'],3,3) %in% c('M','F')
      ){
        df[row,'SexoH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'SexoH2'] <- ""
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],2,2)
        df[row,'MaturaH2'] <- ""
        df[row,'Updated'] <- TRUE
      }
      else{
        df[row,'SexoH1'] <- ''
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- ''
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- FALSE
      }
      
    }
    
    # Sexo 'HMF' o 'HFM' y Matura 'F4M3' o 'M2F3' ...
    else if(df[row,'Matura_len'] == 4){
      if(substr(df[row,'Matura'],1,1) %in% c('M','F') &
         substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
         substr(df[row,'Matura'],3,3) %in% c('M','F') &
         substr(df[row,'Matura'],4,4) %in% c('0','1','2','3','4','5','6','7','8') 
      ){
        df[row,'SexoH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'SexoH2'] <- substr(df[row,'Matura'],3,3) 
        df[row,'MaturaH1'] <- substr(df[row,'Matura'],2,2)
        df[row,'MaturaH2'] <- substr(df[row,'Matura'],4,4)
        df[row,'Updated'] <- TRUE
        
      }
      else{
        df[row,'SexoH1'] <- ''
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- ''
        df[row,'MaturaH2'] <- ''
        df[row,'Updated'] <- FALSE
      }
    }
    else{
      df[row,'SexoH1'] <- ''
      df[row,'SexoH2'] <- '' 
      df[row,'MaturaH1'] <- ''
      df[row,'MaturaH2'] <- ''
      df[row,'Updated'] <- FALSE
    }
  }
  else{
    # Sexo M, F y Matura 0,1,2,15...
    if (substr(df[row,'Matura'],1,1) %in% c('0','1','2','3','4','5','6','7','8') & 
        (substr(df[row,'Sexo'],1,1) %in% c('F','M'))) {
      df[row,'SexoH1'] <- substr(df[row,'Sexo'],1,1)
      df[row,'SexoH2'] <- '' 
      df[row,'MaturaH1'] <- substr(df[row,'Matura'],1,1)
      df[row,'MaturaH2'] <- ''
      df[row,'Updated'] <- TRUE
      
    }
    else{
      df[row,'SexoH1'] <- ''
      df[row,'SexoH2'] <- '' 
      df[row,'MaturaH1'] <- ''
      df[row,'MaturaH2'] <- ''
      df[row,'Updated'] <- FALSE
    }
    
  }
}

df <- df[df$Updated==TRUE,]

df <-  df[,c('Ano','Mes','Trimestre','Area','Estrato','Especie','Total_length',
             'Furcal_length','Sexo','Matura','Hermafrodita','SexoH1','SexoH2',
             'MaturaH1','MaturaH2','Peso_total','Peso_gonada','Data')]

dir = '../../../DATA/output/'
f2 = paste(dir,'cruzeiro_data.csv',sep="")
write_csv(df,f2)

##### Insert into DB
dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'), 
                          host = Sys.getenv('POSTGRES_HOST'), 
                          port = Sys.getenv('POSTGRES_PORT'), 
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

DBI::dbWriteTable(dbicon, "tblCruzeiro", df, row.names=FALSE, overwrite=T) #, append=TRUE

dbDisconnect(dbicon)
