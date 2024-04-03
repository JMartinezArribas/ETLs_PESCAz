#!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Contact: javimartinezarribas@gmail.com
# Date:    12-01-2023
# Resume:  This script loads the data from the Biologicas_ok_pesca.csv file, 
#          selects and transforms the fields that will be used in its subsequent 
#          analysis and loads the tblPesca table with the result dataset.
#
#         This data will be used in:
#         - Matura-Comprimento:  index_pesca.Rmd
###############################################################################


library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(varhandle)
library(DBI)

dir = '../../../DATA/input/'
f1 = paste(dir,'Biologicas_ok_Pesca.csv',sep="")
df = read.csv(f1, sep=',',fileEncoding="latin1")


df <- df %>%
  select(Especie,Data_captura,Local_captura,Arte,TL,FL,Peso_total,Peso_gonada,
         Sexo,Maturacao) %>%
  mutate(Ano = lubridate::year(mdy(df$Data_captura)),
         Mes = lubridate::month(mdy(df$Data_captura), label = FALSE),
         Trimestre = lubridate::quarter(mdy(df$Data_captura))) %>%
  rename(Matura=Maturacao, Total_length = TL, Furcal_length = FL)


df <- df %>%
  filter(Sexo %in% c('F','M','HF','HM','HFM','HMF')) %>%
  mutate(
    Sexo = str_trim(Sexo),
    Sexo = replace_na(Sexo,""),
    Sexo = toupper(Sexo),
    Hermafrodita = case_when(
      Sexo %in% c('HFM','HMF','HM','HF') ~ TRUE,
      TRUE ~ FALSE
    ))

#La codificación en numeros romanos se codifica con case_when
#Las Os sustituirlas por 0s en origen. Tampoco tiene sentido
#Los numeros con interrogación o asterisco se filtran
#Los registros con varias maduraciones pero un solo sexo, si es H, se filtra, si 
#es H o F, se aprovecha
#HM1F1 y H1F1 se filtra

#df['Maturacao'] <- apply(df['Maturacao'],2, function(x) gsub("\\s+", "", x))

#Matura = apply(df[,'Matura'],2, function(x) gsub("\\s+", "", x)),
df <- df %>% mutate(Matura = gsub("\\s+", "", Matura),
                    Matura = replace_na(Matura,""),
                    Matura = case_when(Matura %in% c('I') ~ '1',
                                       Matura %in% c('II') ~ '2',
                                       Matura %in% c('III') ~ '3',
                                       Matura %in% c('IV') ~ '4',
                                       Matura %in% c('V') ~ '5',
                                       Matura %in% c('VI') ~ '6',
                                       Matura %in% c('VII') ~ '7',
                                       Matura %in% c('VIII') ~ '8',
                                       Matura %in% c('O') ~ '0',
                                       TRUE ~ Matura),
                    Matura_len = str_count(Matura),
                    Matura_len = replace_na(Matura_len,0))


# El tamaño puede ser de 1 caracter, 2 caracteres, 3 caracteres y 4 caracteres
# 1 caracter: num = c('0','1','2','3','4','5','6','7','8')
# 2 caracteres: letra = c('M','F') + num = c('0','1','2','3','4','5','6','7','8')
# 3 caracteres: num + c('/') + num
# 4 caracteres: letra + num + letra + num




for (row in 1:nrow(df)) {
  if(df[row,'Hermafrodita']==TRUE){
    
    if (df[row,'Matura_len'] == 1){
      # Sexo HM, HF
      if (df[row,'Matura'] %in% c('0','1','2','3','4','5','6','7','8') & 
          (substr(df[row,'Sexo'],2,2) %in% c('F','M'))) {
          df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
          df[row,'SexoH2'] <- '' 
          df[row,'MaturaH1'] <- as.numeric(df[row,'Matura'])
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
    # Son Sexo='HMF' o 'HFM' o 'HM' o 'HF' y Maturacao='F1','M3', etc
    else if(df[row,'Matura_len'] == 2){
      if(substr(df[row,'Matura'],1,1) %in% c('M','F') & 
         substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8')){
        df[row,'SexoH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- as.numeric(as.vector(substr(df[row,'Matura'],2,2)))
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
    #Matura de la forma 1/3 y Sexo HMF o HFM (No vale 'HM' o 'HF')
    else if(df[row,'Matura_len'] == 3){
      
      if(substr(df[row,'Matura'],1,1) %in% c('0','1','2','3','4','5','6','7','8') &
        substr(df[row,'Matura'],2,2) %in% c('/') &
        substr(df[row,'Matura'],3,3) %in% c('0','1','2','3','4','5','6','7','8') &
        df[row,'Sexo'] %in% c('HFM','HMF')
        ){
          df[row,'SexoH1'] <- substr(df[row,'Sexo'],2,2)
          df[row,'SexoH2'] <- substr(df[row,'Sexo'],3,3) 
          df[row,'MaturaH1'] <- as.numeric(as.vector(substr(df[row,'Matura'],1,1)))
          df[row,'MaturaH2'] <- as.numeric(as.vector(substr(df[row,'Matura'],3,3)))
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
      
    # Sexo 'HMF' o 'HFM' y Maturacao 'F4M3' o 'M2F3' ...
    else if(df[row,'Matura_len'] == 4){
      if(substr(df[row,'Matura'],1,1) %in% c('M','F') &
         substr(df[row,'Matura'],2,2) %in% c('0','1','2','3','4','5','6','7','8') &
         substr(df[row,'Matura'],3,3) %in% c('M','F') &
         substr(df[row,'Matura'],4,4) %in% c('0','1','2','3','4','5','6','7','8') 
      ){
        df[row,'SexoH1'] <- substr(df[row,'Matura'],1,1)
        df[row,'SexoH2'] <- substr(df[row,'Matura'],3,3) 
        df[row,'MaturaH1'] <- as.numeric(as.vector(substr(df[row,'Matura'],2,2)))
        df[row,'MaturaH2'] <- as.numeric(as.vector(substr(df[row,'Matura'],4,4)))
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
      # Sexo M, F y Maturacao 0,1,2,...
      if (df[row,'Matura_len'] == 1 &
          df[row,'Matura'] %in% c('0','1','2','3','4','5','6','7','8') & 
          (substr(df[row,'Sexo'],1,1) %in% c('F','M'))) {
        df[row,'SexoH1'] <- substr(df[row,'Sexo'],1,1)
        df[row,'SexoH2'] <- '' 
        df[row,'MaturaH1'] <- as.numeric(df[row,'Matura'])
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
df['Area'] <- ''
df['Data'] <- df['Data_captura']
df <- df[df$Updated==TRUE,]


columns <- c('Ano','Mes','Trimestre','Especie','Arte','Local_captura','Area',
             'Total_length','Furcal_length','Sexo','Matura','Hermafrodita','SexoH1','SexoH2',
             'MaturaH1','MaturaH2','Peso_total','Peso_gonada','Data')


df <-  df[,columns]

f2 = paste(dir,'pesca_data.csv',sep="")
write_csv(df,f2)

##### Insert into DB
dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'), 
                          host = Sys.getenv('POSTGRES_HOST'), 
                          port = Sys.getenv('POSTGRES_PORT'), 
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

DBI::dbWriteTable(dbicon, "tblPesca", df, row.names=FALSE, overwrite=T) #, append=TRUE

dbDisconnect(dbicon)
