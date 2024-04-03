#!/usr/bin/env Rscript
###############################################################################
# Author:  Javier Martinez-Arribas
# Contact: javimartinezarribas@gmail.com
# Date:    23-03-2023
# Resume:  This script loads the data from "Descargas nos Açores.xlsx" file,
#          selects and transforms the fields and group some especies:
#           Goraz: Goraz + Peixão + Carapau (nos anos anteriores a 2020)
#           Alfonsinos: Alfonsim + Imperador
#           Pargo: Pargo + parguete
#           Congro: Congro + Safio
#          We have to read all files in the folder and create some extra fields
#          like Clase and Nome cientifico doing a left join with a master file.
#          This data will be used in:
#            - Desembarques.Rmd
###############################################################################


library(tidyverse)
library(dplyr)
library(purrr)
library(readxl)
library(DBI)


tryCatch(
  expr = {
    dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                              db = Sys.getenv('POSTGRES_DB'), 
                              host = Sys.getenv('POSTGRES_HOST'), 
                              port = Sys.getenv('POSTGRES_PORT'), 
                              user = Sys.getenv('POSTGRES_USER'),
                              password = Sys.getenv('POSTGRES_PASSWORD'))
    
    #"Calculos de RPN-LF (area).xlsx"
    file1 <- "/Users/javiermartinez/Documents/Regis/Proyecto/PESCAz/DATA/input/Descargas nos Açores_2015_2022.xlsx"
    df_maestro = read_excel(file1)
    
    #Indicates the path of the folder where these xlsx are located
    setwd('/Users/javiermartinez/Documents/Regis/Proyecto/PESCAz/Github/Uploaded_repositories/Desembarques/FicherosDescargas')
    
    #load files
    file.list <- list.files(pattern='*.xlsx')
    #anos <- sub("Descargas nos Açores_","",sub(".xlsx","",file.list))
    
    #creat the list files
    df.list <- lapply(file.list, read_excel)
    #df.list <- mapply(cbind, df.list, "Ano"=anos, SIMPLIFY=F)
    
    #makes the union of all in a dataframe
    df_final<-purrr::reduce(df.list, rbind)
    
  
    df_maestro <- df_maestro %>%
                    rename(Nome_Cientifico = `Nome cientifico`,
                           Especie = Espécie) %>%
                    select(Especie,Clase, Nome_Cientifico)
    
    df_maestro <- distinct(df_maestro)
                    
            
    df_final$Especie[df_final$Especie %in% c("Goraz","Peixão")] <- "Goraz"
    df_final$Especie[(df_final$Especie == "Carapau" & df_final$Ano < 2020)] <- "Goraz"
    
    df_final$Especie[df_final$Especie %in% c("Alfonsino","Imperador")] <- "Alfonsinos"
    df_final$Especie[df_final$Especie %in% c("Pargo","parguete")] <- "Pargo"
    df_final$Especie[df_final$Especie %in% c("Congro","Safio")] <- "Congro"
    
    df <- left_join(df_final, df_maestro, by="Especie")
    
    df <- df %>%
      group_by(Especie,Ano)%>%
      reframe(Clase=Clase,
                Nome_Cientifico=Nome_Cientifico,
                Peso_Kg=sum(Peso_Kg), #A soma não é necessária porque há um registro por espécie e ano
                Peso_Tn=sum(Peso_Kg)/1000, #o mesmo que acima
                Valor_EUR=sum(Valor_EUR), #o mesmo que acima
                Preço_Medio_Kg=mean(Preço_Medio_Kg))  #o mesmo que acima
                
    df <- distinct(df)
      
    
    DBI::dbWriteTable(dbicon, "tblDescargas", df, row.names=FALSE, overwrite=T) #, append=TRUE
    
    
    dbDisconnect(dbicon)
    print("Loading of 'tblDescargas' completed successfully...")
  },
  error = function(e){ 
    print(paste("Error:", e))
    stop("Please verify the error and re-run the script or contact with the administrator.")
  },
  warning = function(w){
    print(paste("Warning:", w))
    print("Please verify if the warning message could affect the analysis.")
  }
)

