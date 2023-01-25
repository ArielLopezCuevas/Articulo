library(readr)
library(lubridate)
library(askpass)
library(DBI)
library(RPostgres)
library(DBI)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)

#Leemos las bases

conteos_semanales_covid_comorbilidades <- read_csv("~/conteos_semanales_covid_comorbilidades.csv")

conteos <- conteos_semanales_covid_comorbilidades %>%  
  separate(`semana_epi_sintomas\tentidad_res\tmunicipio_res\ttipo_paciente\tdefuncion\tobesidad\tdiabetes\thipertension\tn`, 
           into = c("semana_epi_sintomas", "entidad_res", "municipio_res", "tipo_paciente", "defuncion", "obesidad", "diabetes", "hipertension", "n"), 
           sep = "\t")

ensanut <- read_csv("~/R/ensanut_areas_peq.csv")

#Cambiar los números por caracteres 

names(ensanut)[1] <- "cvegeo"
names(conteos)[9] <- "casos"
conteos$obesidad <- gsub(1, "SI", conteos$obesidad)
conteos$obesidad <- gsub(2, "NO", conteos$obesidad)
conteos$obesidad <- gsub(98, "SE IGNORA", conteos$obesidad)
conteos$diabetes <- gsub(1, "SI", conteos$diabetes)
conteos$diabetes <- gsub(2, "NO", conteos$diabetes)
conteos$diabetes <- gsub(98, "SE IGNORA", conteos$diabetes)
conteos$hipertension <- gsub(1, "SI", conteos$hipertension)
conteos$hipertension <- gsub(2, "NO", conteos$hipertension)
conteos$hipertension <- gsub(98, "SE IGNORA", conteos$hipertension)
conteos$tipo_paciente <- gsub(1, "AMBULATORIO", conteos$tipo_paciente)
conteos$tipo_paciente <- gsub(2, "HOSPITALIZADO", conteos$tipo_paciente)
conteos$tipo_paciente <- gsub(99, "NO ESPECIFICADO", conteos$tipo_paciente)

#Mutate para sacar el cvegeo

conteos <- conteos %>% 
  mutate(entidad_res = str_pad(string = entidad_res, 
                               width = 2, 
                               side = "left", pad = 0)) %>%  
  mutate(municipio_res = str_pad(string = conteos$municipio_res, 
                                 width = 3, 
                                 side = "left", pad = 0)) %>% 
  mutate(cvegeo = paste0(entidad_res, municipio_res)) %>% 
  mutate(cvegeo = as.numeric(paste0(entidad_res, municipio_res))) %>% 
  mutate(casos = as.numeric(conteos$casos))

#Se cambia la clase del cvegeo de ensanut por dobble para que se pueda realizar el join

ensanut <- ensanut %>% mutate(cvegeo = as.numeric(cvegeo))

#Se saca el join

conteos <- left_join(conteos, ensanut, by = "cvegeo")

#Para sacar la suma de los municipios

conteos1 <- conteos %>%  group_by(cvegeo) %>% tally()

#Juntar las dos tablas 

conteos2 <- left_join(conteos, conteos1)

#Definamos por olas

##Sacar conteos por olas

##ola1

conteos1_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 10:32) %>% 
  group_by(cvegeo) %>% 
  tally()

names(conteos1_ola)[2] <- "ola1"

ola1 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 10:32)