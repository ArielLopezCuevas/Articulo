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