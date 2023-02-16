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
library(tibble)
library(purrr)

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
  mutate(casos = as.numeric(conteos$casos))

#Se saca el join

conteos <- left_join(conteos, ensanut, by = "cvegeo")

#Definamos por olas

##Sacar conteos por olas

##ola1

conteos1_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 10:32) %>% 
  group_by(cvegeo) %>% 
  summarise(casos_semana = sum(casos))

names(conteos1_ola)[2] <- "ola"

ola1 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 10:32)

##ola2

conteos2_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 44:53) %>% 
  group_by(cvegeo) %>% 
  summarise(n = sum(casos))

conteos2_ola2 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 01:12) %>% 
  group_by(cvegeo) %>% 
  summarise(n = sum(casos))

###se realiza el join de los conteos de la ola 2

conteos2_ola2 <-left_join(conteos2_ola, conteos2_ola2, by="cvegeo") 

###Se suma los conteos para sacar el general

conteos2_ola2 <- conteos2_ola2 %>% 
  mutate(ola = rowSums(conteos2_ola2[ , c(2,3)], na.rm=TRUE)) %>% 
  select(-c(n.x, n.y))

##Se filtra la tabla ola2 

ola2 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 44:53) 

ola2_1 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 01:12)

ola2 <- rbind(ola2, ola2_1)

##ola 3
#Se saca el conteo de la ola

conteos3_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 24:41) %>% 
  group_by(cvegeo) %>% 
  summarise(casos_semana = sum(casos))

names(conteos3_ola)[2] <- "ola"

ola3 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 24:41)

#ola 4

conteos4_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 50:53) %>% 
  group_by(cvegeo) %>% 
  summarise(n = sum(casos))

conteos4_ola4 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 01:14) %>% 
  group_by(cvegeo) %>% 
  summarise(n = sum(casos))

###se realiza el join de los conteos de la ola 4

conteos4_ola <-left_join(conteos4_ola, conteos4_ola4, by="cvegeo") 

###Se suma los conteos para sacar el general

conteos4_ola <- conteos4_ola %>% 
  mutate(ola = rowSums(conteos4_ola[ , c(2,3)], na.rm=TRUE)) %>% 
  select(-c(n.x, n.y))

##Se filtra la tabla ola4 

ola4 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 50:53) 

ola4_1 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 01:14)

ola4 <- rbind(ola4, ola4_1)

#ola 5

conteos5_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 21:39) %>% 
  group_by(cvegeo) %>% 
  summarise(n = sum(casos))

names(conteos5_ola)[2] <- "ola"

ola5 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 21:39)

#Se realiza el join para las olas

ola1 <- left_join(ola1, conteos1_ola, by="cvegeo")
ola2 <- left_join(ola2, conteos2_ola2, by="cvegeo")
ola3 <- left_join(ola3, conteos3_ola, by="cvegeo")
ola4 <- left_join(ola4, conteos4_ola, by="cvegeo")
ola5 <- left_join(ola5, conteos5_ola, by="cvegeo")

#Se realiza la funcion para crear las graficas de la obesidad 

obesidad <- function(x){
  grafica <- x %>% 
    group_by(cvegeo, entidad_res, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola) %>% 
    summarise(n = sum(casos)) %>% 
    mutate(porcen_obesidad = n*100/ola) %>% 
    filter(obesidad.x == "SI")
  a <- ggplot(grafica, aes(x = obesidad.y, y = porcen_obesidad))+
    geom_point(aes(color=entidad_res),size = 1.5)+
    scale_color_viridis(discrete=TRUE, option="viridis")+
    geom_smooth(method = lm)+
    geom_abline(linetype = 2)+
    facet_grid(tipo_paciente~.)+
    labs(x = "% Prevalence obesity",
         y = "% Population with obesity covid19",
         colour = "Mexico States")
  
  b <- ggplot(grafica, aes(x = obesidad.y, y = porcen_obesidad))+
    geom_point(aes(color=entidad_res),size = 1.5)+
    scale_color_viridis(discrete=TRUE, option="viridis")+
    geom_smooth(method = lm)+
    geom_abline(linetype = 2)+
    scale_x_continuous(limits = c(0,100))+
    facet_grid(tipo_paciente~.)+
    labs(x = "% Prevalence obesity",
         y = "% Population with obesity covid19",
         colour = "Mexico States")
  return(list(a, b))
}

#Graficas para obesidad

obesidad(ola1)
obesidad(ola2)
obesidad(ola3)
obesidad(ola4)
obesidad(ola5)

#Funcion para graficas de diabetes 

diabetes <- function(x){
  grafica <- x %>% 
    group_by(cvegeo, entidad_res, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola) %>% 
    summarise(n = sum(casos)) %>% 
    mutate(porcen_diabetes = n*100/ola) %>% 
    filter(diabetes.x == "SI")
  a <- ggplot(grafica, aes(x = diabetes.y, y = porcen_diabetes))+
          geom_point(aes(color=entidad_res),size = 1.5)+
          scale_color_viridis(discrete=TRUE, option="viridis")+
          geom_smooth(method = lm)+
          geom_abline(linetype = 2)+
          facet_grid(tipo_paciente~.)+
          labs(x = "% Prevalence diabetes",
               y = "% Population with diabetes covid19",
               colour = "Mexico States")
  
   b<- ggplot(grafica, aes(x = diabetes.y, y = porcen_diabetes))+
          geom_point(aes(color=entidad_res),size = 1.5)+
          scale_color_viridis(discrete=TRUE, option="viridis")+
          geom_smooth(method = lm)+
          geom_abline(linetype = 2)+
          scale_x_continuous(limits = c(0,100))+
          facet_grid(tipo_paciente~.)+
          labs(x = "% Prevalence diabetes",
               y = "% Population with diabetes covid19",
               colour = "Mexico States")
   return(list(a, b))
   
}

#Graficas para diabetes

diabetes(ola1)
diabetes(ola2)
diabetes(ola3)
diabetes(ola4)
diabetes(ola5)

#Funcion para graficas de hipertension 

hipertension <- function(x){
  grafica <- x %>% 
    group_by(cvegeo, entidad_res, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola) %>% 
    summarise(n = sum(casos)) %>% 
    mutate(porcen_hipertension = n*100/ola) %>% 
    filter(hipertension.x == "SI")
  a <- ggplot(grafica, aes(x = hipertension.y, y = porcen_hipertension))+
          geom_point(aes(color=entidad_res),size = 1.5)+
          scale_color_viridis(discrete=TRUE, option="viridis")+
          geom_smooth(method = lm)+
          geom_abline(linetype = 2)+
          facet_grid(tipo_paciente~.)+
          labs(x = "% Prevalence hypertension",
               y = "% Population with hypertension covid19",
               colour = "Mexico States")
  
  b <- ggplot(grafica, aes(x = hipertension.y, y = porcen_hipertension))+
          geom_point(aes(color=entidad_res),size = 1.5)+
          scale_color_viridis(discrete=TRUE, option="viridis")+
          geom_smooth(method = lm)+
          geom_abline(linetype = 2)+
          scale_x_continuous(limits = c(0,100))+
          facet_grid(tipo_paciente~.)+
          labs(x = "% Prevalence hypertension",
               y = "% Population with hypertension covid19",
               colour = "Mexico States")
  return(list(a, b))
}

#Graficas para hipertension

hipertension(ola1)
hipertension(ola2)
hipertension(ola3)
hipertension(ola4)
hipertension(ola5)

#Mapa de la prevalencia de las morbilidades por municipio
##Leemos la base y cambiamos la base para poder hacer el join 

mapa <- read_sf("00mun.shp")

names(mapa)[1] <- "cvegeo"

##Hacemos un join completo para que pueda salir en el mapa baja california 

mapmuni <- full_join(ensanut, mapa, by = "cvegeo")

##Mapa para obesidad

mapmuni %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = obesidad))+
  labs(title = "% Population with obesity in México",
       caption = "Ensanut",
       fill = "% Obesity")+
  viridis::scale_fill_viridis()

##Mapa para hipertension

mapmuni %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = hipertension))+
  labs(title = "% Population with hypertension in México",
       caption = "Ensanut",
       fill = "% Hypertension")+
  viridis::scale_fill_viridis()

##Mapa para diabetes

mapmuni %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = diabetes))+
  labs(title = "% Population with diabetes in México",
       caption = "Ensanut",
       fill = "% Diabetes")+
  viridis::scale_fill_viridis()

