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

##ola2

conteos2_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2020) %>% 
  filter(semana == 44:53) %>% 
  group_by(cvegeo) %>% 
  tally()

conteos2_ola2 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 01:12) %>% 
  group_by(cvegeo) %>% 
  tally()

###se realiza el join de los conteos de la ola 2

conteos2_ola2 <-left_join(conteos2_ola, conteos2_ola2, by="cvegeo") 

###Se suma los conteos para sacar el general

conteos2_ola2 <- conteos2_ola2 %>% 
  mutate(ola2 = rowSums(conteos2_ola2[ , c(2,3)], na.rm=TRUE)) %>% 
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

ola2 <- full_join(ola2, ola2_1)

##ola 3
#Se saca el conteo de la ola

conteos3_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2021) %>% 
  filter(semana == 24:41) %>% 
  group_by(cvegeo) %>% 
  tally()

names(conteos3_ola)[2] <- "ola3"

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
  tally()

conteos4_ola4 <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 01:14) %>% 
  group_by(cvegeo) %>% 
  tally()

###se realiza el join de los conteos de la ola 4

conteos4_ola <-left_join(conteos4_ola, conteos4_ola4, by="cvegeo") 

###Se suma los conteos para sacar el general

conteos4_ola <- conteos4_ola %>% 
  mutate(ola4 = rowSums(conteos4_ola[ , c(2,3)], na.rm=TRUE)) %>% 
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

ola4 <- full_join(ola4, ola4_1)

#ola 5

conteos5_ola <- conteos %>% 
  separate(semana_epi_sintomas, into = c("anuum", "semana"), sep = "-") %>% 
  filter(anuum == 2022) %>% 
  filter(semana == 21:39) %>% 
  group_by(cvegeo) %>% 
  tally()

names(conteos5_ola)[2] <- "ola5"

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

#Se saca grafica por olas para obesidad

ola1 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola1) %>% 
  tally() %>% 
  mutate(porcen_obesidad = n*100/ola1) %>% 
  filter(obesidad.x == "SI") %>% 
  ggplot(aes(x = obesidad.y, y = porcen_obesidad))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "First wave obesity",
       subtitle = "Datetime epiweek 2020-10 to epiweek 2020-32",
       x = "% Prevalence obesity",
       y = "% Population with obesity covid19")

ola2 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola2) %>% 
  tally() %>% 
  mutate(porcen_obesidad = n*100/ola2) %>% 
  filter(obesidad.x == "SI") %>% 
  ggplot(aes(x = obesidad.y, y = porcen_obesidad))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Second wave obesity",
       subtitle = "Datetime epiweek 2020-44 to epiweek 2021-12",
       x = "% Prevalence obesity",
       y = "% Population with obesity covid19")

ola3 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola3) %>% 
  tally() %>% 
  mutate(porcen_obesidad = n*100/ola3) %>% 
  filter(obesidad.x == "SI") %>% 
  ggplot(aes(x = obesidad.y, y = porcen_obesidad))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Third wave obesity",
       subtitle = "Datetime epiweek 2021-24 to epiweek 2021-42",
       x = "% Prevalence obesity",
       y = "% Population with obesity covid19")

ola4 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola4) %>% 
  tally() %>% 
  mutate(porcen_obesidad = n*100/ola4) %>% 
  filter(obesidad.x == "SI") %>% 
  ggplot(aes(x = obesidad.y, y = porcen_obesidad))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fourth wave obesity",
       subtitle = "Datetime epiweek 2021-50 to epiweek 2022-14",
       x = "% Prevalence obesity",
       y = "% Population with obesity covid19")

ola5 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, obesidad.x, obesidad.y, ola5) %>% 
  tally() %>% 
  mutate(porcen_obesidad = n*100/ola5) %>% 
  filter(obesidad.x == "SI") %>% 
  ggplot(aes(x = obesidad.y, y = porcen_obesidad))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fifth wave obesity",
       subtitle = "Datetime epiweek 2022-21 to epiweek 2022-39",
       x = "% Prevalence obesity",
       y = "% Population with obesity covid19")

#Diabetes por olas

ola1 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola1) %>% 
  tally() %>% 
  mutate(porcen_diabetes = n*100/ola1) %>% 
  filter(diabetes.x == "SI") %>% 
  ggplot(aes(x = diabetes.y, y = porcen_diabetes))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "First wave diabetes",
       subtitle = "Datetime epiweek 2020-10 to epiweek 2020-32",
       x = "% Prevalence diabetes",
       y = "% Population with diabetes covid19")

ola2 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola2) %>% 
  tally() %>% 
  mutate(porcen_diabetes = n*100/ola2) %>% 
  filter(diabetes.x == "SI") %>% 
  ggplot(aes(x = diabetes.y, y = porcen_diabetes))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Second wave diabetes",
       subtitle = "Datetime epiweek 2020-44 to epiweek 2021-12",
       x = "% Prevalence diabetes",
       y = "% Population with diabetes covid19")


ola3 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola3) %>% 
  tally() %>% 
  mutate(porcen_diabetes = n*100/ola3) %>% 
  filter(diabetes.x == "SI") %>% 
  ggplot(aes(x = diabetes.y, y = porcen_diabetes))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Third wave diabetes",
       subtitle = "Datetime epiweek 2021-24 to epiweek 2021-42",
       x = "% Prevalence diabetes",
       y = "% Population with diabetes covid19")

ola4 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola4) %>% 
  tally() %>% 
  mutate(porcen_diabetes = n*100/ola4) %>% 
  filter(diabetes.x == "SI") %>% 
  ggplot(aes(x = diabetes.y, y = porcen_diabetes))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fourth wave diabetes",
       subtitle = "Datetime epiweek 2021-50 to epiweek 2022-14",
       x = "% Prevalence diabetes",
       y = "% Population with diabetes covid19")

ola5 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, diabetes.x, diabetes.y, ola5) %>% 
  tally() %>% 
  mutate(porcen_diabetes = n*100/ola5) %>% 
  filter(diabetes.x == "SI") %>% 
  ggplot(aes(x = diabetes.y, y = porcen_diabetes))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fifth wave diabetes",
       subtitle = "Datetime epiweek 2022-21 to epiweek 2022-39",
       x = "% Prevalence diabetes",
       y = "% Population with diabetes covid19")

#Hipertension por olas

ola1 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola1) %>% 
  tally() %>% 
  mutate(porcen_hipertension = n*100/ola1) %>% 
  filter(hipertension.x == "SI") %>% 
  ggplot(aes(x = hipertension.y, y = porcen_hipertension))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "First wave hypertension",
       subtitle = "Datetime epiweek 2020-10 to epiweek 2020-32",
       x = "% Prevalence hypertension",
       y = "% Population with hypertension covid19")

ola2 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola2) %>% 
  tally() %>% 
  mutate(porcen_hipertension = n*100/ola2) %>% 
  filter(hipertension.x == "SI") %>% 
  ggplot(aes(x = hipertension.y, y = porcen_hipertension))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Second wave hypertension",
       subtitle = "Datetime epiweek 2020-44 to epiweek 2021-12",
       x = "% Prevalence hypertension",
       y = "% Population with hypertension covid19")

ola3 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola3) %>% 
  tally() %>% 
  mutate(porcen_hipertension = n*100/ola3) %>% 
  filter(hipertension.x == "SI") %>% 
  ggplot(aes(x = hipertension.y, y = porcen_hipertension))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Third wave hypertension",
       subtitle = "Datetime epiweek 2021-24 to epiweek 2021-42",
       x = "% Prevalence hypertension",
       y = "% Population with hypertension covid19")

ola4 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola4) %>% 
  tally() %>% 
  mutate(porcen_hipertension = n*100/ola4) %>% 
  filter(hipertension.x == "SI") %>% 
  ggplot(aes(x = hipertension.y, y = porcen_hipertension))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fourth wave hypertension",
       subtitle = "Datetime epiweek 2021-50 to epiweek 2022-14",
       x = "% Prevalence hypertension",
       y = "% Population with hypertension covid19")

ola5 %>% 
  group_by(cvegeo, mun_nom, tipo_paciente, hipertension.x, hipertension.y, ola5) %>% 
  tally() %>% 
  mutate(porcen_hipertension = n*100/ola5) %>% 
  filter(hipertension.x == "SI") %>% 
  ggplot(aes(x = hipertension.y, y = porcen_hipertension))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()+
  facet_grid(tipo_paciente~.)+
  labs(title = "Fifth wave hypertension",
       subtitle = "Datetime epiweek 2022-21 to epiweek 2022-39",
       x = "% Prevalence hypertension",
       y = "% Population with hypertension covid19")
