#Librerys####
library(tidyverse)
library(openxlsx)
library(eph)


#Trabajo la base####
base <- get_microdata(year = 2023, trimester = 3, type = "individual", vars = "all")
#Falta definir las vars
#Creo las vars nuevas de la base

base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(
    nivel.ed = factor(
      case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                NIVEL_ED %in% 4 ~ "Secundaria Completa",
                NIVEL_ED == 5 ~ "Superior Incompleto",
                NIVEL_ED == 6 ~ "Superior Completo",
                TRUE ~ "Ns/Nr"),
      levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo")))
base <- base %>%
      filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
      mutate(
        tamanio.establec = factor(
      case_when(PP04C %in% 1:6  ~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 ~ "Grande",
                PP04C %in% 99   ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")))

base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(   descuento_jubil = case_when(PP07H == 1 ~ "Protegido",
                                PP07H == 2 ~ "No-protegido"))
    
base <- base %>%
      filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
      mutate(aportes_propios = case_when(PP07I == 1 ~ "Monotributista",
                                         PP07I == 2 ~ "Negro"))
        
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate( part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si",
                              TRUE ~ "No"))
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(tiempo.determinado = case_when(PP07C ==  1 ~ "Si",
                                   TRUE ~ "No"))

