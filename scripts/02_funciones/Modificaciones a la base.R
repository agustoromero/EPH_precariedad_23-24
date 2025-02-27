#Librerys####
library(tidyverse)
library(openxlsx)
library(eph)
library(dplyr)
library(haven)

#Trabajo la base####
#Creo las vars nuevas de la base

base <- get_microdata(year = 2023, trimester = 3, type = "individual", vars = "all")
base <- organize_labels(base, type = "individual")

#Defino variables de la persona####
#Rango etario
base <- base %>%
  mutate(
    rango_etario = case_when(
      CH06 < 19  ~ "Menor a 19",
      CH06 >= 19 & CH06 <= 25 ~ "19 a 25",
      CH06 >= 26 & CH06 <= 35 ~ "26 a 35",
      CH06 >= 36 & CH06 <= 45 ~ "36 a 45",
      CH06 >= 46 & CH06 <= 55 ~ "46 a 55",
      CH06 >= 56 & CH06 <= 65 ~ "56 a 65",
      CH06 >= 66 & CH06 <= 75 ~ "66 a 75",
      CH06 >= 76 & CH06 <= 85 ~ "76 a 85",
      CH06 > 85  ~ "Mayor a 85",
      TRUE ~ NA_character_  # Para valores faltantes
    )
  )
#Nivel educativo con desagregación de superior incompleta
base <- base %>%
  mutate(
    nivel.ed = factor(
      case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                NIVEL_ED %in% 4 ~ "Secundaria Completa",
                NIVEL_ED == 5 ~ "Superior Incompleto",
                NIVEL_ED == 6 ~ "Superior Completo",
                TRUE ~ "Ns/Nr"),
      levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo")))


#Se propone una nueva forma de construir la variable establecimiento incluyendo:
#las preguntas de rescate y a las asalariadas de casas particulares
#Armo las variables de establecimiento####


base <- base %>% organize_caes()  #labels rama segun caes
base <- organize_cno(base)
#con lo anterior corre cuadro 1 


base <- base %>%
  filter(ESTADO == 1) %>%
  mutate(tamanio.establec.nueva = case_when(
    PP04C == 1 ~ "uni",
    PP04C %in% c(2, 3, 4, 5) ~ "peque",
    PP04C %in% c(6, 7, 8) ~ "mediano",
    PP04C %in% c(9, 10, 11, 12) ~ "grande",
    PP04C99 == 1 ~ "peque",
    PP04C99 == 2 ~ "mediano",
    PP04C99 == 3 ~ "grande",
    PP04C99 == 9 ~ "NS/NR",
    TRUE ~ NA_character_  # Captura valores no definidos
  ))

# Verificar si la variable fue creada correctamente
print(table(base$tamanio.establec.nueva, useNA = "always"))
                                                                                   
 #PP04B1==1 ~ "casaparticular"))

#Cambios en las variables del puesto####

#Resultado de puesto y de estableccimiento

#ANTIGUEDAD. Dado que PP07A y PP05H son variables categóricas...

base <- base %>%
  mutate(
    antiguedad_empleo = case_when(
      as_factor(PP07A) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP07A),#asalariados
      as_factor(PP05H) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP05H) #no asalariados (independientes/cuentapropia)
    ),
    antiguedad_empleo = factor(
      antiguedad_empleo, 
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("menor a 1 mes", "1 a 3 meses", "más de 3 a 6 meses", 
                 "más de 6 a 12 meses", "más de 1 año a 5 años", "más de 5 años")
    )
  )

#hata aca corre cuadro 2







#Criterio precariedad de la clase. creamos variables en base

base <- base %>%  
  filter(ESTADO == 1, CAT_OCUP == 3) %>%  # Ocupados asalariados
  mutate(
    # Signo de precariedad tecnológica y de calificación
    preca_tecno_calif = case_when(
      TECNOLOGIA == 1 & CALIFICACION == 1 ~ 4,  # Solo cuando ambos son 1
      TRUE ~ 0 ),
    
    # Clasificación de educación
    nivel.ed = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
      NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    
    # Clasificación de tamaño del establecimiento
    tamanio.establec = factor(case_when(
      PP04C %in% 1:6  ~ "Pequeño",
      PP04C %in% 7:8  ~ "Mediano",
      PP04C %in% 9:12 ~ "Grande",
      PP04C %in% 99   ~ "Ns/Nr"
    ), levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
    
    # Descuento jubilatorio
    descuento_jubil = case_when(PP07H == 1 ~ "Si", PP07H == 2 ~ "No"),
    
    # Part-time involuntario
    part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si", TRUE ~ "No"),
    
    # Tiempo determinado
    tiempo.determinado = case_when(PP07C == 1 ~ "Si", TRUE ~ "No"),
    
    # Signos de precariedad
    signo_educ_tamaño = as.integer(nivel.ed == "Menor a Secundaria" & tamanio.establec == "Pequeño"),  # 1er signo
    signo_sindescuento = as.integer(descuento_jubil == "No"),  # 2do signo
    signo_tiempo = as.integer(part.time.inv == "Si" & tiempo.determinado == "No"),  # 3er signo
    signo_tecno_calif = as.integer(preca_tecno_calif == 4),  # 4to signo
    
    # Total de signos de precariedad
    total_4_signos = signo_educ_tamaño + signo_sindescuento + signo_tiempo + signo_tecno_calif,
    
    # Al menos 1 de 4 signos
    almenos1de4 = as.integer(total_4_signos >= 1),
    
    # Al menos 1 de 3 signos (sin contar tecnología y calificación)
    almenos1de3 = as.integer(signo_educ_tamaño + signo_sindescuento + signo_tiempo >= 1),
    
    # Sin precariedad en los primeros 3 signos
    sin_preca_de3 = as.integer(signo_educ_tamaño + signo_sindescuento + signo_tiempo == 0),
    
    # Sin precariedad en los 4 signos
    sin_preca_de4 = as.integer(total_4_signos == 0)
  )






