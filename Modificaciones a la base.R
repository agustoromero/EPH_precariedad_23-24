#Librerys####
library(tidyverse)
library(openxlsx)
library(eph)


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
base <- base %>% organize_caes()  

base <- base %>%  filter(ESTADO==1) %>%  mutate(tamanio.establec.nueva = case_when(PP04C== 1~ "uni",
                                                                                   PP04C== 2~ "peque",
                                                                                   PP04C== 3~ "peque",
                                                                                   PP04C== 4~ "peque",
                                                                                   PP04C== 5~ "peque",
                                                                                   PP04C== 6~ "mediano",
                                                                                   PP04C== 7~ "mediano",
                                                                                   PP04C== 8~ "mediano",
                                                                                   PP04C== 9~ "grande",
                                                                                   PP04C== 10~ "grande",
                                                                                   PP04C== 11~ "grande",
                                                                                   PP04C== 12~ "grande",
                                                                                   PP04C99==1 ~ "peque",
                                                                                   PP04C99==2 ~ "mediano",
                                                                                   PP04C99== 3 ~ "grande",
                                                                                   PP04C99==9 ~ "NS/NR",
                                                                                   PP04B1==1 ~ "casaparticular"))
#Cambios en las variables del puesto####
#descuento jubilatorio
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(   descuento_jubil = case_when(PP07H == 1 ~ "Protegido",
                                PP07H == 2 ~ "No-protegido"))
#aportes propios al interior de los asalariados precarios
base <- base %>%
      filter(ESTADO == 1, CAT_OCUP == 3, PP07H ==2) %>% # Ocupados asalariados
      mutate(aportes_propios = case_when(PP07I == 1 ~ "Monotributista",
                                         PP07I == 2 ~ "Negro"))
        
#part time involuntario
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate( part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si",
                              TRUE ~ "No"))
#Tiempo determinado
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(tiempo.determinado = case_when(PP07C ==  1 ~ "Si",
                                   TRUE ~ "No"))
#Calificacion_ocupaciones

base <- organize_cno(base)

base <- base %>%  
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
   mutate(
     preca_tecno_calif = case_when(
       TECNOLOGIA == 1 | 
       CALIFICACION %in% c(3, 4) ~ 1,
       TRUE ~ 0
     ))


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
  