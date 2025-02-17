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
#Calificacion_ocupaciones

base_cno <- organize_cno(base)

base_cno <- base_cno %>%  
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
   mutate(
     precariedad_cno = case_when(
       digito4 == 1 | 
       digito5 %in% c(3, 4) ~ 1,
       TRUE ~ 0
     ))

# Analizar la precariedad por sexo
tabla_precariedad_sexo <- calculate_tabulates(
  base = base_con_cno,
  x = "CH04",
  y = "precariedad_cno",
  weights = "PONDERA"
)
print(tabla_precariedad_sexo)

# Analizar la precariedad por nivel educativo
tabla_precariedad_educacion <- calculate_tabulates(
  base = base_con_cno,
  x = "NIVEL_ED",
  y = "precariedad_cno",
  weights = "PONDERA"
)
print(tabla_precariedad_educacion)


##GGPLOT2
# Visualizar la precariedad por sexo
ggplot(tabla_precariedad_sexo$tabulates[[8]], aes(x = CH04, y = percentage, fill = precariedad_cno)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Precariedad Laboral por Sexo", x = "Sexo", y = "Porcentaje", fill = "Precariedad")

# Visualizar la precariedad por nivel educativo
ggplot(tabla_precariedad_educacion$tabulates[[8]], aes(x = NIVEL_ED, y = percentage, fill = precariedad_cno)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Precariedad Laboral por Nivel Educativo", x = "Nivel Educativo", y = "Porcentaje", fill = "Precariedad")


##Precariedad de tiempo

base_cno <- base %>%
  mutate(
    disponibilidad_mas_horas = case_when(
      PP03G == 1 & PP03H %in% c(1, 2) ~ "Disponible",
      TRUE ~ "No Disponible"
    )
  )
#Aquí, PP03G == 1 & PP03H %in% c(1, 2) verifica si el encuestado quería trabajar 
#más horas y si podía empezar a trabajarlas en un plazo de dos semanas o inmediatamente

base_con_cno <- base_con_cno %>%
  mutate(
    antiguedad_en_empleo = case_when(
      !is.na(PP07A) & PP07A %in% c(1:6) ~ PP07A,  # Asalariados
      !is.na(PP05H) & PP05H %in% c(1:6) ~ PP05H,  # Independientes
      TRUE ~ NA_real_  # Otros casos
    )
  )
#Dado que PP07A y PP05H son variables categóricas

#análisis tiempo
tabla_precariedad_horas <- calculate_tabulates(
  base = base_con_cno,
  x = "disponibilidad_mas_horas",
  y = "precariedad_cno",
  weights = "PONDERA"
)
print(tabla_precariedad_horas)

tabla_precariedad_antiguedad <- calculate_tabulates(
  base = base_con_cno,
  x = "antiguedad_en_empleo",
  y = "precariedad_cno",
  weights = "PONDERA"
)

print(tabla_precariedad_antiguedad)

#sub y sobre ocupación 

#PP3E_TOT: Total de horas trabajadas en la ocupación principal.
#PP3F_TOT: Total de horas trabajadas en otras ocupaciones.

base_con_cno <- base_con_cno %>%
  mutate(
    total_horas_trabajadas = PP3E_TOT + PP3F_TOT
  )

base_con_cno <- base_con_cno %>%
  mutate(
    categoria_ocupacional = case_when(
      total_horas_trabajadas < 35 ~ "Subocupado",
      total_horas_trabajadas > 45 ~ "Sobreocupado",
      TRUE ~ "Ocupado Pleno"
    )
  )
#PP03G (quería trabajar más horas) y PP03J (buscó otro empleo)
base_con_cno <- base_con_cno %>%
  mutate(
    condicion_demanda = case_when(
      PP03G == 1 | PP03J == 1 ~ "Demandante",
      TRUE ~ "No Demandante"
    )
  )

base_con_cno <- base_con_cno %>%
  mutate(
    clasificacion_final = paste(categoria_ocupacional, condicion_demanda, sep = " - ")
  )

tabla_clasificacion_final <- calculate_tabulates(
  base = base_con_cno,
  x = "clasificacion_final",
  weights = "PONDERA"
)
print(tabla_clasificacion_final)