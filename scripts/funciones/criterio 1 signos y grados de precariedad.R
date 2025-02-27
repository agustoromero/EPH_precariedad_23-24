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




# Función para el cuadro general de ambos (sin separar por sexo)
cuadro_precariedad_ambos <- function(df) {
  indicadores <- df %>%
    summarise(
      Total = n(),
      "Signo 1: Educación-Tamaño" = sum(signo_educ_tamaño, na.rm = TRUE),
      "Signo 2: Sin Descuento Jubilatorio" = sum(signo_sindescuento, na.rm = TRUE),
      "Signo 3: Tiempo (PT involuntario + Indeterminado)" = sum(signo_tiempo, na.rm = TRUE),
      "Signo 4: Tecnología-Calificación" = sum(signo_tecno_calif, na.rm = TRUE),
      "Total con al menos 1 de 3" = sum(almenos1de3, na.rm = TRUE),
      "Total sin signos de precariedad" = sum(sin_preca_de3, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = -Total, names_to = "Indicador", values_to = "Frecuencia_ambos") %>%
    mutate(Proporcion_ambos = round(Frecuencia_ambos / Total * 100, 2)) 
  
  # Reorganizar y pivotar
  indicadores <- indicadores %>%
    pivot_longer(cols = c(Frecuencia_ambos, Proporcion_ambos), names_to = "Tipo", values_to = "Valor") %>%
    pivot_wider(names_from = Tipo, values_from = Valor) %>%
    select(Indicador, Frecuencia_ambos, Proporcion_ambos)
  
  return(indicadores)
}

# Función para el cuadro por sexo
cuadro_precariedad_sexo <- function(df) {
  indicadores <- df %>%
    group_by(CH04) %>%  # Suponiendo que CH04 es la columna de sexo
    summarise(
      Total = n(),
      "Signo 1: Educación-Tamaño" = sum(signo_educ_tamaño, na.rm = TRUE),
      "Signo 2: Sin Descuento Jubilatorio" = sum(signo_sindescuento, na.rm = TRUE),
      "Signo 3: Tiempo (PT involuntario + Indeterminado)" = sum(signo_tiempo, na.rm = TRUE),
      "Signo 4: Tecnología-Calificación" = sum(signo_tecno_calif, na.rm = TRUE),
      "Total con al menos 1 de 3" = sum(almenos1de3, na.rm = TRUE),
      "Total sin signos de precariedad" = sum(sin_preca_de3, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = -c(Total, CH04), names_to = "Indicador", values_to = "Frecuencia") %>%
    mutate(Proporcion = round(Frecuencia / Total * 100, 2)) %>%
    pivot_wider(names_from = CH04, values_from = c(Frecuencia, Proporcion)) 
  
  indicadores <- indicadores %>%
    select(Indicador, starts_with("Frecuencia"), starts_with("Proporcion"))
  
  return(indicadores)
}

# Llamar ambas funciones
cuadro_general <- cuadro_precariedad_ambos(base)
cuadro_sexo <- cuadro_precariedad_sexo(base)

# Combinar los resultados con bind_rows
cuadro_signos_preca_final <- bind_rows(cuadro_general, cuadro_sexo)
rm(cuadro_general, cuadro_sexo)
# Ver el resultado
cuadro_signos_preca_final