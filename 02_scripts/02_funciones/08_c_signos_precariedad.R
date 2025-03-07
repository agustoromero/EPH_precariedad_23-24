#Criterio precariedad de la clase. creamos variables en base


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
cuadro_general <- cuadro_precariedad_ambos(base_asalariados)
cuadro_sexo <- cuadro_precariedad_sexo(base_asalariados)

# Combinar los resultados con bind_rows
c.8_signos_preca_final <- bind_rows(cuadro_general, cuadro_sexo)
rm(cuadro_general, cuadro_sexo)
# Ver el resultado
c.8_signos_preca_final