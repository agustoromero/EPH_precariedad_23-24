
calcular_niveled_tamanio_sexo <- function(df) {
  cuadro_base <- df %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
    group_by(nivel.ed, tamanio.establec.nueva) %>% 
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  # Calcular fila de total por nivel educativo
  total_niveled <- cuadro_base %>%
    group_by(nivel.ed) %>%
    summarise(
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calcular fila de total general
  total_general <- cuadro_base %>%
    summarise(
      nivel.ed = "Total",
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_niveled, total_general) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función
c.4_niveled_tamanio_sexo_final <- calcular_niveled_tamanio_sexo(base_asalariados)

# Mostrar resultado
print(c.4_niveled_tamanio_sexo_final)