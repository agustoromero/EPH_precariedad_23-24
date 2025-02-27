calcular_rama_sexo <- function(df) {
  cuadro_base <- df %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
    group_by(caes_seccion_label) %>% 
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
  
  # Calcular fila de total
  total_fila <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función
cuadro_rama_sexo_final <- calcular_rama_sexo(base)

# Mostrar resultado
print(cuadro_rama_sexo_final)


###################################################



calcular_rama_condicion_registro <- function(df) {
  cuadro_base <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3) %>%
    group_by(caes_seccion_label) %>%
    summarise(
      total = sum(PONDERA[CAT_OCUP == 3], na.rm = TRUE),
      Asal_protegidos = sum(PONDERA[CAT_OCUP == 3 & PP07H == 1], na.rm = TRUE),
      Asal_precarios_tot = sum(PONDERA[CAT_OCUP == 3 & PP07H == 2], na.rm = TRUE),
      Asal_precarios_i1_mono = sum(PONDERA[CAT_OCUP == 3 & PP07I == 1], na.rm = TRUE),
      Asal_precarios_i2_negr = sum(PONDERA[CAT_OCUP == 3 & PP07I == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_asal_prote = Asal_protegidos / total,
      part_asal_precarios = Asal_precarios_tot / total,
      part_mono_en_precarios = Asal_precarios_i1_mono / Asal_precarios_tot,
      part_negro_en_precarios = Asal_precarios_i2_negr / Asal_precarios_tot,
      part_NSNR_en_precarios = (total - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot
    )
  
  # Calcular fila de total
  total_fila <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función
cuadro_rama_condicion_registro_final <- calcular_rama_condicion_registro(base)

# Mostrar resultado
print(cuadro_rama_condicion_registro_final)
