base <- base %>% mutate(PP04A = as.character(PP04A))

calcular_estab_condicion_registro <- function(df, tipo_estab = NULL, etiqueta_estab) {
  # Filtrar asalariados ocupados
  df_filtrado <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3)
  
  # Si se especifica un tipo de establecimiento, filtrar
  if (!is.null(tipo_estab)) {
    df_filtrado <- df_filtrado %>% filter(PP04A == tipo_estab)
  }
  
  cuadro_base <- df_filtrado %>%
    group_by(caes_seccion_label) %>%
    summarise(
      total = sum(PONDERA, na.rm = TRUE),
      Asal_protegidos = sum(PONDERA[PP07H == 1], na.rm = TRUE),
      Asal_precarios_tot = sum(PONDERA[PP07H == 2], na.rm = TRUE),
      Asal_precarios_i1_mono = sum(PONDERA[PP07H == 2 & PP07I == 1], na.rm = TRUE),
      Asal_precarios_i2_negr = sum(PONDERA[PP07H == 2 & PP07I == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_asal_prote = Asal_protegidos / total,
      part_asal_precarios = Asal_precarios_tot / total,
      part_mono_en_precarios = Asal_precarios_i1_mono / Asal_precarios_tot,
      part_negro_en_precarios = Asal_precarios_i2_negr / Asal_precarios_tot,
      part_NSNR_en_precarios = (Asal_precarios_tot - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot
    )
  
  # Calcular fila de total para ese tipo de establecimiento
  total_fila <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Agregar columna de establecimiento
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(tipo_establecimiento = etiqueta_estab)
  
  return(cuadro_final)
}

# Calcular cuadros con totales por tipo de establecimiento
cuadro_total <- calcular_estab_condicion_registro(base, NULL, "Total")
cuadro_estatal <- calcular_estab_condicion_registro(base, 1, "Estatal")
cuadro_privado <- calcular_estab_condicion_registro(base, 2, "Privado")
cuadro_otro <- calcular_estab_condicion_registro(base, 3, "Otro")

# Unir todos los cuadros
cuadro_final <- bind_rows(cuadro_total, cuadro_estatal, cuadro_privado, cuadro_otro) %>%
  mutate(across(everything(), ~ replace(., is.na(.), "-")))

# Mostrar el cuadro final
print(cuadro_final)
