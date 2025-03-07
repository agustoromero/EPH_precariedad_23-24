
calcular_estab_condicion_registro <- function(df, tipo_estab = NULL, etiqueta_estab) {
  if (!is.null(tipo_estab)) {
    df <- df %>% filter(PP04A == tipo_estab)  # Filtrar por tipo de establecimiento si se especifica
  }
  
  cuadro_base <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3) %>%
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
  
  # Calcular fila de total por establecimiento
  total_fila <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Agregar columna de establecimiento
  cuadro_estab_condicion_registro <- bind_rows(cuadro_base, total_fila) %>%
    mutate(tipo_establecimiento = etiqueta_estab)
  
  return(cuadro_estab_condicion_registro)
}

# Calcular cuadros con totales por tipo de establecimiento
cuadro_total <- calcular_estab_condicion_registro(base, NULL, "Total")
cuadro_estatal <- calcular_estab_condicion_registro(base, 1, "Estatal")
cuadro_privado <- calcular_estab_condicion_registro(base, 2, "Privado")
cuadro_otro <- calcular_estab_condicion_registro(base, 3, "Otro")

# Unir todos los cuadros
c.6_estab_condicion_registro_tipo_final <- bind_rows(cuadro_total, cuadro_estatal, cuadro_privado, cuadro_otro) %>%
  mutate(across(everything(), ~ replace(., is.na(.), "-")))
# Eliminar los objetos intermedios con rm()
rm(cuadro_total, cuadro_estatal, cuadro_privado, cuadro_otro)
# Mostrar el cuadro final
print(c.6_estab_condicion_registro_tipo_final)