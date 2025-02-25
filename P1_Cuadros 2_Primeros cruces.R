calcular_niveled_sexo_edad <- function(df) {
  cuadro_base <- df %>% 
    group_by(rango_etario, nivel.ed) %>% 
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    group_by(rango_etario) %>% 
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / sum(Poblacion_Varones, na.rm = TRUE),
      Prop_Mujeres = Poblacion_Mujeres / sum(Poblacion_Mujeres, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  # Calcular fila de total por rango etario
  total_fila <- cuadro_base %>%
    group_by(rango_etario) %>%
    summarise(
      nivel.ed = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función
cuadro_niveled_edad_final <- calcular_niveled_sexo_edad(base)

# Mostrar resultado
print(cuadro_niveled_edad_final)




#nivel ed y tamaño establecimiento
C_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")
Cvar_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3 & CH04==1)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                y="tamanio.establec.nueva",
                      weights="PONDERA")
Cmuj_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3 & CH04==2)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")



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
