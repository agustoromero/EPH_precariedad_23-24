#Cuadros de clasificacion precaria

# Analizar la precariedad tecno_calif por sexo

tabla_preca_TC_sexo <- calculate_tabulates(
  base = base,
  x = "CH04",
  y = "preca_tecno_calif",
  weights = "PONDERA"
)

print(tabla_preca_TC_sexo)


# Analizar la precariedad por nivel educativo
tabla_preca_TC_educ <- calculate_tabulates(
  base = base,            #ambos sexos
  x = "NIVEL_ED",
  y = "preca_tecno_calif",
  weights = "PONDERA") %>% 
  mutate(sexo = "Ambos")
print(tabla_preca_TC_educ)

tabla_precaTC_educ_varon <- base %>% 
  filter(CH04 == "1") %>%  # solo varones
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "preca_tecno_calif",
    weights = "PONDERA")     %>% 
  mutate(sexo = "Varón")
print(tabla_precaTC_educ_varon)


tabla_precaTC_educ_mujer <- base %>% 
  filter(CH04 == "2") %>%  #  solo mujeres
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "preca_tecno_calif",
    weights = "PONDERA") %>% 
  mutate(sexo = "mujer")
print(tabla_precaTC_educ_mujer)

tabla_precaTC_educ_sexo_tot <- bind_rows(tabla_preca_TC_educ, tabla_precaTC_educ_varon, tabla_precaTC_educ_mujer)





# Crear tabla para ambos sexos por rango etario
tabla_precaTC_edad <- base %>%  
  calculate_tabulates(
    x = "rango_etario", 
    y = "preca_tecno_calif", 
    weights = "PONDERA"
  ) %>% 
  rename(total_segun_rango_etario = `0`) %>%  # Renombrar correctamente
  mutate(
    total_grupo = sum(total_segun_rango_etario, na.rm = TRUE),  # Total combinado
    porcentaje_sobre_total = (total_segun_rango_etario / total_grupo) * 100,  # Porcentaje sobre total
    sexo = "Ambos"  # Agregar variable de sexo
  ) %>% 
  select(-total_grupo)  # Eliminar la columna total_grupo
print(tabla_precaTC_edad)

# Calcular fila total combinada
total_fila_edad <- tabla_precaTC_edad %>%
  summarise(
    `rango_etario/preca_tecno_calif` = "Total",
    total_segun_rango_etario = sum(total_segun_rango_etario, na.rm = TRUE),
    porcentaje_sobre_total = sum(porcentaje_sobre_total, na.rm = TRUE),
    sexo = "Total"
  )

# Combinar la fila de totales con la tabla original
tabla_precaTC_edad_con_totales <- bind_rows(tabla_precaTC_edad, total_fila_edad)

# Ver la tabla final con totales
print(tabla_precaTC_edad_con_totales)


# Crear tabla base para los varones por rango etario
tabla_precaTC_edad_varon <- base %>%  
  filter(CH04 == "1") %>%  # Solo varones
  calculate_tabulates(
    x = "rango_etario", 
    y = "preca_tecno_calif", 
    weights = "PONDERA"
  ) %>% 
  rename(total_segun_rango_etario = `0`) %>%  # Renombrar correctamente
  mutate(
    total_grupo = sum(total_segun_rango_etario, na.rm = TRUE),
    porcentaje_sobre_varones = (total_segun_rango_etario / total_grupo) * 100,
    sexo = "Varón"  # Agregar variable de sexo
  ) %>%
  select(-total_grupo)  # Eliminar la columna total_grupo
print(tabla_precaTC_edad_varon)

# Calcular fila de totales para varones
total_fila_varon_edad <- tabla_precaTC_edad_varon %>%
  summarise(
    `rango_etario/preca_tecno_calif` = "Total",
    total_segun_rango_etario = sum(total_segun_rango_etario, na.rm = TRUE),
    porcentaje_sobre_varones = sum(porcentaje_sobre_varones, na.rm = TRUE),
    sexo = "Total"
  )

# Combinar la fila de totales con la tabla original
tabla_precaTC_edad_varon_con_totales <- bind_rows(tabla_precaTC_edad_varon, total_fila_varon_edad)
print(tabla_precaTC_edad_varon_con_totales)

# Crear tabla base para las mujeres por rango etario
tabla_precaTC_edad_mujer <- base %>%  
  filter(CH04 == "2") %>%  # Solo mujeres
  calculate_tabulates(
    x = "rango_etario", 
    y = "preca_tecno_calif", 
    weights = "PONDERA"
  ) %>% 
  rename(total_segun_rango_etario = `0`) %>%  # Renombrar correctamente
  mutate(
    total_grupo = sum(total_segun_rango_etario, na.rm = TRUE),
    porcentaje_sobre_mujeres = (total_segun_rango_etario / total_grupo) * 100,
    sexo = "Mujer"  # Agregar variable de sexo
  ) %>%
  select(-total_grupo)  # Eliminar la columna total_grupo
print(tabla_precaTC_edad_mujer)

# Calcular fila de totales para mujeres
total_fila_mujer_edad <- tabla_precaTC_edad_mujer %>%
  summarise(
    `rango_etario/preca_tecno_calif` = "Total",
    total_segun_rango_etario = sum(total_segun_rango_etario, na.rm = TRUE),
    porcentaje_sobre_mujeres = sum(porcentaje_sobre_mujeres, na.rm = TRUE),
    sexo = "Total"
  )

# Combinar la fila de totales con la tabla original
tabla_precaTC_edad_mujer_con_totales <- bind_rows(tabla_precaTC_edad_mujer, total_fila_mujer_edad)
print(tabla_precaTC_edad_mujer_con_totales)


# Combinar las tablas de ambos sexos, varones y mujeres en una sola tabla final
tabla_precaTC_edad_final <- bind_rows(
  tabla_precaTC_edad_con_totales, 
  tabla_precaTC_edad_varon_con_totales, 
  tabla_precaTC_edad_mujer_con_totales
) %>%
  mutate(across(everything(), ~ replace(., is.na(.), "-")))

# Ver la tabla final
print(tabla_precaTC_edad_final)





#Precariedad_tecno_calif_antiguedad

# Crear tabla para ambos sexos
tabla_precaTC_ant <- base %>%  
  calculate_tabulates(
    x = "antiguedad_empleo",
    y = "preca_tecno_calif",
    weights = "PONDERA"
  ) %>% 
  rename(total_segun_antiguedad = `0`) %>%  # Renombrar correctamente
  mutate(
    total_grupo = sum(total_segun_antiguedad, na.rm = TRUE),  # Total combinado
    porcentaje_sobre_total = (total_segun_antiguedad / total_grupo) * 100,  # Porcentaje sobre total
    sexo = "Ambos"  # Agregar variable de sexo
  ) %>% 
  select(-total_grupo)  # Eliminar la columna total_grupo
print(tabla_precaTC_ant)

# Calcular fila total combinada
total_fila <- tabla_precaTC_ant %>%
  summarise(
    `antiguedad_empleo/preca_tecno_calif` = "Total",
    total_segun_antiguedad = sum(total_segun_antiguedad, na.rm = TRUE),
    porcentaje_sobre_total = sum(porcentaje_sobre_total, na.rm = TRUE),
    sexo = "Total"
  )

# Combinar la fila de totales con la tabla original
tabla_precaTC_ant_con_totales <- bind_rows(tabla_precaTC_ant, total_fila)

# Ver la tabla final con totales
print(tabla_precaTC_ant_con_totales)



  
# Crear tabla base para los varones
tabla_precaTC_ant_varon <- base %>%  
  filter(CH04 == "1") %>%  # Solo varones
  calculate_tabulates(
    x = "antiguedad_empleo",
    y = "preca_tecno_calif",
    weights = "PONDERA"
  ) %>% 
  rename(total_segun_antiguedad = `0`) %>%  # Renombrar correctamente
  mutate(
    total_grupo = sum(total_segun_antiguedad, na.rm = TRUE),
    porcentaje_sobre_varones = (total_segun_antiguedad / total_grupo) * 100,
    sexo = "Varón"  # Agregar variable de sexo
  ) %>%
  select(-total_grupo)  # Eliminar la columna total_grupo

print(tabla_precaTC_ant_varon)

# Calcular fila de totales para varones
total_fila_varon <- tabla_precaTC_ant_varon %>%
  summarise(
    `antiguedad_empleo/preca_tecno_calif` = "Total",
    total_segun_antiguedad = sum(total_segun_antiguedad, na.rm = TRUE),
    porcentaje_sobre_varones = sum(porcentaje_sobre_varones, na.rm = TRUE),
    sexo = "Total"
  )
# Combinar la fila 
tabla_precaTC_ant_varon_con_totales <- bind_rows(tabla_precaTC_ant_varon, total_fila_varon)
print(tabla_precaTC_ant_varon_con_totales)
  
  
  
# Crear tabla base para los mujeres  
  tabla_precaTC_ant_mujer <- base %>%  
    filter(CH04 == "2") %>%  # Solo mujeres
    calculate_tabulates(
      x = "antiguedad_empleo",
      y = "preca_tecno_calif",
      weights = "PONDERA"
    ) %>% 
    rename(total_segun_antiguedad = `0`) %>%  # Renombrar correctamente
    mutate(
      total_grupo = sum(total_segun_antiguedad, na.rm = TRUE),
      porcentaje_sobre_mujeres = (total_segun_antiguedad / total_grupo) * 100,
      sexo = "Mujer"  # Agregar variable de sexo
    ) %>%
    select(-total_grupo)  # Eliminar la columna total_grupo
  
  print(tabla_precaTC_ant_mujer)
  
  total_fila <- tabla_precaTC_ant_mujer %>%
    summarise(
      `antiguedad_empleo/preca_tecno_calif` = "Total",
      total_segun_antiguedad = sum(total_segun_antiguedad, na.rm = TRUE),
      porcentaje_sobre_mujeres = sum(porcentaje_sobre_mujeres, na.rm = TRUE),
      sexo = "Total"
    )
  # Combinar la fila con la tabla original
  tabla_precaTC_ant_mujer_con_totales <- bind_rows(tabla_precaTC_ant_mujer, total_fila)
  print(tabla_precaTC_ant_mujer_con_totales)
 
  
  
   tabla_precaTC_ant_FINAL <- bind_rows(tabla_precaTC_ant_con_totales, tabla_precaTC_ant_varon_con_totales, tabla_precaTC_ant_mujer_con_totales) %>%
     mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
#libero ambiente de trabajo
   
   objetos <- c("total_fila_edad", "total_fila_mujer_edad", "total_fila_varon", "total_fila_varon_edad", "total_fila", 
                "tabla_precaTC_edad_varon", "tabla_precaTC_edad_mujer", "tabla_precaTC_educ_varon", "tabla_precaTC_educ_mujer",
                "tabla_preca_TC_sexo", "tabla_precaTC_ant", "tabla_precaTC_ant_con_totales", 
                "tabla_precaTC_edad_varon_con_totales", "tabla_precaTC_edad_mujer_con_totales",
                "tabla_precaTC_ant_mujer", "tabla_precaTC_ant_mujer_con_totales", "tabla_precaTC_ant_varon",
                "tabla_precaTC_ant_varon_con_totales", "tabla_precaTC_edad", "tabla_precaTC_edad_con_totales",
                "tabla_precaTC_edad_mujer", "table_precaTC_edad_mujer_con_totales")
   
   rm(list = intersect(objetos, ls()))
   
   #lascosascambia