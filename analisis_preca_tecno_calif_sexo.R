
# Analizar la precariedad tecno_calif por sexo

tabla_preca_TC_sexo <- calculate_tabulates(
  base = base,
  x = "CH04",
  y = "preca_tecno_calif",
  weights = "PONDERA"
)

print(tabla_preca_TC_sexo)


# Precariedad_tecno_calif_rango_etario

tabla_preca_TC_edad <- calculate_tabulates(
  base = base,           #ambos sexos
  x = "rango_etario",
  y = "preca_tecno_calif",
  weights = "PONDERA" ) %>% 
  mutate(sexo="Ambos")

print(tabla_preca_TC_edad)


tabla_precaTC_edad_varon <- base %>% 
  filter(CH04 == "1") %>%  # solo varones
  calculate_tabulates(
    x = "antiguedad_empleo",
    y = "preca_tecno_calif",
    weights = "PONDERA")     %>% 
  mutate(sexo = "Varón")
print(tabla_precaTC_edad_varon)


tabla_precaTC_edad_mujer <- base %>% 
  filter(CH04 == "2") %>%  #  solo mujeres
  calculate_tabulates(
    x = "antiguedad_empleo",
    y = "preca_tecno_calif",
    weights = "PONDERA") %>% 
  mutate(sexo = "mujer")
print(tabla_precaTC_edad_mujer)

tabla_precaTC_edad_sexo_tot <- bind_rows(tabla_preca_tecno_calif_edad, tabla_precaTC_edad_varon, tabla_precaTC_edad_mujer)





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


#Precariedad_tecno_calif_antiguedad

tabla_precaTC_ant <- calculate_tabulates(
  base = base,                 #ambos sexos
  x = "antiguedad_empleo",
  y = "preca_tecno_calif",
  weights = "PONDERA") %>% 
  mutate(sexo = "Total")
print(tabla_precaTC_ant)

  
  tabla_precaTC_ant_varon <- base %>% 
    filter(CH04 == "1") %>%  # solo varones
    calculate_tabulates(
      x = "antiguedad_empleo",
      y = "preca_tecno_calif",
      weights = "PONDERA")     %>% 
      mutate(sexo = "Varón")
  print(tabla_precaTC_ant_varon)

  tabla_precaTC_ant_mujer <- base %>%  
    filter(CH04 == "2") %>%  # Solo mujeres
    calculate_tabulates(
      x = "antiguedad_empleo",
      y = "preca_tecno_calif",
      weights = "PONDERA"
    ) %>% 
    rename(total_segun_antiguedad = `0`) %>%  # Renombrar correctamente
    mutate(total_grupo = sum(total_segun_antiguedad, na.rm = TRUE)) %>% 
    mutate(sexo = "mujer")  # Agregar variable de sexo al final
  
  print(tabla_precaTC_ant_mujer)
  
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
  
  # Combinar la fila de totales con la tabla original
  tabla_precaTC_ant_mujer_con_totales <- bind_rows(tabla_precaTC_ant_mujer, total_fila)
  
  tabla_precaTC_ant_tot <- bind_rows(tabla_precaTC_ant, tabla_precaTC_ant_varon, tabla_precaTC_ant_mujer)
  
  
  # Sumar total de cada grupo
  tabla_precaTC_ant_tot <- tabla_precaTC_ant_tot %>%
    group_by(CH04, antiguedad_empleo) %>%
    mutate(total_grupo = sum(Freq, na.rm = TRUE)) %>% 
    ungroup()
  
  # Calcular el peso relativo
  total_general <- sum(tabla_precaTC_ant_tot$Freq, na.rm = TRUE)
  
  tabla_precaTC_ant_tot <- tabla_precaTC_ant_tot %>%
    mutate(porcentaje = (Freq / total_general) * 100)

  
#sub y sobre ocupación 

#PP3E_TOT: Total de horas trabajadas en la ocupación principal.
#PP3F_TOT: Total de horas trabajadas en otras ocupaciones.
#
# base <- base %>%
#   mutate(
#     total_horas_trabajadas = PP3E_TOT + PP3F_TOT
#   )
