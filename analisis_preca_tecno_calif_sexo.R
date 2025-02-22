
# Analizar la precariedad tecno_calif por sexo

tabla_preca_tecno_calif_sexo <- calculate_tabulates(
  base = base,
  x = "CH04",
  y = "preca_tecno_calif",
  weights = "PONDERA"
)

print(tabla_preca_tecno_calif_sexo)

# Analizar la precariedad por nivel educativo
tabla_preca_tecno_calif_educ <- calculate_tabulates(
  base = base,
  x = "NIVEL_ED",
  y = "preca_tecno_calif",
  weights = "PONDERA"
)
print(tabla_preca_tecno_calif_educ)





#Dado que PP07A y PP05H son variables categóricas

base <- base %>%
  mutate(
    antiguedad_empleo = case_when(
      as_factor(PP07A) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP07A),#asalariados
      as_factor(PP05H) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP05H) #no asalariados (independientes/cuentapropia)
    ),
    antiguedad_empleo = factor(
      antiguedad_empleo, 
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("menor a 1 mes", "1 a 3 meses", "más de 3 a 6 meses", 
                 "más de 6 a 12 meses", "más de 1 año a 5 años", "más de 5 años")
    )
  )
    



tabla_precaTC_ant <- calculate_tabulates(
  base = base,
  x = "antiguedad_empleo",
  y = "preca_tecno_calif",
  weights = "PONDERA") %>% 
  mutate(sexo = "Total")
print(tabla_precaTC_ant)

  
  tabla_precaTC_ant_varon <- base %>% 
    filter(CH04 == "1") %>%  # Filtrás solo varones
    calculate_tabulates(
      x = "antiguedad_empleo",
      y = "preca_tecno_calif",
      weights = "PONDERA")     %>% 
      mutate(sexo = "Varón")
  print(tabla_precaTC_ant_varon)


  tabla_precaTC_ant_mujer <- base %>% 
    filter(CH04 == "2") %>%  # Filtrás solo varones
    calculate_tabulates(
      x = "antiguedad_empleo",
      y = "preca_tecno_calif",
      weights = "PONDERA") %>% 
        mutate(sexo = "mujer")
  print(tabla_precaTC_ant_mujer)

  tabla_precaTC_ant_tot <- bind_rows(tabla_precaTC_ant, tabla_precaTC_ant_varon, tabla_precaTC_ant_mujer)
  

# print(tabla_precariedad_antiguedad)

#sub y sobre ocupación 

#PP3E_TOT: Total de horas trabajadas en la ocupación principal.
#PP3F_TOT: Total de horas trabajadas en otras ocupaciones.


#
# base <- base %>%
#   mutate(
#     total_horas_trabajadas = PP3E_TOT + PP3F_TOT
#   )
