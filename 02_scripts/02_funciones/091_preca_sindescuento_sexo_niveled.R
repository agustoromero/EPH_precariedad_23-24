base <- base %>%  
  filter(ESTADO == 1, CAT_OCUP == 3) %>%  # Ocupados asalariados
  mutate(
    preca_signo_sindescuento = case_when(
      signo_sindescuento == 1 ~ 1,  # Si el signo de "sin descuento" es 1, marca como precario
      TRUE ~ 0  # De lo contrario, no es precario
    )
  )

# Analizar la precariedad signo_sindescuento por sexo
tabla_preca_SS_sexo <- calculate_tabulates(
  base = base,
  x = "CH04",  # Sexo (código 1 = varón, 2 = mujer)
  y = "preca_signo_sindescuento",
  weights = "PONDERA"
)

print(tabla_preca_SS_sexo)

# Analizar la precariedad signo_sindescuento por nivel educativo (ambos sexos)
tabla_preca_SS_educ <- calculate_tabulates(
  base = base,
  x = "NIVEL_ED",
  y = "preca_signo_sindescuento",
  weights = "PONDERA"
) %>%
  mutate(sexo = "Ambos")
print(tabla_preca_SS_educ)

tabla_precaSS_educ_varon <- base %>% 
  filter(CH04 == "1") %>%  # Solo varones
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "preca_signo_sindescuento",
    weights = "PONDERA"
  ) %>% 
  mutate(sexo = "Varón")
print(tabla_precaSS_educ_varon)

tabla_precaSS_educ_mujer <- base %>% 
  filter(CH04 == "2") %>%  # Solo mujeres
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "preca_signo_sindescuento",
    weights = "PONDERA"
  ) %>% 
  mutate(sexo = "Mujer")
print(tabla_precaSS_educ_mujer)

tabla_precaSS_educ_sexo_final <- bind_rows(tabla_preca_SS_educ, tabla_precaSS_educ_varon, tabla_precaSS_educ_mujer)

# Limpiar el entorno de trabajo de las variables no necesarias
objetos <- c("tabla_precaSS_educ_varon", "tabla_precaSS_educ_mujer", "tabla_preca_SS_sexo")
rm(list = intersect(objetos, ls()))
