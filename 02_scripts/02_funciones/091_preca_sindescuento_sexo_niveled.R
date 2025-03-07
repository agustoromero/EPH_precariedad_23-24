# Analizar la precariedad signo_sindescuento por sexo
tabla_preca_SS_sexo <- calculate_tabulates(
  base = base_asalariados,
  x = "CH04",  # Sexo (código 1 = varón, 2 = mujer)
  y = "signo_sindescuento",
  weights = "PONDERA"
)

print(tabla_preca_SS_sexo)

# Analizar la precariedad signo_sindescuento por nivel educativo (ambos sexos)
tabla_preca_SS_educ <- calculate_tabulates(
  base = base_asalariados,
  x = "NIVEL_ED",
  y = "signo_sindescuento",
  weights = "PONDERA"
) %>%
  mutate(sexo = "Ambos")
print(tabla_preca_SS_educ)

tabla_precaSS_educ_varon <- base_asalariados %>% 
  filter(CH04 == "1") %>%  # Solo varones
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "signo_sindescuento",
    weights = "PONDERA"
  ) %>% 
  mutate(sexo = "Varón")
print(tabla_precaSS_educ_varon)

tabla_precaSS_educ_mujer <- base_asalariados %>% 
  filter(CH04 == "2") %>%  # Solo mujeres
  calculate_tabulates(
    x = "NIVEL_ED",
    y = "signo_sindescuento",
    weights = "PONDERA"
  ) %>% 
  mutate(sexo = "Mujer")
print(tabla_precaSS_educ_mujer)

c.91_precaSS_educ_sexo_final <- bind_rows(tabla_preca_SS_educ, tabla_precaSS_educ_varon, tabla_precaSS_educ_mujer)

# Limpiar el entorno de trabajo de las variables no necesarias
objetos <- c("tabla_precaSS_educ_varon", "tabla_precaSS_educ_mujer", "tabla_preca_SS_sexo", "tabla_preca_SS_educ")
rm(list = intersect(objetos, ls()))
