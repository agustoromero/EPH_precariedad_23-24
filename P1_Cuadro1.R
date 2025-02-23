library(dplyr)

# Función para calcular tasas ocupacionales por grupo con totales por bloque de edad
calcular_cuadro <- function(df, sexo_filtro = NULL, sexo_label) {
  if (!is.null(sexo_filtro)) {
    df <- df %>% filter(CH04 == sexo_filtro) # Filtrar por sexo si se especifica
  }
  
  cuadro_base <- df %>% 
    group_by(rango_etario) %>% 
    summarise(
      Poblacion = sum(PONDERA, na.rm = TRUE),
      Ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
      Desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
      PEA = Ocupados + Desocupados,
      Ocupados_demand = sum(PONDERA[ESTADO == 1 & PP03J == 1], na.rm = TRUE),
      Suboc_demandante = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J == 1], na.rm = TRUE),
      Suboc_no_demand = sum(PONDERA[ESTADO == 1 & INTENSI == 1 & PP03J %in% c(2,9)], na.rm = TRUE),
      Subocupados = Suboc_demandante + Suboc_no_demand,
      Ocupados_dis_nodem = sum(PONDERA[ESTADO == 1 & PP03J == 1 & PP03I == 2], na.rm = TRUE),
      Sobreocupados = sum(PONDERA[ESTADO == 1 & INTENSI == 3], na.rm = TRUE)
    ) %>% 
    mutate(
      `Tasa Actividad` = PEA / Poblacion,
      `Tasa Empleo` = Ocupados / Poblacion,
      `Tasa Desocupacion` = Desocupados / PEA,
      `Tasa ocupados demandantes` = Ocupados_demand / PEA,
      `Tasa Subocupacion` = Subocupados / PEA,
      `Tasa Subocupacion demandante` = Suboc_demandante / PEA,
      `Tasa Subocupacion no demandante` = Suboc_no_demand / PEA
    )
  
  # Calcular fila de totales por grupo de edad
  total_fila <- cuadro_base %>%
    summarise(
      rango_etario = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Agregar columna de sexo
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>% mutate(sexo = sexo_label)
  
  return(cuadro_final)
}

# Calcular cuadros con totales por edad
cuadro_total <- calcular_cuadro(base, NULL, "Ambos")
cuadro_varones <- calcular_cuadro(base, 1, "Varón")
cuadro_mujeres <- calcular_cuadro(base, 2, "Mujer")

# Unir todos los cuadros
cuadro_final <- bind_rows(cuadro_total, cuadro_varones, cuadro_mujeres) %>%
  mutate(across(everything(), ~ replace(., is.na(.), "-")))

# Mostrar el cuadro final
print(cuadro_final)