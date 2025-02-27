calcular_poblacion_estado <- function(df) {
  # Asegurarnos de que ESTADO sea un factor
  df$ESTADO <- as.factor(df$ESTADO)
  
#Calcular totales por ESTADO (0, 1, 2, 3, 4) y por sexo (Ambos, Varones, Mujeres)
cuadro_estado_sexo <- df %>%
  group_by(ESTADO) %>%
  summarise(
    Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
    Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
    Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
    .groups = "drop"
  )

# Asignar los nombres correspondientes a los niveles del factor
levels(cuadro_estado_sexo$ESTADO) <- c("Entrevista no realizada", "Ocupado", "Desocupado", "Inactivo", "Menor de 10 años")

# Reemplazar NAs por "-" en las columnas de población
cuadro_estado_sexo <- cuadro_estado_sexo %>%
  mutate(across(Poblacion_Ambos:Poblacion_Mujeres, ~ replace(., is.na(.), "-")))

return(cuadro_estado_sexo)
}

# Aplicar la función al dataset 'base'
cuadro_estado_final <- calcular_poblacion_estado(base)

# Mostrar el resultado
print(cuadro_estado_final)




##################################




calcular_poblacion_estado_cat_ocup <- function(df) {
  # Filtrar solo los registros con ESTADO == 1 (Ocupado)
  df_ocupado <- df %>%
    filter(ESTADO == 1)
  
  # Asegurarnos de que ESTADO y CAT_OCUP sean factores
  df_ocupado$ESTADO <- as.factor(df_ocupado$ESTADO)
  df_ocupado$CAT_OCUP <- as.factor(df_ocupado$CAT_OCUP)
  
  # Calcular los totales por CAT_OCUP (solo para ESTADO == 1) y por sexo (Ambos, Varones, Mujeres)
  cuadro_cat_ocup <- df_ocupado %>%
    group_by(CAT_OCUP) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Asignar nombres a las categorías de CAT_OCUP
  levels(cuadro_cat_ocup$CAT_OCUP) <- c("Patrón", "Cuenta propia", "Obrero o empleado", 
                                                "Trabajador familiar sin remuneración", "Ns./Nr.")
  
  # Reemplazar NAs por "-" en las columnas de población
  cuadro_cat_ocup <- cuadro_cat_ocup %>%
    mutate(across(Poblacion_Ambos:Poblacion_Mujeres, ~ replace(., is.na(.), "-")))
  
  return(cuadro_cat_ocup)
}

# Aplicar la función al dataset 'base'
cuadro_cat_ocup_final <- calcular_poblacion_estado_cat_ocup(base)

# Mostrar el resultado
print(cuadro_cat_ocup_final)
