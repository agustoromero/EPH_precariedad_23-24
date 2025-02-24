
# Agrupar por sexo y cantidad de signos de precariedad
cuadro_precariedad <- base %>%
  group_by(CH04, total_signos) %>%
  summarise(
    total = sum(PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sexo = case_when(CH04 == 1 ~ "Varón",
                     CH04 == 2 ~ "Mujer"),
    total_signos = factor(total_signos, levels = 0:3, labels = c("0 signos", "1 signo", "2 signos", "3 signos"))
  ) %>%
  select(sexo, total_signos, total) %>%
  arrange(sexo, total_signos)

# Agregar la categoría "Ambos" sumando Varón + Mujer
cuadro_ambos <- cuadro_precariedad %>%
  group_by(total_signos) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sexo = "Ambos")

# Calcular el total general sumando todos los valores
total_general <- cuadro_precariedad %>%
  summarise(
    sexo = "Total",
    total_signos = "Total",
    total = sum(total, na.rm = TRUE)
  )

# Unir todas las categorías y agregar la fila de total general
cuadro_final <- bind_rows(cuadro_precariedad, cuadro_ambos, total_general) %>%
  arrange(sexo, total_signos)

# Mostrar tabla final
print(cuadro_final)

# Crear una función para calcular la tabla por cada tamaño de establecimiento
calcular_tabla_por_tamanio <- function(base, tamanio) {
  base_filtrada <- base %>%
    filter(tamanio.establec == tamanio) %>%
    group_by(CH04, total_signos) %>%
    summarise(
      total = sum(PONDERA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sexo = case_when(CH04 == 1 ~ "Varón",
                       CH04 == 2 ~ "Mujer"),
      total_signos = factor(total_signos, levels = 0:3, labels = c("0 signos", "1 signo", "2 signos", "3 signos"))
    ) %>%
    select(sexo, total_signos, total) %>%
    arrange(sexo, total_signos)
  
  # Agregar la categoría "Ambos" sumando Varón + Mujer
  cuadro_ambos <- base_filtrada %>%
    group_by(total_signos) %>%
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(sexo = "Ambos")
  
  # Calcular el total general sumando todos los valores
  total_general <- base_filtrada %>%
    summarise(
      sexo = "Total",
      total_signos = "Total",
      total = sum(total, na.rm = TRUE)
    )
  
  # Unir todas las categorías y agregar la fila de total general
  cuadro_final <- bind_rows(base_filtrada, cuadro_ambos, total_general) %>%
    arrange(sexo, total_signos)
  
  # Agregar el nombre del tamaño de establecimiento como una columna
  cuadro_final <- cuadro_final %>%
    mutate(tamanio.establec = tamanio)
  
  return(cuadro_final)
}

# Calcular las tablas para cada tamaño de establecimiento
cuadro_pequeno <- calcular_tabla_por_tamanio(base, "Pequeño")
cuadro_mediano <- calcular_tabla_por_tamanio(base, "Mediano")
cuadro_grande <- calcular_tabla_por_tamanio(base, "Grande")

# Unir todas las tablas por tamaño de establecimiento
cuadro_final_completo <- bind_rows(cuadro_pequeno, cuadro_mediano, cuadro_grande)

# Mostrar la tabla final
print(cuadro_final_completo)
