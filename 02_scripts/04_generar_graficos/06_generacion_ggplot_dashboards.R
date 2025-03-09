# Supongamos que los cuadros ahora incluyen la columna "periodo" con el formato "YYYY_T"
# Unir los cuadros en una misma estructura con la columna TRIMESTRE y ANO4
evolucion_c1.1 <- c.1.1_estado_final %>%
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = "")) %>%
  pivot_longer(cols = starts_with("Poblacion_"), names_to = "Sexo", values_to = "Poblacion")

evolucion_c1.2 <- c.1.2_cat_ocup_final %>%
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = "")) %>%
  pivot_longer(cols = starts_with("Poblacion_"), names_to = "Sexo", values_to = "Poblacion")

# Gráfico de líneas para la evolución de `c.1.1`
ggplot(evolucion_c1.1, aes(x = periodo, y = Poblacion, color = ESTADO, group = ESTADO)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Sexo, scales = "free_y") +
  labs(title = "Evolución de la población por estado", x = "Trimestre", y = "Población") +
  theme_minimal()

# Gráfico de líneas para la evolución de `c.1.2`
ggplot(evolucion_c1.2, aes(x = periodo, y = Poblacion, color = CAT_OCUP, group = CAT_OCUP)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Sexo, scales = "free_y") +
  labs(title = "Evolución de la población por categoría ocupacional", x = "Trimestre", y = "Población") +
  theme_minimal()


generar_grafico_evolucion <- function(df, variable_agrupadora, titulo) {
  df %>%
    mutate(periodo = as.character(anio_trim)) %>%  # Usar el nombre correcto de la columna
    pivot_longer(cols = starts_with("Poblacion_"), names_to = "Sexo", values_to = "Poblacion") %>%
    ggplot(aes(x = periodo, y = Poblacion, color = !!sym(variable_agrupadora), group = !!sym(variable_agrupadora))) +
    geom_line() +
    geom_point() +
    facet_wrap(~Sexo, scales = "free_y") +
    labs(title = titulo, x = "Trimestre", y = "Población") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Prueba la función con el dataset corregido
generar_grafico_evolucion(c.1.1_estado_final, "ESTADO", "Evolución de la población por estado")

