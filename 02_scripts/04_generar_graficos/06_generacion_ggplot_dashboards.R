# Supongamos que los cuadros ahora incluyen la columna "periodo" con el formato "YYYY_T"
# Unir los cuadros en una misma estructura con la columna TRIMESTRE y ANO4
evolucion_c1.1 <- c.1.1_estado_final %>%
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = "_")) %>%
  pivot_longer(cols = starts_with("Poblacion_"), names_to = "Sexo", values_to = "Poblacion")

evolucion_c1.2 <- c.1.2_cat_ocup_final %>%
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = "_")) %>%
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
