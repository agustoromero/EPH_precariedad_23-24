
# Transformar los datos para que sean adecuados para ggplot
c.1.1_grafico <- c.1.1_consolidado %>%
  select(ESTADO, anio_trim, Prop_Varones, Prop_Mujeres) %>%
  pivot_longer(cols = c(Prop_Varones, Prop_Mujeres), 
               names_to = "Sexo", 
               values_to = "Proporcion")

# Gráfico
ggplot(c.1.1_grafico, aes(x = anio_trim, y = Proporcion, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  facet_wrap(~ESTADO) +  # Un gráfico por cada ESTADO
  labs(title = "Proporción de Varones y Mujeres por Estado y Trimestre",
       x = "Trimestre",
       y = "Proporción",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Transformar los datos para que sean adecuados para ggplot
c.1.2_grafico <- c.1.2_consolidado %>%
  select(CAT_OCUP, anio_trim, Prop_Varones, Prop_Mujeres) %>%
  pivot_longer(cols = c(Prop_Varones, Prop_Mujeres), 
               names_to = "Sexo", 
               values_to = "Proporcion")

# Gráfico
ggplot(c.1.2_grafico, aes(x = anio_trim, y = Proporcion, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  facet_wrap(~CAT_OCUP) +  # Un gráfico por cada categoría ocupacional
  labs(title = "Proporción de Varones y Mujeres por Categoría Ocupacional y Trimestre",
       x = "Trimestre",
       y = "Proporción",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))