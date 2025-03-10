
# Librerías necesarias
library(dplyr)
library(ggplot2)
library(plotly)


# Transformar los datos para que sean adecuados para ggplot
c.1.1_grafico <- c.1.1_consolidado %>%
  select(ESTADO, anio_trim, Prop_Varones, Prop_Mujeres) %>%
  pivot_longer(cols = c(Prop_Varones, Prop_Mujeres), 
               names_to = "Sexo", 
               values_to = "Proporcion")

# Gráfico
ggplot(c.1.1_grafico, aes(x = anio_trim, y = Proporcion, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  geom_text(aes(label = scales::percent(Proporcion, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +  # Agregar etiquetas de datos
  facet_wrap(~ESTADO) +  # Un gráfico por cada ESTADO
  labs(title = "Proporción de Varones y Mujeres por Estado y Trimestre",
       x = "Trimestre",
       y = "Proporción",
       fill = "Sexo") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Formato porcentaje
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
  geom_text(aes(label = scales::percent(Proporcion, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +  # Agregar etiquetas de datos
  facet_wrap(~CAT_OCUP) +  # Un gráfico por cada categoría ocupacional
  labs(title = "Proporción de Varones y Mujeres por Categoría Ocupacional y Trimestre",
       x = "Trimestre",
       y = "Proporción",
       fill = "Sexo") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Formato porcentaje
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
<<<<<<< HEAD














############################################################################

#C.091


# Transformar los datos para graficar
c.91_grafico <- c.91_precaSS_educ_sexo_final %>%
  pivot_longer(cols = -c(anio_trim, nivel.ed1, sexo), 
               names_to = "Variable", 
               values_to = "Valor")

# Gráfico de barras agrupadas con ggplot2
p <- ggplot(c.91_grafico, aes(x = as.factor(anio_trim), y = Valor, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  facet_wrap(~nivel.ed1, scales = "free_y") +  # Un gráfico por nivel educativo
  labs(title = "Evolución de la Precarización Laboral por Nivel Educativo y Sexo",
       x = "Trimestre",
       y = "Proporción",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(Valor, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3)

# Convertir a gráfico interactivo con plotly
ggplotly(p)

=======
>>>>>>> cecd632063978dd9df38197b645dd8fcfe27750e
