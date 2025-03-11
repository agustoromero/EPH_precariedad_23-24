
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











library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
# Cargar los datos consolidados
data <- c.3_consolidado %>%
  filter(tamanio.establec.nueva != "Total", caes_seccion_label != "Total") %>%
  mutate(
    Prop_Varones = as.numeric(Prop_Varones),
    Prop_Mujeres = as.numeric(Prop_Mujeres)
  ) %>%
  replace_na(list(Prop_Varones = 0, Prop_Mujeres = 0)) %>%
  pivot_longer(cols = c(Prop_Varones, Prop_Mujeres), 
               names_to = "Sexo", 
               values_to = "Proporcion")

# UI
ui <- fluidPage(
  titlePanel("Distribución de Asalariados por Rama y Tamaño del Establecimiento"),
  sidebarLayout(
    sidebarPanel(
      selectInput("caes_seccion_label", "Seleccionar Rama:", choices = unique(data$caes_seccion_label)),
      selectInput("tamanio", "Seleccionar Tamaño del Establecimiento:", choices = unique(data$tamanio.establec.nueva))
    ),
    mainPanel(
      plotlyOutput("grafico_prop")
    )
  )
)

# Server
server <- function(input, output) {
  datos_filtrados <- reactive({
    data %>%
      filter(caes_seccion_label == input$caes_seccion_label,
             tamanio.establec.nueva == input$tamanio)
  })
  
  output$grafico_prop <- renderPlotly({
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = anio_trim, y = Proporcion, fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = ifelse(Proporcion > 0, scales::percent(Proporcion, accuracy = 0.1), "")), 
                position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
      scale_fill_manual(values = c("Prop_Varones" = "blue", "Prop_Mujeres" = "pink")) +
      facet_wrap(~Sexo, scales = "fixed") +
      theme_minimal() +
      labs(title = "Proporción de Varones y Mujeres por Trimestre",
           y = "Proporción",
           x = "Año-Trimestre")
    
    ggplotly(p)
  })
}

# Ejecutar la app
shinyApp(ui, server)
