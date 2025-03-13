
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




# UI del dashboard
ui <- fluidPage(
  titlePanel("Proporciones de Precariedad Laboral"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sexo", "Seleccionar Sexo:",
                  choices = unique(c.6_signos_preca_consolidado$Sexo),
                  selected = "Ambos")
    ),
    mainPanel(
      plotOutput("precariedadPlot")
    )
  )
)

# Server del dashboard
server <- function(input, output) {
  output$precariedadPlot <- renderPlot({
    df <- c.6_signos_preca_consolidado %>%
      filter(Sexo == input$sexo) %>%
      select(anio_trim, Prop_Signo1, Prop_Signo2, Prop_Signo3, Prop_Almenos1de3) %>%
      pivot_longer(-anio_trim, names_to = "Indicador", values_to = "Proporcion")
    
    ggplot(df, aes(x = factor(anio_trim), y = Proporcion, fill = Indicador)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(Proporcion, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
      labs(title = "Proporciones de Precariedad Laboral", y = "Proporción (%)", x = "Trimestre", fill = "Indicador") +
      theme_minimal()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

# # Cargar los datos consolidados
# data <- c.4_consolidado %>%
#   filter(tamanio.establec.nueva != "Total", nivel.ed1 != "Total") %>%
#   mutate(
#     Prop_Varones = as.numeric(Prop_Varones),
#     Prop_Mujeres = as.numeric(Prop_Mujeres)
#   ) %>%
#   replace_na(list(Prop_Varones = 0, Prop_Mujeres = 0))
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Distribución de Asalariados por Nivel Educativo y Tamaño del Establecimiento"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("nivel_ed", "Seleccionar Nivel Educativo:", choices = unique(data$nivel.ed1)),
#       selectInput("tamanio", "Seleccionar Tamaño del Establecimiento:", choices = unique(data$tamanio.establec.nueva))
#     ),
#     mainPanel(
#       plotlyOutput("grafico_prop")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output) {
#   datos_filtrados <- reactive({
#     data %>%
#       filter(nivel.ed1 == input$nivel_ed,
#              tamanio.establec.nueva == input$tamanio)
#   })
#   
#   output$grafico_prop <- renderPlotly({
#     datos <- datos_filtrados()
#     
#     p <- ggplot(datos, aes(x = anio_trim, group = interaction(anio_trim))) +
#       geom_bar(aes(y = Prop_Varones, fill = "Varones"), stat = "identity", position = "dodge") +
#       geom_bar(aes(y = Prop_Mujeres, fill = "Mujeres"), stat = "identity", position = "dodge") +
#       geom_text(aes(y = Prop_Varones, label = ifelse(Prop_Varones > 0, scales::percent(Prop_Varones, accuracy = 0.1), "")),
#                 vjust = -0.5, color = "black") +
#       geom_text(aes(y = Prop_Mujeres, label = ifelse(Prop_Mujeres > 0, scales::percent(Prop_Mujeres, accuracy = 0.1), "")),
#                 vjust = 1.5, color = "black") +
#       scale_fill_manual(values = c("Varones" = "blue", "Mujeres" = "pink")) +
#       facet_wrap(~nivel.ed1, scales = "fixed") +
#       theme_minimal() +
#       labs(title = "Proporción de Varones y Mujeres por Nivel Educativo y Trimestre",
#            y = "Proporción",
#            x = "Año-Trimestre")
#     
#     ggplotly(p)
#   })
# }
# 
# # Ejecutar la app
# shinyApp(ui, server)
# 
# 
# 
# 
# 
# 
# 
# 
# # Cargar los datos consolidados
# data <- c.5_rama_cond.registro_tipo.establec_consolidado %>%
#   filter(tipo_establecimiento != "Total")
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Evolución de la Condición de Registro por Sector"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("tipo_estab", "Seleccionar Tipo de Establecimiento:", 
#                   choices = unique(data$tipo_establecimiento)),
#       selectizeInput("sectores", "Seleccionar Sectores:", 
#                      choices = unique(data$caes_seccion_label), multiple = TRUE)
#     ),
#     mainPanel(
#       uiOutput("graficos_ui")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output) {
#   datos_filtrados <- reactive({
#     df <- data %>% filter(tipo_establecimiento == input$tipo_estab)
#     if (!is.null(input$sectores) && length(input$sectores) > 0) {
#       df <- df %>% filter(caes_seccion_label %in% input$sectores)
#     }
#     df
#   })
#   
#   output$graficos_ui <- renderUI({
#     datos <- datos_filtrados()
#     sectores_seleccionados <- unique(datos$caes_seccion_label)
#     req(sectores_seleccionados)
#     
#     plot_output_list <- lapply(sectores_seleccionados, function(sector) {
#       plotlyOutput(outputId = paste0("grafico_", gsub(" ", "_", sector)))
#     })
#     
#     do.call(tagList, plot_output_list)
#   })
#   
#   observe({
#     datos <- datos_filtrados()
#     sectores_seleccionados <- unique(datos$caes_seccion_label)
#     
#     lapply(sectores_seleccionados, function(sector) {
#       output[[paste0("grafico_", gsub(" ", "_", sector))]] <- renderPlotly({
#         datos_sector <- datos %>% filter(caes_seccion_label == sector)
#         
#         p <- ggplot(datos_sector, aes(x = anio_trim)) +
#           geom_line(aes(y = part_asal_prote, color = "Protegidos"), size = 1) +
#           geom_line(aes(y = part_asal_precarios, color = "Precarios"), size = 1) +
#           geom_line(aes(y = part_mono_en_precarios, color = "Mono en Precarios"), size = 1) +
#           geom_line(aes(y = part_negro_en_precarios, color = "Negro en Precarios"), size = 1) +
#           geom_line(aes(y = part_NSNR_en_precarios, color = "NSNR en Precarios"), size = 1) +
#           scale_y_continuous(labels = scales::percent) +
#           scale_color_manual(values = c("Protegidos" = "blue", "Precarios" = "red", 
#                                         "Mono en Precarios" = "orange", "Negro en Precarios" = "purple", 
#                                         "NSNR en Precarios" = "green")) +
#           theme_minimal() +
#           labs(title = paste("Evolución de la Condición de Registro -", sector),
#                y = "Proporción", x = "Año-Trimestre", color = "Condición")
#         
#         ggplotly(p)
#       })
#     })
#   })
# }
# 
# # Ejecutar la app
# shinyApp(ui, server)


#grafico precariedad sin descuento
library(plotly)
library(dplyr)

# Suponemos que c.91_precaSS_educ_sexo_final ya tiene la columna anio_trim
# (extraída previamente de `anio_trim/nivel.ed1`)
# Aseguramos el orden deseado:
sexos <- c("Varón", "Mujer", "Ambos")

# Función para escalar tamaños (evita tamaños desproporcionados)
escala_puntos <- function(valores) {
  5 + sqrt(valores) * 9  # Puedes ajustar estos valores si es necesario
}

# Crear gráfico interactivo con configuración inicial
fig <- plot_ly() %>%
  layout(
    title = "Proporción de Niveles Educativos - Varón",
    xaxis = list(title = "Trimestre"),
    yaxis = list(
      title = "Nivel Educativo",
      categoryorder = "array",
      categoryarray = c("Menor a Secundaria", "Secundaria Completa", "Superior Incompleto", "Superior Completo"),
      autorange = TRUE
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = list(
          # Botón para Varón: primeras 4 trazas visibles
          list(
            method = "update", 
            args = list(
              list(visible = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
              list(title = "Proporción de Niveles Educativos - Varón")
            ),
            label = "Varón"
          ),
          # Botón para Mujer: trazas 5 a 8 visibles
          list(
            method = "update", 
            args = list(
              list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
              list(title = "Proporción de Niveles Educativos - Mujer")
            ),
            label = "Mujer"
          ),
          # Botón para Ambos: últimas 4 trazas visibles
          list(
            method = "update", 
            args = list(
              list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)),
              list(title = "Proporción de Niveles Educativos - Ambos")
            ),
            label = "Ambos"
          )
        )
      )
    )
  )

# Agregar trazas para cada grupo, en el orden deseado
for (sexo in sexos) {
  datos_filtrados <- c.91_precaSS_educ_sexo_final %>%
    filter(sexo == !!sexo)
  
  # Asignar color según sexo
  color <- case_when(
    sexo == "Varón" ~ "blue",
    sexo == "Mujer" ~ "red",
    sexo == "Ambos" ~ "grey"
  )
  
  # Solo las trazas para Varón se muestran por defecto
  visible_flag <- ifelse(sexo == "Varón", TRUE, FALSE)
  
  # Se agregan 4 trazas para cada grupo (una para cada nivel educativo)
  fig <- fig %>%
    add_trace(
      data = datos_filtrados,
      x = ~ anio_trim,
      y = ~ "Menor a Secundaria",
      type = 'scatter',
      mode = 'markers+text',
      text = ~ paste0(round(`prop_Menor a Secundaria`, 1), "%"),
      textposition = "middle center",
      marker = list(size = ~ escala_puntos(`prop_Menor a Secundaria`), color = color),
      name = paste("Menor a Secundaria (", sexo, ")"),
      visible = visible_flag
    ) %>%
    add_trace(
      data = datos_filtrados,
      x = ~ anio_trim,
      y = ~ "Secundaria Completa",
      type = 'scatter',
      mode = 'markers+text',
      text = ~ paste0(round(`prop_Secundaria Completa`, 1), "%"),
      textposition = "middle center",
      marker = list(size = ~ escala_puntos(`prop_Secundaria Completa`), color = color),
      name = paste("Secundaria Completa (", sexo, ")"),
      visible = visible_flag
    ) %>%
    add_trace(
      data = datos_filtrados,
      x = ~ anio_trim,
      y = ~ "Superior Incompleto",
      type = 'scatter',
      mode = 'markers+text',
      text = ~ paste0(round(`prop_Superior Incompleto`, 1), "%"),
      textposition = "middle center",
      marker = list(size = ~ escala_puntos(`prop_Superior Incompleto`), color = color),
      name = paste("Superior Incompleto (", sexo, ")"),
      visible = visible_flag
    ) %>%
    add_trace(
      data = datos_filtrados,
      x = ~ anio_trim,
      y = ~ "Superior Completo",
      type = 'scatter',
      mode = 'markers+text',
      text = ~ paste0(round(`prop_Superior Completo`, 1), "%"),
      textposition = "middle center",
      marker = list(size = ~ escala_puntos(`prop_Superior Completo`), color = color),
      name = paste("Superior Completo (", sexo, ")"),
      visible = visible_flag
    )
}

# Mostrar gráfico
fig
