# Cargar base
base <- readRDS("01_data/outputs_filtros/base_persona.rds")

# Filtrar ocupados
base_ocupados <- base %>%
  filter(ESTADO == 1)

# Guardar base ocupados
saveRDS(base_ocupados, "01_data/outputs_filtros/base_ocupados.rds")
