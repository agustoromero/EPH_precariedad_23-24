# Cargar base
base <- readRDS("data/base_persona.rds")

# Filtrar ocupados
base_ocupados <- base %>%
  filter(ESTADO == 1)

# Guardar base ocupados
saveRDS(base_ocupados, "data/base_ocupados.rds")
