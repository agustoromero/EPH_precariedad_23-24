# Librerías
library(tidyverse)
library(eph)
library(haven)

# Descarga y organización de la base
base <- get_microdata(year = 2023, trimester = 3, type = "individual", vars = "all") %>%
  organize_labels(type = "individual")

# Guardar base sin modificaciones
saveRDS(base, "data/base_original.rds")