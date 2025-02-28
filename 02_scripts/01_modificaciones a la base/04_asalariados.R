# Cargar base de ocupados
base_ocupados <- readRDS("01_data/outputs_filtros/base_ocupados.rds")

# Filtrar asalariados
base_asalariados <- base_ocupados %>%
  filter(CAT_OCUP == 3) %>%
  mutate(
    tamanio.establec.nueva = case_when(
      PP04C == 1 ~ "uni",
      PP04C %in% c(2, 3, 4, 5) ~ "peque",
      PP04C %in% c(6, 7, 8) ~ "mediano",
      PP04C %in% c(9, 10, 11, 12) ~ "grande",
      PP04C99 == 1 ~ "peque",
      PP04C99 == 2 ~ "mediano",
      PP04C99 == 3 ~ "grande",
      PP04C99 == 9 ~ "NS/NR",
      TRUE ~ NA_character_
    )
  )

# Guardar base asalariados
saveRDS(base_asalariados, "01_data/outputs_filtros/base_asalariados.rds")