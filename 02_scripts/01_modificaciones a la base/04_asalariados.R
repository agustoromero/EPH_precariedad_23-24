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
    ),
    antiguedad_empleo = case_when(
      as_factor(PP07A) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP07A),#asalariados
      as_factor(PP05H) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP05H) #no asalariados (independientes/cuentapropia)
    ), 
    antiguedad_empleo = factor(
      antiguedad_empleo, 
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("menor a 1 mes", "1 a 3 meses", "más de 3 a 6 meses", 
                 "más de 6 a 12 meses", "más de 1 año a 5 años", "más de 5 años")
    )
  )

# Guardar base asalariados
saveRDS(base_asalariados, "01_data/outputs_filtros/base_asalariados.rds")