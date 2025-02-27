# Cargar base de asalariados
base_asalariados <- readRDS("data/base_asalariados.rds")

# Crear variables de precariedad
base_asalariados <- base_asalariados %>%
  mutate(
    descuento_jubil = case_when(PP07H == 1 ~ "Si", PP07H == 2 ~ "No"),
    part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si", TRUE ~ "No"),
    tiempo.determinado = case_when(PP07C == 1 ~ "Si", TRUE ~ "No"),
    signo_educ_tamaño = as.integer(nivel.ed1 == "Menor a Secundaria" & tamanio.establec.nueva == "peque"),
    signo_sindescuento = as.integer(descuento_jubil == "No"),
    signo_tiempo = as.integer(part.time.inv == "Si" & tiempo.determinado == "No"),
    total_3_signos = signo_educ_tamaño + signo_sindescuento + signo_tiempo,
    almenos1de3 = as.integer(total_3_signos >= 1)
  )

# Guardar base con precariedad
saveRDS(base_asalariados, "data/base_precariedad.rds")