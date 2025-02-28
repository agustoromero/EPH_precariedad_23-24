# Cargar base de asalariados
base_asalariados <- readRDS("01_data/outputs_filtros/base_asalariados.rds")

# Crear variables de precariedad
base_asalariados <- base_asalariados %>%
  mutate(
    # Signo de precariedad tecnológica y de calificación
    preca_tecno_calif = case_when(
      TECNOLOGIA == 1 & CALIFICACION == 1 ~ 4,  # Solo cuando ambos son 1
      TRUE ~ 0 ),
    
    # Clasificación de educación
    nivel.ed = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
      NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    
    # Clasificación de tamaño del establecimiento
    tamanio.establec = factor(case_when(
      PP04C %in% 1:6  ~ "Pequeño",
      PP04C %in% 7:8  ~ "Mediano",
      PP04C %in% 9:12 ~ "Grande",
      PP04C %in% 99   ~ "Ns/Nr"
    ), levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
    
    descuento_jubil = case_when(PP07H == 1 ~ "Si", PP07H == 2 ~ "No"),
    part.time.inv = case_when(PP3E_TOT < 35 & PP03G == 1 ~ "Si", TRUE ~ "No"),
    tiempo.determinado = case_when(PP07C == 1 ~ "Si", TRUE ~ "No"),
    signo_educ_tamaño = as.integer(nivel.ed == "Menor a Secundaria" & tamanio.establec == "Pequeño"),  # 1er signo
    signo_sindescuento = as.integer(descuento_jubil == "No"),  # 2do signo
    signo_tiempo = as.integer(part.time.inv == "Si" & tiempo.determinado == "No"),  # 3er signo
    signo_tecno_calif = as.integer(preca_tecno_calif == 4),  # 4to signo
    total_4_signos = signo_educ_tamaño + signo_sindescuento + signo_tiempo + signo_tecno_calif,
    almenos1de4 = as.integer(total_4_signos >= 1),
    almenos1de3 = as.integer(signo_educ_tamaño + signo_sindescuento + signo_tiempo >= 1),
    sin_preca_de3 = as.integer(signo_educ_tamaño + signo_sindescuento + signo_tiempo == 0),
    sin_preca_de4 = as.integer(total_4_signos == 0)
  )

# Guardar base con precariedad
saveRDS(base_asalariados, "01_data/outputs_filtros/base_precariedad.rds")