# Cargar base original
base <- readRDS("01_data/Input_original/base_original.rds")

# Crear variables rango_etario y nivel.ed1
base <- base %>%
  mutate(
    rango_etario = case_when(
      CH06 < 19  ~ "Menor a 19",
      CH06 >= 19 & CH06 <= 25 ~ "19 a 25",
      CH06 >= 26 & CH06 <= 35 ~ "26 a 35",
      CH06 >= 36 & CH06 <= 45 ~ "36 a 45",
      CH06 >= 46 & CH06 <= 55 ~ "46 a 55",
      CH06 >= 56 & CH06 <= 65 ~ "56 a 65",
      CH06 >= 66 & CH06 <= 75 ~ "66 a 75",
      CH06 >= 76 & CH06 <= 85 ~ "76 a 85",
      CH06 > 85  ~ "Mayor a 85",
      TRUE ~ NA_character_
    ),
    nivel.ed1 = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria", ###¿primario?
      NIVEL_ED %in% 4 ~ "Secundaria Completa",
      NIVEL_ED == 5 ~ "Superior Incompleto",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo"))
  )

# Guardar base con variables personales
saveRDS(base, "01_data/outputs_filtros/base_persona.rds")