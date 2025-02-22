#Cuadros de clasificacion precaria


base <- base %>%  
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(
    preca_tecno_calif = case_when(
      TECNOLOGIA == 1 | 
        CALIFICACION %in% c(3, 4) ~ 1,
      TRUE ~ 0
    ))
