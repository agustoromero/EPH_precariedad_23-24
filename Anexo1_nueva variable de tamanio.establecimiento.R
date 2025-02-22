#Una comparación entre las variables cosntruidas para el tamaño de establecimiento. 

base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>% # Ocupados asalariados
  mutate(
    tamanio.establec_original = factor(
      case_when(PP04C %in% 1:6  ~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 ~ "Grande",
                PP04C %in% 99   ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")))
Cuadro1_original <- base %>%  group_by(tamanio.establec_original) %>% summarise ( TOTAL = sum(PONDERA))
  
  
  
#Categorizo sin jerarquía y de manera exaustiva.   
  base <- base %>%  filter(ESTADO==1) %>%  mutate(tamanio.establec.nueva = case_when(PP04C== 1~ "uni",
                                                                               PP04C== 2~ "peque",
                                                                               PP04C== 3~ "peque",
                                                                               PP04C== 4~ "peque",
                                                                               PP04C== 5~ "peque",
                                                                               PP04C== 6~ "mediano",
                                                                               PP04C== 7~ "mediano",
                                                                               PP04C== 8~ "mediano",
                                                                               PP04C== 9~ "grande",
                                                                               PP04C== 10~ "grande",
                                                                               PP04C== 11~ "grande",
                                                                               PP04C== 12~ "grande",
                                                                               PP04C99==1 ~ "peque",
                                                                               PP04C99==2 ~ "mediano",
                                                                               PP04C99== 3 ~ "grande",
                                                                               PP04C99==9 ~ "NS/NR",
                                                                               PP04B1==1 ~ "casaparticular"))
#Repito el cuadro con la nueva construcción  
  Cuadro1_nueva <- base %>%  
    group_by(tamanio.establec.nueva) %>% summarise ( TOTAL = sum(PONDERA))

  cuadro_anexo_3_23 <- list(Cuadro1_original,Cuadro1_nueva)
  
  
write.xlsx (file = "C:/Users/Hache/Documents/GitHub/EPH_precariedad_23-24/cuadros/cuadro_anexo_tamanioestablecimiento.", cuadro_anexo_3_23)
  
                                                