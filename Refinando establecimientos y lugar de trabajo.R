#BASE####
base <- base %>%
filter(ESTADO == 1, CAT_OCUP == 3) %>%    mutate(EST_tamano = case_when(PP04C== 1~ "unipersonal",
                                PP04C== 2~ "peque(2-5)",
                                PP04C== 3~ "peque(2-5)",
                                PP04C== 4~ "peque(2-5)",
                                PP04C== 5~ "peque(2-5)",
                                PP04C== 6~ "mediano",
                                PP04C== 7~ "mediano",
                                PP04C== 8~ "mediano",
                                PP04C== 9~ "grande",
                                PP04C== 10~ "grande",
                                PP04C== 11~ "grande",
                                PP04C== 12~ "grande",
                                PP04C99==1 ~ "peque(2-5)",
                                PP04C99==2 ~ "mediano",
                                PP04C99== 3 ~ "grande",
                                PP04C99==9 ~ "NS/NR",
                                PP04B1==1 ~ "casaparticular"))
base <- base %>%
  filter(ESTADO == 1, CAT_OCUP == 3) %>%  mutate( tamanio.establec = factor(
    case_when(PP04C %in% 1:6  ~ "Pequeño",
              PP04C %in% 7:8  ~ "Mediano",
              PP04C %in% 9:12 ~ "Grande",
              PP04C %in% 99   ~ "Ns/Nr",
              PP04B1 %in% 1 ~ "casaparticulear"),
    levels = c("Pequeño","Mediano","Grande","Ns/Nr","casaparticulear")))
#####


#Cuadro ANAEXO1####

#analizamos y refinamos la busqueda de información por tamaño de establecimiento

c13 <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3)  %>%   
  calculate_tabulates(base = base, 
                      x="EST_tamano",
                      y="tamanio.establec",
                      weights="PONDERA")
