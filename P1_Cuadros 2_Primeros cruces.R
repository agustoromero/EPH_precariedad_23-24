
C_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")
Csexo_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3)  %>%
  group_by(CH04) %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")
