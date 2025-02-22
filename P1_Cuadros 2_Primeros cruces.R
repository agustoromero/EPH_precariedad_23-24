
C_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")
Cvar_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3 & CH04==1)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                y="tamanio.establec.nueva",
                      weights="PONDERA")
Cmuj_niveled_estab <- base  %>% filter(ESTADO==1 &  CAT_OCUP==3 & CH04==2)  %>%
  calculate_tabulates(base = base, 
                      x="nivel.ed",
                      y="tamanio.establec.nueva",
                      weights="PONDERA")
