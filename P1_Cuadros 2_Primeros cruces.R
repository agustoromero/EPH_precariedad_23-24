
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

Crama <- base  %>%
  filter(ESTADO==1 &  CAT_OCUP==3 & CH04==2)  %>% 
  group_by(caes_seccion_label)%>% summarise( total = sum (PONDERA))

Csexo_rama <- base  %>%
  filter(ESTADO==1 &  CAT_OCUP==3)  %>% 
  group_by(caes_seccion_label) %>% summarise( total = sum (PONDERA),
                                             varon = sum (PONDERA[CH04==1]),
                                             mujer = sum (PONDERA[CH04==2]),
                                             'Prop. mujer' =  mujer / total,
                                             'Prop. varon' = varon / total)

Cregistro_rama <- base %>% filter(ESTADO==1, CAT_OCUP==3) %>%  group_by(caes_seccion_label) %>% summarise( 
                                                                        total                        = sum (PONDERA[CAT_OCUP==3]),
                                                                        Asal_protegidos              = sum (PONDERA[CAT_OCUP==3& PP07H==1]),
                                                                        Asal_precarios_tot            = sum (PONDERA[CAT_OCUP==3& PP07H==2]),
                                                                        Asal_precarios_i1_mono       = sum (PONDERA[CAT_OCUP==3& PP07I==1]),
                                                                        Asal_precarios_i2_negr       = sum (PONDERA[CAT_OCUP==3& PP07I==2]),
                                                                       'part asal_prote'             = Asal_protegidos / total,
                                                                       'part asal_precarios'         = Asal_precarios_tot / total,
                                                                       'part mono en precarios'      = Asal_precarios_i1_mono / Asal_precarios_tot,
                                                                       'part negro en precarios'     = Asal_precarios_i2_negr / Asal_precarios_tot,
                                                                       'part NSNR en precarios'      = (total - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot 
                                                                       )

