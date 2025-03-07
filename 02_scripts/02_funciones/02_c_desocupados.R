C.2_desocupados <- base %>% 
  
  summarise( Poblacion           = sum(PONDERA),
             Ocupados            = sum(PONDERA[ESTADO==1]),
             Desocupado          = sum(PONDERA[ESTADO==2]),
             PEA                 = Ocupados + Desocupado,
             Muj_hasta29        = sum(PONDERA[ESTADO==2 & CH04==2 & (CH06 %in% c(-1:29))]),
             Muj_30a64          = sum(PONDERA[ESTADO==2 & CH04==2 & (CH06 %in% c(30:64))]),
             Muj_65ymas         = sum(PONDERA[ESTADO==2 & CH04==2 & (CH06 >=65)]),
             Var_hasta29        = sum(PONDERA[ESTADO==2 & CH04==1 & (CH06 %in% c(-1:29))]),
             Var_30a64          = sum(PONDERA[ESTADO==2 & CH04==1 & (CH06 %in% c(30:64))]),
             Var_65ymas         = sum(PONDERA[ESTADO==2 & CH04==1 & (CH06 >=65)]),
             Jefes              = sum(PONDERA[CH03==1 & ESTADO==2]),
             Conyuges           = sum(PONDERA[CH03==2 & ESTADO==2]),
             Hijos              = sum(PONDERA[CH03==3 & ESTADO==2]),
             Otros_Componentes  = sum(PONDERA[CH03 %in% c(4:10) & ESTADO==2]),
             NIVEL_ED_1         = sum(PONDERA[NIVEL_ED==1 & ESTADO==2]),
             NIVEL_ED_2         = sum(PONDERA[NIVEL_ED==2 & ESTADO==2]),
             NIVEL_ED_3         = sum(PONDERA[NIVEL_ED==3 & ESTADO==2]),
             NIVEL_ED_4         = sum(PONDERA[NIVEL_ED==4 & ESTADO==2]),
             NIVEL_ED_5         = sum(PONDERA[NIVEL_ED==5 & ESTADO==2]),
             NIVEL_ED_6         = sum(PONDERA[NIVEL_ED==6 & ESTADO==2]),
             NIVEL_ED_7         = sum(PONDERA[NIVEL_ED==7 & ESTADO==2]),
             NIVEL_ED_9         = sum(PONDERA[NIVEL_ED==9 & ESTADO==2]),
             
             #tiempo de busqueda
             men1mes            = sum(PONDERA[PP10A==1], na.rm=T),
             de1a3              = sum(PONDERA[PP10A==2], na.rm=T),
             mas3a6             = sum(PONDERA[PP10A==3], na.rm=T),
             mas6a12            = sum(PONDERA[PP10A==4], na.rm=T),
             mas1año            = sum(PONDERA[PP10A==5], na.rm=T), 
             nsnr_tiembusq      = sum(PONDERA[PP10A==9], na.rm=T),
             #tipo de desocupado
             cocupant           = sum(PONDERA[PP10E %in% c(1:5)]),
             sinocup            = sum(PONDERA[PP10D==2 | PP10E==6],na.rm=T),
             nsnrocupant        = sum (PONDERA[ PP10E==9],na.rm=T),
             
             
             
             #categoria ocupacional en la última ocupación
             CAT_OCUP3          = sum(PONDERA[ESTADO==2 & CAT_OCUP==3]),
             CAT_OCUPNO3        = sum(PONDERA[ESTADO==2 & (CAT_OCUP==1 | CAT_OCUP==2 | CAT_OCUP==4)]),
             CAT_OCUP9          = sum (PONDERA[ PP10E==9],na.rm=T),
             sinocupbis         = sum(PONDERA[PP10D==2 | PP10E==6],na.rm=T),
             
             #tipo de establecimiento de última ocupacion
             Sector1            = sum(PONDERA[ESTADO==2 & PP11A==1]),
             Sector2            = sum(PONDERA[ESTADO==2 & PP11A==2]),
             Sector3            = sum(PONDERA[ESTADO==2 & PP11A==3]),
             Sector9            = sum(PONDERA[ESTADO==2 & PP11A==9]),
             sinocuptris        = sum(PONDERA[PP10D==2 | PP10E==6],na.rm=T))

