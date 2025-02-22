Cuadro_1.1a <- base %>%   summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            Ocupados_dis_nodem= sum(PONDERA[ESTADO == 1 & PP03J==1 & PP03I==2]), 
            Sobreocupados     =sum(PONDERA[ESTADO == 1 & INTENSI ==3]),
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa ocupados demandantes'       = Ocupados_demand/PEA,
            'Tasa Subocupaci?n'               = Subocupados/PEA,
            'Tasa Subocupaci?n demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupaci?n no demandante' = Suboc_no_demand/PEA)



