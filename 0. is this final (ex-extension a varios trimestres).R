# 0. Entorno de trabajo ####  
# Librerías necesarias
  library(tidyverse)
  library(eph)
  library(haven)
  library(dplyr)
  library(readr)
  library(openxlsx)
#Entorno  
  # Ruta donde se almacenan los archivos RDS
  ruta_datos <- "01_data/"
  # Función para importar datos desde RDS
  importar_datos <- function(ano, trimestre) {
    archivo <- paste0(ruta_datos, "base_", ano, "_T", trimestre, ".rds")
    
    if (file.exists(archivo)) {
      datos <- readRDS(archivo) %>%
        mutate(anio_trim = paste0(ANO4, "T", TRIMESTRE))  # Agregar columna para identificación
      
      return(datos)
    } else {
      message("No se encontró: ", archivo)
      return(NULL)
    }
  }

#  #     0.1. BASES_SET ####
  # Definir los trimestres de interés (3T 2023 hasta 2T 2024)
  trimestres_seleccionados <- expand.grid(
    ANO4 = 2023:2024, 
    TRIMESTRE = 1:4
  ) %>%
    filter(!(ANO4 == 2023 & TRIMESTRE < 3),  # Excluir antes del 3T 2023
           !(ANO4 == 2024 & TRIMESTRE > 2))  # Excluir después del 2T 2024
 
  {
# Función para descargar y guardar datos en RDS
# Tiltie esta parte para no repetir con las lineas 40--52
    # descargar_datos <- function(ano, trimestre) {
  #   archivo <- paste0(ruta_datos, "base_", ano, "_T", trimestre, ".rds")
  #   
  #   if (!file.exists(archivo)) {  # Descargar solo si el archivo no existe
  #     datos <- get_microdata(year = ano, period = trimestre, type = "individual")
  #     saveRDS(datos, file = archivo)
  #     message("Descargado y guardado: ", archivo)
  #   } else {
  #     message("Ya existe: ", archivo)
  #   }
  # }
  # # Descargar los archivos necesarios
  # mapply(descargar_datos, 
  #        trimestres_seleccionados$ANO4, 
  #        trimestres_seleccionados$TRIMESTRE) 
    } #tiltié porque se repite (BORRAR) 
 
  
  # Importar y combinar todos los archivos
  lista_datos <- lapply(1:nrow(trimestres_seleccionados), function(i) {
    importar_datos(trimestres_seleccionados$ANO4[i], 
                   trimestres_seleccionados$TRIMESTRE[i])
  })

 {
  # Modificación de todo: termina en base_ocupados
  # Unir todas las bases en una sola 
  datos_completos <- bind_rows(lista_datos)
  # Organizar etiquetas y clasificaciones
  datos_completos <- datos_completos %>%
    organize_labels(type = "individual") %>%
    organize_caes() %>%   # Etiquetas según CAES
    organize_cno()        # Clasificación según CNO
    # Vista de los datos importados
    head(datos_completos)
  # Guardar base consolidada
saveRDS(datos_completos, "01_data/input_original/bases_originales.rds")

    # Cargar bases originales - ex datos_completos
base <- readRDS("01_data/input_original/bases_originales.rds")

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
    nivel.ed = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria", ###¿primario?
      NIVEL_ED %in% 4 ~ "Secundaria Completa",
      NIVEL_ED == 5 ~ "Superior Incompleto",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo"))
  )
# Filtrar ocupados
  base_ocupados <- base %>%
    filter(ESTADO == 1)
#(ver : aca irían los cambios a los ocupados: tamaño del establecimiento, clasificacion y etc.)
# Guardar base ocupados (ver BORRAR)
saveRDS(base_ocupados, "01_data/outputs_filtros/bases_ocupados.rds")
  
# Transformaciones a ocupados (Ex "filtrar asalariados" cambie una cositas porque son variables de ocupados) 
base_ocupados <- base_ocupados %>%
  filter(ESTADO== 1) %>%
  mutate(
    tamanio.establec.nueva = case_when(
      !is.na(PP04C99) ~ case_when(
        PP04C99 == 1 ~ "peque",
        PP04C99 == 2 ~ "mediano",
        PP04C99 == 3 ~ "grande",
        PP04C99 == 9 ~ "NS/NR"
      ),
      PP04C == 1 ~ "uni",
      PP04C %in% c(2, 3, 4, 5) ~ "peque",
      PP04C %in% c(6, 7, 8) ~ "mediano",
      PP04C %in% c(9, 10, 11, 12) ~ "grande",
      TRUE ~ NA_character_
    ),
    antiguedad_empleo = case_when(
      as_factor(PP07A) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP07A),#asalariados
      as_factor(PP05H) %in% c("1", "2", "3", "4", "5", "6") ~ as_factor(PP05H) #no asalariados (independientes/cuentapropia)
    ), 
    antiguedad_empleo = factor(
      antiguedad_empleo, 
      levels = c("1", "2", "3", "4", "5", "6"),
      labels = c("menor a 1 mes", "1 a 3 meses", "más de 3 a 6 meses", 
                 "más de 6 a 12 meses", "más de 1 año a 5 años", "más de 5 años")
    )
  )
      {#base_asalariados <- base_ocupados %>% filter(CAT_OCUP==3)
# Guardar base asalariados
#saveRDS(base_ocupados, "01_data/outputs_filtros/bases_asalariados.rds")
#saveRDS(base_asalariados, "01_data/outputs_filtros/bases_asalariados.rds")

}# (BORRAR) De nuevo, quizás no tiene sentido guardar una nueva base.Armaría todo este capitulo con una sola base

}#BASE OCUPADOS #Modificaciones a la base
  
# 1. Asalariados - Crear variables de precariedad ####
{#(ver : sugerencia de modificar trtamiento pa ocupados vs asalariados) 
#Acá hay variables para los asalariados que se pueden aplicar para todos (ver )
#las variables de nivel.ed y tamano ya estan creadas (ver )
base_asalariados <- base_ocupados %>% filter(CAT_OCUP==3)
base_asalariados <- base_asalariados %>%
  mutate(
    # Signo de precariedad tecnológica y de calificación
    preca_tecno_calif = case_when(
      TECNOLOGIA == 1 & CALIFICACION == 1 ~ 4,  # Solo cuando ambos son 1
      TRUE ~ 0 ),
    # Clasificación de educación
    nivel.ed = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria", ###¿primario?
      NIVEL_ED %in% 4 ~ "Secundaria Completa",
      NIVEL_ED == 5 ~ "Superior Incompleto",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo"))
  )    
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

# Guardar base con precariedad # (ver : podria ser la misma base de asalariados.)
#acá tiene otro sentido guardarla porque tenes cargadas las variables de precariedad, igual se me hacen muchas bases.
saveRDS(base_asalariados, "01_data/outputs_filtros/bases_precariedad.rds")
} #1.1 base asalariados
  # Cargar base de asalariados
base_asalariados <- readRDS("01_data/outputs_filtros/bases_asalariados.rds")

#APLICAMOS ANALISIS A CADA TRIMESTRE Y UNIMOS ####
# Filtrar por Año y Trimestre con reconocimiento de texto
filtrar_por_trimestre <- function(df, anio, trimestre) {
  # Crear la columna anio.trim para el formato YYYYTn
  df <- df %>%
    mutate(anio_trim = paste(ANO4, "T", TRIMESTRE, sep = ""))
  
  # Filtrar el dataframe usando str_detect para los patrones "YYYYTn"
  df_filtrado <- df %>%
    filter(str_detect(anio_trim, paste(anio, "T", trimestre, sep = "")))
  
  return(df_filtrado)
}

# Funciones para cálculo
calcular_poblacion_estado <- function(df) {
  # Asegurarnos de que ESTADO sea un factor
  df$ESTADO <- as.factor(df$ESTADO)
  
  c.1.1_estado_sexo <- df %>%
    group_by(ESTADO, anio_trim) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(Poblacion_Ambos:Poblacion_Mujeres, ~ replace(., is.na(.), "-")))
  
  # Asignar niveles a ESTADO
  levels(c.1.1_estado_sexo$ESTADO) <- c("Entrevista no realizada", "Ocupado", "Desocupado", "Inactivo", "Menor de 10 años")
  
  return(c.1.1_estado_sexo)
}

calcular_poblacion_estado_cat_ocup <- function(df) {
  df_ocupado <- df %>%
    filter(ESTADO == 1)
  
  df_ocupado$ESTADO <- as.factor(df_ocupado$ESTADO)
  df_ocupado$CAT_OCUP <- as.factor(df_ocupado$CAT_OCUP)
  
  c.1.2_cat_ocup <- df_ocupado %>%
    group_by(CAT_OCUP, anio_trim) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(Poblacion_Ambos:Poblacion_Mujeres, ~ replace(., is.na(.), "-")))
  
  levels(c.1.2_cat_ocup$CAT_OCUP) <- c("Patrón", "Cuenta propia", "Obrero o empleado", 
                                       "Trabajador familiar sin remuneración", "Ns./Nr.")
  
  return(c.1.2_cat_ocup)
}

calcular_rama_tamanio_sexo <- function(df) {
  # Ahora estamos trabajando con base_asalariados, por lo que filtramos sobre ella
  cuadro_base <- df %>%
    filter(ESTADO == 1 & CAT_OCUP == 3) %>%
    group_by(caes_seccion_label, tamanio.establec.nueva, anio_trim) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  total_rama <- cuadro_base %>%
    group_by(caes_seccion_label) %>%
    summarise(
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  total_general <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  cuadro_final <- bind_rows(cuadro_base, total_rama, total_general) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}
 #(ver) ACA UN TOQUE ME PIERDO
# Filtrar y aplicar las funciones a cada trimestre
trimestres <- list(
  "2023_3" = filtrar_por_trimestre(base, "2023", 3),
  "2023_4" = filtrar_por_trimestre(base, "2023", 4),
  "2024_1" = filtrar_por_trimestre(base, "2024", 1),
  "2024_2" = filtrar_por_trimestre(base, "2024", 2)
)

# Aplicar las funciones a cada trimestre y consolidar los resultados
c.1.1_resultados <- lapply(trimestres, calcular_poblacion_estado)
c.1.2_resultados <- lapply(trimestres, calcular_poblacion_estado_cat_ocup)

# Para calcular rama_tamanio_sexo, se trabaja sobre base_asalariados
base_asalariados_trimestres <- list(
  "2023_3" = filtrar_por_trimestre(base_asalariados, "2023", 3),
  "2023_4" = filtrar_por_trimestre(base_asalariados, "2023", 4),
  "2024_1" = filtrar_por_trimestre(base_asalariados, "2024", 1),
  "2024_2" = filtrar_por_trimestre(base_asalariados, "2024", 2)
)

# Aplicar la función para rama_tamanio_sexo
c.3_resultados <- lapply(base_asalariados_trimestres, calcular_rama_tamanio_sexo)

# Unir los resultados por trimestre
c.1.1_consolidado <- bind_rows(c.1.1_resultados)
c.1.2_consolidado <- bind_rows(c.1.2_resultados)
c.3_consolidado <- bind_rows(c.3_resultados)

# Ver los resultados #Probaria otra forma de ver (ver )
str(c.1.1_consolidado)
str(c.1.2_consolidado)
str(c.3_consolidado)

#Extension a scripts 04 05 06
#script 04

calcular_niveled_tamanio_sexo <- function(df) {
  cuadro_base <- df %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
    group_by(nivel.ed, tamanio.establec.nueva, anio_trim) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  # Calcular fila de total por nivel educativo
  total_niveled <- cuadro_base %>%
    group_by(nivel.ed) %>%
    summarise(
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calcular fila de total general
  total_general <- cuadro_base %>%                 #####ACA totales por TREMESTRE######
    summarise(
      nivel.ed = "Total",
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_niveled, total_general) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función a cada trimestre
c.4_niveled_tamanio_sexo_resultados <- lapply(base_asalariados_trimestres, calcular_niveled_tamanio_sexo)

# Consolidar los resultados
c.4_niveled_tamanio_sexo_consolidado <- bind_rows(c.4_niveled_tamanio_sexo_resultados)

# Mostrar resultado consolidado (Ver) # otra forma de verlo?
print(c.4_niveled_tamanio_sexo_consolidado)

#sript 05

calcular_rama_sexo <- function(df) {
  cuadro_base <- df %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
    group_by(caes_seccion_label, anio_trim) %>%
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  # Calcular fila de total
  total_fila <- cuadro_base %>%                #####ACA totales por TREMESTRE######
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función a cada trimestre
c.5_rama_sexo_resultados <- lapply(base_asalariados_trimestres, calcular_rama_sexo)

# Consolidar los resultados
c.5_rama_sexo_consolidado <- bind_rows(c.5_rama_sexo_resultados)

# Mostrar resultado consolidado
print(c.5_rama_sexo_consolidado)

#script 06

calcular_estab_condicion_registro <- function(df, tipo_estab = NULL, etiqueta_estab) {
  if (!is.null(tipo_estab)) {
    df <- df %>% filter(PP04A == tipo_estab)  # Filtrar por tipo de establecimiento si se especifica
  }
  
  cuadro_base <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3) %>%
    group_by(caes_seccion_label, anio_trim) %>%
    summarise(
      total = sum(PONDERA, na.rm = TRUE),
      Asal_protegidos = sum(PONDERA[PP07H == 1], na.rm = TRUE),
      Asal_precarios_tot = sum(PONDERA[PP07H == 2], na.rm = TRUE),
      Asal_precarios_i1_mono = sum(PONDERA[PP07H == 2 & PP07I == 1], na.rm = TRUE),
      Asal_precarios_i2_negr = sum(PONDERA[PP07H == 2 & PP07I == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_asal_prote = Asal_protegidos / total,
      part_asal_precarios = Asal_precarios_tot / total,
      part_mono_en_precarios = Asal_precarios_i1_mono / Asal_precarios_tot,
      part_negro_en_precarios = Asal_precarios_i2_negr / Asal_precarios_tot,
      part_NSNR_en_precarios = (Asal_precarios_tot - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot
    )
  
  # Calcular fila de total por establecimiento
  total_fila <- cuadro_base %>%                     #####ACA totales por TREMESTRE######
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Agregar columna de establecimiento
  cuadro_estab_condicion_registro <- bind_rows(cuadro_base, total_fila) %>%
    mutate(tipo_establecimiento = etiqueta_estab)
  
  return(cuadro_estab_condicion_registro)
}

# Calcular cuadros con totales por tipo de establecimiento para cada trimestre
cuadro_estab_resultados <- lapply(base_asalariados_trimestres, function(x) {
  list(
    total = calcular_estab_condicion_registro(x, NULL, "Total"),
    estatal = calcular_estab_condicion_registro(x, 1, "Estatal"),
    privado = calcular_estab_condicion_registro(x, 2, "Privado"),
    otro = calcular_estab_condicion_registro(x, 3, "Otro")
  )
})

# Unir todos los cuadros por trimestre
c.6_estab_condicion_registro_tipo_consolidado <- do.call(bind_rows, lapply(cuadro_estab_resultados, bind_rows))

# Mostrar resultado consolidado
print(c.6_estab_condicion_registro_tipo_consolidado)

#script 07

# Función para aplicar calcular_rama_sexo a cada trimestre
calcular_rama_sexo_trimestral <- function(df) {
  cuadro_base <- df %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3) %>% 
    group_by(anio_trim, caes_seccion_label) %>%  # Agregado anio_trim
    summarise(
      Poblacion_Ambos = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  # Calcular fila de total por trimestre
  total_fila <- cuadro_base %>%
    group_by(anio_trim) %>%  # Agregado anio_trim
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función a la base de datos
c.7.1_rama_sexo_consolidado <- calcular_rama_sexo_trimestral(base_asalariados)


calcular_rama_condicion_registro_trimestral <- function(df) {
  cuadro_base <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3) %>%
    group_by(TRIMESTRE, caes_seccion_label) %>%
    summarise(
      total = sum(PONDERA[CAT_OCUP == 3], na.rm = TRUE),
      Asal_protegidos = sum(PONDERA[CAT_OCUP == 3 & PP07H == 1], na.rm = TRUE),
      Asal_precarios_tot = sum(PONDERA[CAT_OCUP == 3 & PP07H == 2], na.rm = TRUE),
      Asal_precarios_i1_mono = sum(PONDERA[CAT_OCUP == 3 & PP07I == 1], na.rm = TRUE),
      Asal_precarios_i2_negr = sum(PONDERA[CAT_OCUP == 3 & PP07I == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_asal_prote = Asal_protegidos / total,
      part_asal_precarios = Asal_precarios_tot / total,
      part_mono_en_precarios = Asal_precarios_i1_mono / Asal_precarios_tot,
      part_negro_en_precarios = Asal_precarios_i2_negr / Asal_precarios_tot,
      part_NSNR_en_precarios = (total - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot
    )
  
  # Calcular fila de total por trimestre
  total_fila <- cuadro_base %>%
    group_by(TRIMESTRE) %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  # Unir resultados
  cuadro_final <- bind_rows(cuadro_base, total_fila) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función
c.7.2_rama_condicion_registro_final <- calcular_rama_condicion_registro_trimestral(base)

# Mostrar resultado # (Ver)
print(c.7.2_rama_condicion_registro_final)

#script 08


cuadro_precariedad_ambos_trimestral <- function(df) {
  indicadores <- df %>%
    group_by(TRIMESTRE) %>%
    summarise(
      Total = n(),
      "Signo 1: Educación-Tamaño" = sum(signo_educ_tamaño, na.rm = TRUE),
      "Signo 2: Sin Descuento Jubilatorio" = sum(signo_sindescuento, na.rm = TRUE),
      "Signo 3: Tiempo (PT involuntario + Indeterminado)" = sum(signo_tiempo, na.rm = TRUE),
      "Signo 4: Tecnología-Calificación" = sum(signo_tecno_calif, na.rm = TRUE),
      "Total con al menos 1 de 3" = sum(almenos1de3, na.rm = TRUE),
      "Total sin signos de precariedad" = sum(sin_preca_de3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -c(Total, TRIMESTRE), names_to = "Indicador", values_to = "Frecuencia_ambos") %>%
    mutate(Proporcion_ambos = round(Frecuencia_ambos / Total * 100, 2)) %>%
    pivot_wider(names_from = TRIMESTRE, values_from = c(Frecuencia_ambos, Proporcion_ambos))
  
  return(indicadores)
}

cuadro_precariedad_sexo_trimestral <- function(df) {
  indicadores <- df %>%
    group_by(TRIMESTRE, CH04) %>%  # CH04 es la columna de sexo
    summarise(
      Total = n(),
      "Signo 1: Educación-Tamaño" = sum(signo_educ_tamaño, na.rm = TRUE),
      "Signo 2: Sin Descuento Jubilatorio" = sum(signo_sindescuento, na.rm = TRUE),
      "Signo 3: Tiempo (PT involuntario + Indeterminado)" = sum(signo_tiempo, na.rm = TRUE),
      "Signo 4: Tecnología-Calificación" = sum(signo_tecno_calif, na.rm = TRUE),
      "Total con al menos 1 de 3" = sum(almenos1de3, na.rm = TRUE),
      "Total sin signos de precariedad" = sum(sin_preca_de3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -c(Total, CH04, TRIMESTRE), names_to = "Indicador", values_to = "Frecuencia") %>%
    mutate(Proporcion = round(Frecuencia / Total * 100, 2)) %>%
    pivot_wider(names_from = c(TRIMESTRE, CH04), values_from = c(Frecuencia, Proporcion)) 
  
  return(indicadores)
}

# Llamar ambas funciones
cuadro_general <- cuadro_precariedad_ambos_trimestral(base_asalariados)
cuadro_sexo <- cuadro_precariedad_sexo_trimestral(base_asalariados)

# Combinar los resultados
c.8_signos_preca_final <- bind_rows(cuadro_general, cuadro_sexo)
rm(cuadro_general, cuadro_sexo)

# Ver el resultado
print(c.8_signos_preca_final)
#ver script 09 #####
#
#script 91

tabla_preca_SS_sexo_trimestral <- calculate_tabulates(
  base = base_asalariados,
  x = c("TRIMESTRE", "CH04"),  # Sexo por trimestre (Ver : no anda)
  y = "signo_sindescuento",
  weights = "PONDERA"
)
print(tabla_preca_SS_sexo_trimestral)

# Análisis por nivel educativo (ambos sexos)
tabla_preca_SS_educ_trimestral <- calculate_tabulates(
  base = base_asalariados,
  x = c("TRIMESTRE", "NIVEL_ED"),
  y = "signo_sindescuento",
  weights = "PONDERA"
) %>%
  mutate(sexo = "Ambos")
print(tabla_preca_SS_educ_trimestral)

tabla_precaSS_educ_varon_trimestral <- base_asalariados %>%
  filter(CH04 == "1") %>%  # Solo varones
  calculate_tabulates(
    x = c("TRIMESTRE", "NIVEL_ED"),
    y = "signo_sindescuento",
    weights = "PONDERA"
  ) %>%
  mutate(sexo = "Varón")
print(tabla_precaSS_educ_varon_trimestral)

tabla_precaSS_educ_mujer_trimestral <- base_asalariados %>%
  filter(CH04 == "2") %>%  # Solo mujeres
  calculate_tabulates(
    x = c("TRIMESTRE", "NIVEL_ED"),
    y = "signo_sindescuento",
    weights = "PONDERA"
  ) %>%
  mutate(sexo = "Mujer")
print(tabla_precaSS_educ_mujer_trimestral)

# Consolidar las tablas
c.91_precaSS_educ_sexo_final <- bind_rows(
  tabla_preca_SS_educ_trimestral,
  tabla_precaSS_educ_varon_trimestral,
  tabla_precaSS_educ_mujer_trimestral
)

# Limpiar variables innecesarias
objetos <- c("tabla_precaSS_educ_varon_trimestral", "tabla_precaSS_educ_mujer_trimestral",
             "tabla_preca_SS_sexo_trimestral", "tabla_preca_SS_educ_trimestral")
rm(list = intersect(objetos, ls()))

# Ver resultado final
print(c.91_precaSS_educ_sexo_final)

