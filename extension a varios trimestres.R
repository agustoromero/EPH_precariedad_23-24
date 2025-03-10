  # Librerías necesarias
  library(tidyverse)
  library(eph)
  library(haven)
  library(dplyr)
  library(readr)
  library(openxlsx)
  library(stringr)
  library(purrr)
  
  # Ruta donde se almacenan los archivos RDS
  ruta_datos <- "01_data/"
  
  # Definir los trimestres de interés (3T 2023 hasta 2T 2024)
  trimestres_seleccionados <- expand.grid(
    ANO4 = 2023:2024, 
    TRIMESTRE = 1:4
  ) %>%
    filter(!(ANO4 == 2023 & TRIMESTRE < 3),  # Excluir antes del 3T 2023
           !(ANO4 == 2024 & TRIMESTRE > 2))  # Excluir después del 2T 2024
  
  # Función para descargar y guardar datos en RDS #### 
# Tiltie esta parte para no repetir con las lineas 40--52
  descargar_datos <- function(ano, trimestre) {
    archivo <- paste0(ruta_datos, "base_", ano, "_T", trimestre, ".rds")

    if (!file.exists(archivo)) {  # Descargar solo si el archivo no existe
      datos <- get_microdata(year = ano, period = trimestre, type = "individual")
      saveRDS(datos, file = archivo)
      message("Descargado y guardado: ", archivo)
    } else {
      message("Ya existe: ", archivo)
    }
  }


  # Descargar los archivos necesarios####
  mapply(descargar_datos, 
         trimestres_seleccionados$ANO4, 
         trimestres_seleccionados$TRIMESTRE)
  
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
  
  # Importar y combinar todos los archivos
  lista_datos <- lapply(1:nrow(trimestres_seleccionados), function(i) {
    importar_datos(trimestres_seleccionados$ANO4[i], 
                   trimestres_seleccionados$TRIMESTRE[i])
  })
  
  # Unir todas las bases en una sola ####
  datos_completos <- bind_rows(lista_datos)
  # Organizar etiquetas y clasificaciones
  datos_completos <- datos_completos %>%
    organize_labels(type = "individual") %>%
    organize_caes() %>%   # Etiquetas según CAES
    organize_cno()        # Clasificación según CNO
  
  # Guardar base consolidada
  saveRDS(datos_completos, "01_data/input_original/bases_originales.rds")
  
# Vista de los datos importados
head(datos_completos)

# Guardar base consolidada
saveRDS(datos_completos, "01_data/input_original/bases_originales.rds")

##########################################################################3
# Comienza el juego de las bases #### 
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
    nivel.ed1 = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria", ###¿primario?
      NIVEL_ED %in% 4 ~ "Secundaria Completa",
      NIVEL_ED == 5 ~ "Superior Incompleto",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Incompleto","Superior Completo"))
  )

# Guardar base con variables personales 
saveRDS(base, "01_data/outputs_filtros/bases_personas.rds")
#¿otra base mas? ya esta guardada como bases_originales y cómo datos completos. Me quedaría con una sola.
###################################################################################################

# # Cargar base
# base <- readRDS("01_data/outputs_filtros/bases_personas.rds")
# 
# Filtrar ocupados
base_ocupados <- base %>%
  filter(ESTADO == 1)

# Guardar base ocupados
saveRDS(base_ocupados, "01_data/outputs_filtros/bases_ocupados.rds")

################################################################################################

# Cargar base de ocupados
base_ocupados <- readRDS("01_data/outputs_filtros/bases_ocupados.rds")

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
base_asalariados <- base_ocupados %>% filter(CAT_OCUP==3)
# Guardar base asalariados
saveRDS(base_ocupados, "01_data/outputs_filtros/bases_asalariados.rds")
#De nuevo, quizás no tiene sentido guardar una nueva base.####
###################################################################################################

# Cargar base de asalariados
base_asalariados <- readRDS("01_data/outputs_filtros/bases_asalariados.rds")

# Crear variables de precariedad ####
#Acá hay variables para los asalariados que se pueden aplicar para todos (ver )
#las variables de nivel.ed y tamano ya estan creadas (ver )
base_asalariados <- base_asalariados %>%
  mutate(
    # Signo de precariedad tecnológica y de calificación
    preca_tecno_calif = case_when(
      TECNOLOGIA == 1 & CALIFICACION == 1 ~ 4,  # Solo cuando ambos son 1
      TRUE ~ 0 ),
    
    # Clasificación de educación
    nivel.ed = factor(case_when(
      NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
      NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
      NIVEL_ED == 6 ~ "Superior Completo",
      TRUE ~ "Ns/Nr"
    ), levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    
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

# Guardar base con precariedad
#acá tiene otro sentido guardarla porque tenes cargadas las variables de precariedad, igual se me hacen muchas bases.
saveRDS(base_asalariados, "01_data/outputs_filtros/bases_precariedad.rds")

##########################################################################################

#APLICAMOS ANALISIS A CADA TRIMESTRE Y UNIMOS

# # Filtrar por Año y Trimestre con reconocimiento de texto
# Función para filtrar por Año y Trimestre usando solo la parte numérica de TRIMESTRE
filtrar_por_trimestre <- function(df, anio, trimestre) {
  # Reconstruir anio_trim extrayendo solo el dígito inicial de TRIMESTRE
  df <- df %>%
    mutate(anio_trim = paste(ANO4, "T", str_extract(TRIMESTRE, "^[0-9]+"), sep = ""))
  
  # Definir el valor target, por ejemplo "2023T3"
  target <- paste(anio, "T", trimestre, sep = "")
  message("Filtrando por target: ", target)
  
  # Filtrar el dataframe
  df_filtrado <- df %>%
    filter(anio_trim == target)
  
  message("Filas filtradas: ", nrow(df_filtrado))
  return(df_filtrado)
}


# Función para cálculos: cuadro c.1.1
calcular_poblacion_estado <- function(df) {
  # Asegurarnos de que ESTADO sea un factor
  df$ESTADO <- as.factor(df$ESTADO)
  
  c.1.1_estado_sexo <- df %>%
    group_by(ESTADO, anio_trim) %>%  # Utilizamos anio_trim ya corregido
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

# Definir los trimestres de interés usando el formato "YYYYTn" deseado
trimestres <- list(
  "2023_3" = filtrar_por_trimestre(base, "2023", 3),
  "2023_4" = filtrar_por_trimestre(base, "2023", 4),
  "2024_1" = filtrar_por_trimestre(base, "2024", 1),
  "2024_2" = filtrar_por_trimestre(base, "2024", 2)
)

# Aplicar la función a cada trimestre y consolidar los resultados
c.1.1_resultados <- lapply(trimestres, calcular_poblacion_estado)

# Unir los resultados por trimestre
c.1.1_consolidado <- bind_rows(c.1.1_resultados)

# Agregar proporciones al consolidado c.1.1
c.1.1_consolidado <- c.1.1_consolidado %>%
  mutate(
    Poblacion_Ambos = as.numeric(Poblacion_Ambos),
    Poblacion_Varones = as.numeric(Poblacion_Varones),
    Poblacion_Mujeres = as.numeric(Poblacion_Mujeres),
    Prop_Varones = ifelse(Poblacion_Ambos > 0, Poblacion_Varones / Poblacion_Ambos, 0),
    Prop_Mujeres = ifelse(Poblacion_Ambos > 0, Poblacion_Mujeres / Poblacion_Ambos, 0)
  ) %>%
  replace_na(list(Prop_Varones = 0, Prop_Mujeres = 0))  # Evitar NaN

# Verificar los resultados: La columna anio_trim debe mostrar, por ejemplo, "2023T3"
str(c.1.1_consolidado)

##############
calcular_poblacion_estado_cat_ocup <- function(df) {
  df_ocupado <- df %>%
    # Forzamos el formato de anio_trim: extraer solo "YYYYTn"
    mutate(anio_trim = str_extract(as.character(anio_trim), "^[0-9]{4}T[0-9]")) %>%
    filter(ESTADO == 1)
  
  df_ocupado$ESTADO <- as.factor(df_ocupado$ESTADO)
  df_ocupado$CAT_OCUP <- as.factor(df_ocupado$CAT_OCUP)
  
  c.1.2_cat_ocup <- df_ocupado %>%
    group_by(CAT_OCUP, anio_trim) %>%  
    summarise(
      Poblacion_Ambos   = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(Poblacion_Ambos:Poblacion_Mujeres, ~ replace(., is.na(.), "-")))
  
  levels(c.1.2_cat_ocup$CAT_OCUP) <- c("Patrón", "Cuenta propia", "Obrero o empleado", 
                                       "Trabajador familiar sin remuneración", "Ns./Nr.")
  
  return(c.1.2_cat_ocup)
}

# Aplicar la función a cada trimestre y consolidar los resultados
c.1.2_resultados <- lapply(trimestres, calcular_poblacion_estado_cat_ocup)
c.1.2_consolidado <- bind_rows(c.1.2_resultados) %>%
  mutate(
    Poblacion_Ambos   = as.numeric(Poblacion_Ambos),
    Poblacion_Varones = as.numeric(Poblacion_Varones),
    Poblacion_Mujeres = as.numeric(Poblacion_Mujeres),
    Prop_Varones      = ifelse(Poblacion_Ambos > 0, Poblacion_Varones / Poblacion_Ambos, 0),
    Prop_Mujeres      = ifelse(Poblacion_Ambos > 0, Poblacion_Mujeres / Poblacion_Ambos, 0)
  ) %>%
  replace_na(list(Prop_Varones = 0, Prop_Mujeres = 0))

# Verificar los resultados
str(c.1.2_consolidado)


######################################################################

#script03           
# 

calcular_rama_tamanio_sexo <- function(df) {
  cuadro_base <- df %>%
    # Forzamos el formato de anio_trim: extraer solo "YYYYTn"
    mutate(anio_trim = str_extract(as.character(anio_trim), "^[0-9]{4}T[0-9]")) %>%
    filter(ESTADO == 1 & CAT_OCUP == 3) %>%
    group_by(caes_seccion_label, tamanio.establec.nueva, anio_trim) %>%
    summarise(
      Poblacion_Ambos   = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos   = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
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
      caes_seccion_label     = "Total",
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )

  cuadro_final <- bind_rows(cuadro_base, total_rama, total_general) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))

  return(cuadro_final)
}

# Aplicar la función a la base_asalariados para cada trimestre
c.3_resultados <- base_asalariados %>%
  group_by(anio_trim) %>%
  do(calcular_rama_tamanio_sexo(.))

# Consolidar los resultados en una tabla final
c.3_consolidado <- bind_rows(c.3_resultados)

# Verificar los resultados
str(c.3_consolidado)



######################################################################
#script 04

calcular_niveled_tamanio_ocupacion <- function(df) {
  cuadro_base <- df %>%
    mutate(anio_trim = str_extract(as.character(anio_trim), "^[0-9]{4}T[0-9]")) %>%
    filter(ESTADO == 1 & CAT_OCUP == 3) %>%
    group_by(nivel.ed1, tamanio.establec.nueva, anio_trim) %>%
    summarise(
      Poblacion_Ambos   = sum(PONDERA, na.rm = TRUE),
      Poblacion_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Poblacion_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Prop_Ambos   = Poblacion_Ambos / sum(Poblacion_Ambos, na.rm = TRUE),
      Prop_Varones = Poblacion_Varones / Poblacion_Ambos,
      Prop_Mujeres = Poblacion_Mujeres / Poblacion_Ambos
    )
  
  # Asegurarse de que todas las columnas sean de tipo correcto
  cuadro_base <- cuadro_base %>%
    mutate(across(where(is.labelled), as.character)) # Convierte las columnas 'labelled' a 'character'
  
  total_rama <- cuadro_base %>%
    group_by(nivel.ed1) %>%
    summarise(
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  total_general <- cuadro_base %>%
    summarise(
      nivel.ed1     = "Total",
      tamanio.establec.nueva = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  cuadro_final <- bind_rows(cuadro_base, total_rama, total_general) %>%
    mutate(across(everything(), ~ replace(., is.na(.), "-")))
  
  return(cuadro_final)
}

# Aplicar la función a la base_asalariados para cada trimestre
c.4_resultados <- base_asalariados %>%
  group_by(anio_trim) %>%
  do(calcular_niveled_tamanio_ocupacion(.))

# Consolidar los resultados en una tabla final
c.4_consolidado <- bind_rows(c.4_resultados)

# Verificar los resultados
str(c.4_consolidado)



########################################################################################

#sript 05


calcular_rama_estab_condicion_registro <- function(df, tipo_estab = NULL, etiqueta_estab) {
  # Convertir PP04A a numérico para evitar errores de tipo
  df <- df %>% mutate(PP04A = as.numeric(PP04A))
  
  if (!is.null(tipo_estab)) {
    df <- df %>% filter(PP04A == tipo_estab)  # Filtrar por tipo de establecimiento si se especifica
  }
  
  # Forzar formato de anio_trim "YYYYTn"
  df <- df %>% mutate(anio_trim = str_extract(as.character(anio_trim), "^[0-9]{4}T[0-9]"))
  
  cuadro_base <- df %>%
    filter(ESTADO == 1, CAT_OCUP == 3) %>%
    group_by(anio_trim, caes_seccion_label) %>%  
    summarise(
      total                 = sum(PONDERA, na.rm = TRUE),
      Asal_protegidos       = sum(PONDERA[PP07H == 1], na.rm = TRUE),
      Asal_precarios_tot    = sum(PONDERA[PP07H == 2], na.rm = TRUE),
      Asal_precarios_i1_mono = sum(PONDERA[PP07H == 2 & PP07I == 1], na.rm = TRUE),
      Asal_precarios_i2_negr = sum(PONDERA[PP07H == 2 & PP07I == 2], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      part_asal_prote         = Asal_protegidos / total,
      part_asal_precarios     = Asal_precarios_tot / total,
      part_mono_en_precarios  = Asal_precarios_i1_mono / Asal_precarios_tot,
      part_negro_en_precarios = Asal_precarios_i2_negr / Asal_precarios_tot,
      part_NSNR_en_precarios  = (total - Asal_precarios_i1_mono - Asal_precarios_i2_negr) / Asal_precarios_tot
    )
  
  # Calcular fila de total
  total_fila <- cuadro_base %>%
    summarise(
      caes_seccion_label = "Total",
      across(where(is.numeric), sum, na.rm = TRUE)
    )
  
  cuadro_rama_estab_condicion_registro <- bind_rows(cuadro_base, total_fila) %>%
    mutate(tipo_establecimiento = etiqueta_estab)
  
  return(cuadro_rama_estab_condicion_registro)
}

# Generar el análisis por trimestres sin usar lapply
c.5_rama_cond.registro_tipo.establec_consolidado <- map_dfr(trimestres, function(df) {
  cuadro_total   <- calcular_rama_estab_condicion_registro(df, NULL, "Total")
  cuadro_estatal <- calcular_rama_estab_condicion_registro(df, 1, "Estatal")
  cuadro_privado <- calcular_rama_estab_condicion_registro(df, 2, "Privado")
  cuadro_otro    <- calcular_rama_estab_condicion_registro(df, 3, "Otro")
  
  bind_rows(cuadro_total, cuadro_estatal, cuadro_privado, cuadro_otro)
})

# Mostrar resultado final consolidado
print(c.5_rama_cond.registro_tipo.establec_consolidado)



#############################################################################

#script 06

cuadro_precariedad_trimestral <- function(df) {
  cuadro_base <- df %>%
    filter(ESTADO == 1 & CAT_OCUP == 3) %>%
    group_by(anio_trim) %>%
    summarise(
      Total_Ambos   = sum(PONDERA, na.rm = TRUE),
      Total_Varones = sum(PONDERA[CH04 == 1], na.rm = TRUE),
      Total_Mujeres = sum(PONDERA[CH04 == 2], na.rm = TRUE),
      
      Signo1_Ambos  = sum(PONDERA[signo_educ_tamaño == 1], na.rm = TRUE),
      Signo1_Varones = sum(PONDERA[CH04 == 1 & signo_educ_tamaño == 1], na.rm = TRUE),
      Signo1_Mujeres = sum(PONDERA[CH04 == 2 & signo_educ_tamaño == 1], na.rm = TRUE),
      
      Signo2_Ambos  = sum(PONDERA[signo_sindescuento == 1], na.rm = TRUE),
      Signo2_Varones = sum(PONDERA[CH04 == 1 & signo_sindescuento == 1], na.rm = TRUE),
      Signo2_Mujeres = sum(PONDERA[CH04 == 2 & signo_sindescuento == 1], na.rm = TRUE),
      
      Signo3_Ambos  = sum(PONDERA[signo_tiempo == 1], na.rm = TRUE),
      Signo3_Varones = sum(PONDERA[CH04 == 1 & signo_tiempo == 1], na.rm = TRUE),
      Signo3_Mujeres = sum(PONDERA[CH04 == 2 & signo_tiempo == 1], na.rm = TRUE),
      
      Signo4_Ambos  = sum(PONDERA[signo_tecno_calif == 1], na.rm = TRUE),
      Signo4_Varones = sum(PONDERA[CH04 == 1 & signo_tecno_calif == 1], na.rm = TRUE),
      Signo4_Mujeres = sum(PONDERA[CH04 == 2 & signo_tecno_calif == 1], na.rm = TRUE),
      
      Almenos1de3_Ambos = sum(PONDERA[almenos1de3 == 1], na.rm = TRUE),
      Almenos1de3_Varones = sum(PONDERA[CH04 == 1 & almenos1de3 == 1], na.rm = TRUE),
      Almenos1de3_Mujeres = sum(PONDERA[CH04 == 2 & almenos1de3 == 1], na.rm = TRUE),
      
      Sin_preca_Ambos = sum(PONDERA[sin_preca_de3 == 1], na.rm = TRUE),
      Sin_preca_Varones = sum(PONDERA[CH04 == 1 & sin_preca_de3 == 1], na.rm = TRUE),
      Sin_preca_Mujeres = sum(PONDERA[CH04 == 2 & sin_preca_de3 == 1], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -anio_trim, names_to = "Categoria", values_to = "Frecuencia") %>%
    mutate(Sexo = case_when(
      str_detect(Categoria, "_Ambos$") ~ "Ambos",
      str_detect(Categoria, "_Varones$") ~ "Varones",
      str_detect(Categoria, "_Mujeres$") ~ "Mujeres"
    )) %>%
    mutate(Indicador = str_remove(Categoria, "_(Ambos|Varones|Mujeres)$")) %>%
    select(anio_trim, Sexo, Indicador, Frecuencia) %>%
    pivot_wider(names_from = Indicador, values_from = Frecuencia) %>%
    mutate(Proporcion = round(Almenos1de3 / Total * 100, 2)) %>%
    rename(Poblacion = Total)
  
  return(cuadro_base)
}

# Aplicar la función y consolidar los resultados
c.6_signos_preca_consolidado <- base_asalariados %>%
  group_by(anio_trim) %>%
  do(cuadro_precariedad_trimestral(.)) %>%
  ungroup()

# Verificar estructura
str(c.6_signos_preca_consolidado)

#############################################################################

#script 09
#script 91

# Función para calcular la tabla de precariedad sin descuento por sexo, nivel educativo y trimestre
calcular_precariedad_sexo_trimestral <- function(base) {
  
  # Tabla para Ambos sexos, incluyendo nivel educativo
  tabla_preca_SS_sexo_trimestral <- calculate_tabulates(
    base = base,
    x = "anio_trim",  # Trimestre
    y = "nivel.ed1",   # Nivel educativo
    weights = "PONDERA"
  ) %>%
    mutate(sexo = "Ambos")  # Se agrega etiqueta general para ambos sexos
  
  # Tabla para varones, filtrando por sexo y calculando por nivel educativo
  tabla_precaSS_educ_varon_trimestral <- base %>%
    filter(CH04 == 1) %>%  # Filtrar para varones (CH04 == 1)
    calculate_tabulates(
      x = "anio_trim",  # Trimestre
      y = "nivel.ed1",   # Nivel educativo
      weights = "PONDERA"
    ) %>%
    mutate(sexo = "Varón")
  
  # Tabla para mujeres, filtrando por sexo y calculando por nivel educativo
  tabla_precaSS_educ_mujer_trimestral <- base %>%
    filter(CH04 == 2) %>%  # Filtrar para mujeres (CH04 == 2)
    calculate_tabulates(
      x = "anio_trim",  # Trimestre
      y = "nivel.ed1",   # Nivel educativo
      weights = "PONDERA"
    ) %>%
    mutate(sexo = "Mujer")
  
  # Combinar las tablas
  resultado_final <- bind_rows(
    tabla_preca_SS_sexo_trimestral,
    tabla_precaSS_educ_varon_trimestral,
    tabla_precaSS_educ_mujer_trimestral
  )
  
  # Limpiar variables innecesarias
  objetos <- c("tabla_precaSS_educ_varon_trimestral", "tabla_precaSS_educ_mujer_trimestral",
               "tabla_preca_SS_sexo_trimestral")
  rm(list = intersect(objetos, ls()))
  
  # Eliminar columna `total` si existe
  if ("total" %in% names(resultado_final)) {
    resultado_final <- resultado_final %>% select(-total)
  }
  
  return(resultado_final)
}

# Aplicar la función a la base de datos
c.91_precaSS_educ_sexo_final <- calcular_precariedad_sexo_trimestral(base_asalariados)

# Ver el resultado final
print(c.91_precaSS_educ_sexo_final)

# calculate_tabulates <- function(base, x, y, weights) {
#   # Validar que `x`, `y` y `weights` existan en la base
#   if (!all(x %in% names(base))) stop("Algunas variables de 'x' no están en la base de datos.")
#   if (!(y %in% names(base))) stop("La variable 'y' no está en la base de datos.")
#   if (!(weights %in% names(base))) stop("La variable de ponderación no está en la base de datos.")
#   
#   base %>%
#     group_by(across(all_of(x))) %>%  
#     summarise(
#       total = sum(!!sym(weights), na.rm = TRUE),
#       conteo = sum(!!sym(weights) * as.numeric(!!sym(y) > 0), na.rm = TRUE),  # Asegurar binarización
#       proporcion = conteo / total * 100,
#       .groups = "drop"
#     )
# }

# 
# total = 50000 significa que los varones del trimestre 2024T1 representan 50,000 personas en la población.
# conteo = 10000 significa que 10,000 de esas personas tienen y > 0.
# proporcion = 20% significa que el 20% de los varones en 2024T1 tienen y > 0.