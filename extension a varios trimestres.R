# Librerías
library(tidyverse)
library(eph)
library(haven)
library(dplyr)
library(readr)
library(openxlsx)

base <-eph::get_microdata( year = 2023:2024, period = 1:4, type = "individual")
base<- base %>% 
  mutate(anio_trim  = paste0(ANO4,"T",TRIMESTRE))

# Ruta donde están los archivos RDS #DESCARGAR y Guardar previamente en RDS 
ruta_datos <- "01_datos/"

# Definir los trimestres de interés
trimestres_seleccionados <- expand.grid( #genera todas las combinaciones posibles de los valores de los vectores que le pasamos.
  ANO4 = 2023:2024, 
  TRIMESTRE = 1:4
) %>%
  filter(!(ANO4 == 2023 & TRIMESTRE < 3),  # Excluir antes del 3T 2023
         !(ANO4 == 2024 & TRIMESTRE > 2))  # Excluir después del 2T 2024

# Crear una función para importar datos
importar_datos <- function(ano, trimestre) {
  archivo <- paste0(ruta_datos, "base_", ano, "_T", trimestre, ".rds")
  
  if (file.exists(archivo)) {
    datos <- readRDS(archivo) %>%
      mutate(anio_trim = paste0(ANO4, "_T", TRIMESTRE))  # Agregar columna
    
    return(datos)
  } else {
    message("No se encontró: ", archivo)
    return(NULL)
  }
}

# Importar y combinar todos los archivos #Interesa tenerlos por separado ? capaz si para corte trasversal 
lista_datos <- mapply(importar_datos, 
                      trimestres_seleccionados$ANO4, 
                      trimestres_seleccionados$TRIMESTRE, 
                      SIMPLIFY = FALSE)

# Unir todas las bases en una sola
datos_completos <- bind_rows(lista_datos)

# Vista de los datos importados
head(datos_completos)

base <- organize_labels(base, type = "individual")
base <- organize_caes(base)  #labels rama segun caes
base <- organize_cno(base)
# Guardar base sin modificaciones
saveRDS(base, "01_data/input_original/bases_originales.rds")


#######################


# Cargar base original
base <- readRDS("01_data/Input_original/bases_originales.rds")

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

###################################################################################################

# Cargar base
base <- readRDS("01_data/outputs_filtros/bases_personas.rds")

# Filtrar ocupados
base_ocupados <- base %>%
  filter(ESTADO == 1)

# Guardar base ocupados
saveRDS(base_ocupados, "01_data/outputs_filtros/bases_ocupados.rds")

################################################################################################

# Cargar base de ocupados
base_ocupados <- readRDS("01_data/outputs_filtros/bases_ocupados.rds")

# Filtrar asalariados
base_asalariados <- base_ocupados %>%
  filter(CAT_OCUP == 3) %>%
  mutate(
    tamanio.establec.nueva = case_when(
      PP04C == 1 ~ "uni",
      PP04C %in% c(2, 3, 4, 5) ~ "peque",
      PP04C %in% c(6, 7, 8) ~ "mediano",
      PP04C %in% c(9, 10, 11, 12) ~ "grande",
      PP04C99 == 1 ~ "peque",
      PP04C99 == 2 ~ "mediano",
      PP04C99 == 3 ~ "grande",
      PP04C99 == 9 ~ "NS/NR",
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

# Guardar base asalariados
saveRDS(base_asalariados, "01_data/outputs_filtros/bases_asalariados.rds")

###################################################################################################

# Cargar base de asalariados
base_asalariados <- readRDS("01_data/outputs_filtros/bases_asalariados.rds")

# Crear variables de precariedad
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
saveRDS(base_asalariados, "01_data/outputs_filtros/bases_precariedad.rds")



####################################################################################
##########################################################################################

#APLICAMOS ANALISIS A CADA TRIMESTRE Y UNIMOS

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

# Funciones para cálculos

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

# Ver los resultados
str(c.1.1_consolidado)
str(c.1.2_consolidado)
str(c.3_consolidado)

################################################################################

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

# Mostrar resultado consolidado
print(c.4_niveled_tamanio_sexo_consolidado)

########################################################################################

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


########################################################################################

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

#############################################################################

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




#############################################################################

#script 08



#############################################################################

#script 09


#############################################################################

#script 91