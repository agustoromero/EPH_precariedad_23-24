# Librerías
library(tidyverse)
library(eph)
library(haven)

# Descarga y organización de la base
base <- get_microdata(year = 2023, trimester = 3, type = "individual", vars = "all") 
 base <- organize_labels(base, type = "individual")
 base <- organize_caes(base)  #labels rama segun caes
 base <- organize_cno(base)
# Guardar base sin modificaciones
saveRDS(base, "01_data/input_original/base_original.rds")