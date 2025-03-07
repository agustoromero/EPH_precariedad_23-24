# Ejecutar los scripts en orden

# Carpeta 01_modificaciones a la base
source("02_scripts/01_modificaciones_a_la_base/01_carga_datos_y_librerias.R")
source("02_scripts/01_modificaciones_a_la_base/01_carga_datos_y_librerias.R")
source("02_scripts/01_modificaciones_a_la_base/02_variables_persona.R")
source("02_scripts/01_modificaciones_a_la_base/03_ocupados.R")
source("02_scripts/01_modificaciones_a_la_base/04_asalariados.R")
source("02_scripts/01_modificaciones_a_la_base/05_precariedad.R")

# Carpeta 02_funciones
source("02_scripts/02_funciones/01_c_estado_cond.act_sexo.R")
source("02_scripts/02_funciones/02_c_desocupados.R")
source("02_scripts/02_funciones/03_c_rama_tamanio_sexo.R")
source("02_scripts/02_funciones/04_c_tamanio.nueva_nivel_ed_rama_sexo.R")
source("02_scripts/02_funciones/05_c_rama_cond.registro_tipo.establecimiento.R")
source("02_scripts/02_funciones/06_c_estab_condicion_registro_tipo.R")
source("02_scripts/02_funciones/07_c_rama_condicion_registro.R")
source("02_scripts/02_funciones/08_c_signos_precariedad.R")
source("02_scripts/02_funciones/09_preca_tecno_calif_sexo_NIVEL_ED.R")
source("02_scripts/02_funciones/091_preca_sindescuento_sexo_niveled.R")

print("Ejecución completa de todos los scripts.")
