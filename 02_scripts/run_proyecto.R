# 🚀 Script maestro para ejecutar todo el flujo de análisis de precariedad laboral
source("run_proyecto.R")
# 1️⃣ Cargar datos
source("scripts/01_carga_datos_y_librerias.R")

# 2️⃣ Generar variables sociodemográficas
source("scripts/02_variables_persona.R")

# 3️⃣ Filtrar ocupados
source("scripts/03_ocupados.R")

# 4️⃣ Filtrar asalariados y agregar variables laborales
source("scripts/04_asalariados.R")

# 5️⃣ Calcular precariedad
source("scripts/05_precariedad.R")

# 🏁 Mensaje final
message("✅ Flujo completo ejecutado con éxito. Base lista para análisis en RMarkdown.")