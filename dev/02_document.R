# ==============================================================================
# Script para documentar el paquete sbcUtils
# ==============================================================================
# Ejecutar cada vez que modifiques la documentación de funciones
# ==============================================================================

library(devtools)
library(roxygen2)

# Generar documentación ----
document()

# Verificar que todo esté correcto ----
check_man()

message("✓ Documentación generada")
message("Siguiente paso: Ejecutar dev/03_build.R para compilar")
