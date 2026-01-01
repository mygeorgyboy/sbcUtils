# ==============================================================================
# Script para compilar e instalar el paquete sbcUtils
# ==============================================================================
# Ejecutar cada vez que agregues/modifiques funciones
# ==============================================================================

library(devtools)

# Limpiar entorno ----
clean_dll()

# Documentar ----
document()

# Verificar el paquete (buscar errores) ----
check()

# Instalar localmente ----
install()

message("✓ Paquete compilado e instalado")
message("Puedes usar: library(sbcUtils)")
