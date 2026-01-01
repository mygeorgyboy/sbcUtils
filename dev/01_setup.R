# ==============================================================================
# Script de configuración inicial del paquete sbcUtils
# ==============================================================================
# Ejecutar este script UNA VEZ después de crear el paquete
# ==============================================================================

library(devtools)
library(usethis)
library(roxygen2)

# Configuración básica del paquete ----
use_mit_license("Jorge Ramírez")
use_readme_md()
use_news_md()

# Configurar Git ----
use_git()

# Agregar dependencias ----
use_package("DBI", type = "Imports")
use_package("RMySQL", type = "Imports")
use_package("dplyr", type = "Imports")
use_package("tibble", type = "Imports")
use_package("glue", type = "Suggests")

# Configurar tests ----
use_testthat()

# Ignorar archivos ----
use_build_ignore("dev")
use_git_ignore("*.Rproj")
use_git_ignore(".Rproj.user")
use_git_ignore(".Rhistory")
use_git_ignore(".RData")
use_git_ignore("*.rds")

message("✓ Configuración inicial completada")
message("Siguiente paso: Ejecutar dev/02_document.R")
