# ==============================================================================
# Script para actualizar GitHub con los cambios
# ==============================================================================
# Ejecutar cada vez que quieras subir cambios a GitHub
# ==============================================================================

library(gert)

# Agregar todos los cambios ----
git_add(".")

# Crear commit ----
commit_message <- readline("Mensaje del commit: ")
if (commit_message == "") {
  commit_message <- paste("Update:", Sys.Date())
}

git_commit(commit_message)

# Subir a GitHub ----
git_push()

message("✓ Cambios subidos a GitHub")

