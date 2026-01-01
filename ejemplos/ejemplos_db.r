# ==============================================================================
# EJEMPLOS DE USO: Conexiones a MySQL y DB2
# Archivo: ejemplo.R
# ==============================================================================

# Cargar librerías necesarias ----
library(DBI)
library(glue)
library(tibble)
library(dplyr)
library(tidyverse)
library(sbcUtils)

# Si usas RMySQL
# library(RMySQL)

# Si usas RJDBC para DB2
# library(RJDBC)

# ==============================================================================
# 1. CONFIGURACIONES DE CONEXIÓN
# ==============================================================================

# Configuración MySQL ----
config_mysql <- list(
  tipo = "mysql",
  host = "localhost",
  user = "root",
  password = "mi_password_mysql",
  dbname = "mi_base_datos",
  port = 3306  # Opcional, 3306 es el default
)

# Configuración DB2 ----
config_db2 <- list(
  tipo = "db2",
  host = "sb1.mx",
  user = "db2inst1",
  password = "db2Secret0",
  dbname = "estudios",
  port = 50000,  # Opcional, 50000 es el default para DB2
  driver_path = "/opt/ibm/db2/java/db2jcc4.jar"  # O usa variable de entorno DB2_DRIVER_PATH
)

# También puedes usar variables de entorno para mayor seguridad
# Sys.setenv(DB2_DRIVER_PATH = "/opt/ibm/db2/java/db2jcc4.jar")

# ==============================================================================
# 2. EJEMPLO MYSQL - Acceder a 2 tablas
# ==============================================================================

cat("\n========== EJEMPLOS CON MYSQL ==========\n\n")

# Método 1: Conexión manual ----
cat("--- Método 1: Conexión Manual ---\n")

# Conectar
conn_mysql <- sbc_connect(config_mysql, tipo = "mysql")

# Listar todas las tablas disponibles
tablas_disponibles <- sbc_list_tables(conn_mysql)
print(tablas_disponibles)

# Consultar tabla 1: usuarios
usuarios <- sbc_query(conn_mysql, "SELECT * FROM usuarios LIMIT 10")
print(head(usuarios))

# Consultar tabla 2: productos
productos <- sbc_query(conn_mysql, "SELECT * FROM productos WHERE activo = 1")
print(head(productos))

# Verificar si existe una tabla antes de consultarla
if (sbc_table_exists(conn_mysql, "ventas")) {
  ventas <- sbc_query(conn_mysql, "SELECT * FROM ventas WHERE fecha > '2024-01-01'")
  print(nrow(ventas))
}

# Ver estructura de una tabla
info_usuarios <- sbc_table_info(conn_mysql, "usuarios")
print(info_usuarios)

# Cerrar conexión
sbc_disconnect(conn_mysql)


# Método 2: Gestión automática de conexión (RECOMENDADO) ----
cat("\n--- Método 2: Gestión Automática (Recomendado) ---\n")

datos_mysql <- sbc_with_connection(config_mysql, function(conn) {
  list(
    usuarios = sbc_query(conn, "SELECT * FROM usuarios LIMIT 10"),
    productos = sbc_query(conn, "SELECT * FROM productos WHERE activo = 1"),
    estadisticas = sbc_query(conn, "
      SELECT
      familia, categoria, subcategoria,
        COUNT(*) as casos
      FROM catalogo_productos
      GROUP BY familia, categoria, subcategoria order by casos desc
    ")
  )
})

# Acceder a los resultados
print(datos_mysql$usuarios)
print(datos_mysql$productos)
print(datos_mysql$estadisticas)


# Método 3: Con glue para consultas dinámicas ----
cat("\n--- Método 3: Consultas Dinámicas con glue ---\n")

# Supongamos que tienes tablas con sufijos numéricos
num_estudio <- 42

datos_estudio_mysql <- sbc_with_connection(config_mysql, function(conn) {
  list(
    encuestas = sbc_query(conn, glue("SELECT * FROM encuestas_{num_estudio}")),
    propiedades = sbc_query(conn, glue("SELECT * FROM encuestas_propiedades_{num_estudio}"))
  )
})

print(datos_estudio_mysql$encuestas)
print(datos_estudio_mysql)