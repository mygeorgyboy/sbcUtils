# ==============================================================================
# Funciones para gestión de conexiones a bases de datos
# Soporta: MySQL, DB2
# ==============================================================================

# Utilidades internas ----

#' Operador null-coalescing para valores por defecto
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Obtener driver apropiado según tipo de base de datos
#'
#' @param tipo Tipo de base de datos: "mysql" o "db2"
#' @param driver_path Ruta al driver JDBC (solo para DB2)
#' @return Objeto driver DBI
#' @keywords internal
.sbc_get_driver <- function(tipo, driver_path = NULL) {
  tipo <- tolower(tipo)

  switch(
    tipo,
    mysql = {
      if (!requireNamespace("RMySQL", quietly = TRUE)) {
        stop(
          "Paquete 'RMySQL' no está instalado. Instálalo con: install.packages('RMySQL')",
          call. = FALSE
        )
      }
      RMySQL::MySQL()
    },
    db2 = {
      if (!requireNamespace("RJDBC", quietly = TRUE)) {
        stop(
          "Paquete 'RJDBC' no está instalado. Instálalo con: install.packages('RJDBC')",
          call. = FALSE
        )
      }

      path <- driver_path %||% Sys.getenv("DB2_DRIVER_PATH")

      if (path == "" || !file.exists(path)) {
        stop(
          "Driver JDBC de DB2 no encontrado.\n",
          "  • Define la variable de entorno DB2_DRIVER_PATH, o\n",
          "  • Pasa 'driver_path' en la configuración\n",
          "  • Ejemplo: config$driver_path <- '/ruta/a/db2jcc4.jar'",
          call. = FALSE
        )
      }

      RJDBC::JDBC("com.ibm.db2.jcc.DB2Driver", path)
    },
    stop(
      "Tipo de base de datos no soportado: '",
      tipo,
      "'.\n",
      "  Tipos válidos: 'mysql', 'db2'",
      call. = FALSE
    )
  )
}

#' Construir parámetros de conexión según tipo de BD
#'
#' @param tipo Tipo de base de datos ("mysql" o "db2")
#' @param config Lista con configuración de conexión
#' @return Lista de parámetros para DBI::dbConnect()
#' @keywords internal
.sbc_build_connection_params <- function(tipo, config) {
  tipo <- tolower(tipo)

  switch(
    tipo,
    mysql = {
      list(
        drv = .sbc_get_driver("mysql"),
        host = config$host,
        user = config$user,
        password = config$password,
        dbname = config$dbname,
        port = as.integer(config$port %||% 3306L)
      )
    },
    db2 = {
      port <- as.integer(config$port %||% 50000L)

      # Construir URL JDBC para DB2
      jdbc_url <- sprintf(
        "jdbc:db2://%s:%d/%s",
        config$host,
        port,
        config$dbname
      )

      list(
        drv = .sbc_get_driver("db2", config$driver_path),
        url = jdbc_url,
        user = config$user,
        password = config$password
      )
    }
  )
}

#' Validar configuración de conexión
#'
#' @param config Lista con parámetros de conexión
#' @param tipo Tipo de base de datos
#' @return TRUE si válida, lanza error en caso contrario
#' @keywords internal
.sbc_validate_config <- function(config, tipo) {
  # Campos requeridos comunes
  required_fields <- c("host", "user", "password", "dbname")

  # Campos adicionales según tipo
  if (tolower(tipo) == "db2") {
    # Para DB2, driver_path es opcional si existe DB2_DRIVER_PATH
    if (is.null(config$driver_path) && Sys.getenv("DB2_DRIVER_PATH") == "") {
      message(
        "Nota: No se especificó 'driver_path' en config.\n",
        "      Se usará la variable de entorno DB2_DRIVER_PATH si existe."
      )
    }
  }

  # Validar campos requeridos
  missing_fields <- setdiff(required_fields, names(config))

  if (length(missing_fields) > 0) {
    stop(
      "Faltan campos requeridos en configuración:\n",
      "  • ",
      paste(missing_fields, collapse = "\n  • "),
      call. = FALSE
    )
  }

  # Validar que no sean vacíos
  empty_fields <- names(config)[sapply(config[required_fields], function(x) {
    is.null(x) || (is.character(x) && trimws(x) == "")
  })]

  if (length(empty_fields) > 0) {
    stop(
      "Los siguientes campos están vacíos:\n",
      "  • ",
      paste(empty_fields, collapse = "\n  • "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Funciones públicas ----

#' Conectar a base de datos (MySQL o DB2)
#'
#' Establece una conexión a una base de datos MySQL o DB2 según la configuración
#' proporcionada. La conexión debe cerrarse manualmente con \code{sbc_disconnect()}
#' o usar \code{sbc_with_connection()} para gestión automática.
#'
#' @param config Lista con parámetros de conexión requeridos:
#'   \itemize{
#'     \item \strong{tipo}: Tipo de base de datos ("mysql" o "db2")
#'     \item \strong{host}: Dirección del servidor (ej: "localhost", "192.168.1.100")
#'     \item \strong{user}: Usuario de la base de datos
#'     \item \strong{password}: Contraseña del usuario
#'     \item \strong{dbname}: Nombre de la base de datos
#'     \item \strong{port}: Puerto (opcional). Por defecto: 3306 para MySQL, 50000 para DB2
#'     \item \strong{driver_path}: Ruta al JAR del driver JDBC (solo DB2, opcional si existe DB2_DRIVER_PATH)
#'   }
#'
#' @return Objeto de conexión DBI con atributos adicionales:
#'   \itemize{
#'     \item \code{sbc_tipo}: Tipo de base de datos
#'     \item \code{sbc_dbname}: Nombre de la base de datos
#'     \item \code{sbc_host}: Host de conexión
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Conexión a MySQL
#' config_mysql <- list(
#'   tipo = "mysql",
#'   host = "localhost",
#'   user = "root",
#'   password = "mi_password",
#'   dbname = "mi_base_datos"
#' )
#' conn_mysql <- sbc_connect(config_mysql)
#'
#' # Conexión a DB2
#' config_db2 <- list(
#'   tipo = "db2",
#'   host = "servidor.empresa.com",
#'   user = "db2inst1",
#'   password = "password_db2",
#'   dbname = "estudios",
#'   port = 50000,
#'   driver_path = "/opt/ibm/db2/java/db2jcc4.jar"
#' )
#' conn_db2 <- sbc_connect(config_db2)
#'
#' # Usar la conexión
#' result <- sbc_query(conn_mysql, "SELECT * FROM tabla LIMIT 10")
#'
#' # Cerrar conexión
#' sbc_disconnect(conn_mysql)
#' }
sbc_connect <- function(config) {
  # Validar que existe config$tipo
  if (is.null(config$tipo)) {
    stop(
      "No se pudo determinar el tipo de base de datos.\n\n",
      "La configuración debe incluir el campo 'tipo':\n",
      "   config <- list(tipo = 'mysql', host = ..., user = ..., ...)\n\n",
      "Tipos válidos: 'mysql' o 'db2'",
      call. = FALSE
    )
  }

  tipo <- tolower(config$tipo)

  # Validar configuración
  .sbc_validate_config(config, tipo)

  # Construir parámetros de conexión
  params <- .sbc_build_connection_params(tipo, config)

  # Intentar conexión
  tryCatch(
    {
      conn <- do.call(DBI::dbConnect, params)

      # Agregar metadata a la conexión para rastreo
      attr(conn, "sbc_tipo") <- tipo
      attr(conn, "sbc_dbname") <- config$dbname
      attr(conn, "sbc_host") <- config$host
      attr(conn, "sbc_timestamp") <- Sys.time()

      message(sprintf(
        "✓ Conexión exitosa a %s\n  Host: %s | Base de datos: %s",
        toupper(tipo),
        config$host,
        config$dbname
      ))

      conn
    },
    error = function(e) {
      stop(
        "Error al conectar a ",
        toupper(tipo),
        ":\n",
        "  Host: ",
        config$host,
        "\n",
        "  Base de datos: ",
        config$dbname,
        "\n",
        "  Error: ",
        e$message,
        call. = FALSE
      )
    }
  )
}

#' Conectar a base de datos MySQL (función de compatibilidad)
#'
#' Wrapper de \code{sbc_connect()} específico para MySQL.
#' Mantiene compatibilidad con código existente.
#'
#' @param config Lista con parámetros de conexión (host, user, password, dbname, port)
#' @return Objeto de conexión DBI
#' @export
#'
#' @seealso \code{\link{sbc_connect}}
sbc_connect_mysql <- function(config) {
  # Asegurar que el tipo sea mysql
  config$tipo <- "mysql"
  sbc_connect(config)
}

#' Desconectar de base de datos
#'
#' Cierra una conexión activa a la base de datos de forma segura.
#'
#' @param conn Objeto de conexión DBI obtenido de \code{sbc_connect()}
#' @return \code{TRUE} invisiblemente si exitoso, \code{FALSE} si la conexión ya estaba cerrada
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- sbc_connect(config)
#' # ... usar conexión ...
#' sbc_disconnect(conn)
#' }
sbc_disconnect <- function(conn) {
  if (is.null(conn)) {
    warning("Conexión es NULL, no hay nada que cerrar")
    return(invisible(FALSE))
  }

  if (!DBI::dbIsValid(conn)) {
    warning("Conexión ya estaba cerrada o no es válida")
    return(invisible(FALSE))
  }

  # Recuperar metadata
  tipo <- attr(conn, "sbc_tipo") %||% "base de datos"
  dbname <- attr(conn, "sbc_dbname") %||% ""
  host <- attr(conn, "sbc_host") %||% ""

  # Cerrar conexión
  DBI::dbDisconnect(conn)

  # Mensaje informativo
  label <- toupper(tipo)
  if (dbname != "") {
    label <- paste0(label, " (", dbname, ")")
  }
  if (host != "") {
    label <- paste0(label, " @ ", host)
  }

  message(sprintf("✓ Conexión a %s cerrada", label))

  invisible(TRUE)
}

#' Desconectar de MySQL (función de compatibilidad)
#'
#' Wrapper de \code{sbc_disconnect()} para mantener compatibilidad.
#'
#' @param conn Objeto de conexión DBI
#' @return \code{TRUE} si exitoso
#' @export
#'
#' @seealso \code{\link{sbc_disconnect}}
sbc_disconnect_mysql <- function(conn) {
  sbc_disconnect(conn)
}

#' Ejecutar consulta SQL
#'
#' Ejecuta una consulta SQL y devuelve los resultados como tibble.
#' Soporta consultas parametrizadas y normalización automática de nombres de columnas.
#'
#' **Nota sobre tipos de datos MySQL:** Esta función suprime warnings del driver RMySQL
#' relacionados con tipos de datos no reconocidos (TIMESTAMP/tipo 7 y JSON/tipo 245).
#' Estos campos se importan correctamente como \code{character} y pueden convertirse
#' posteriormente al tipo apropiado si es necesario.
#'
#' @param conn Objeto de conexión DBI válido
#' @param sql Consulta SQL a ejecutar. Puede usar \code{glue::glue()} para interpolación
#' @param params Lista de parámetros para consultas parametrizadas (opcional).
#'   Usar \code{?} como placeholder en el SQL
#' @param normalizar_columnas Convertir nombres de columnas a minúsculas (default: \code{TRUE})
#' @param mostrar_info Mostrar mensaje con número de registros obtenidos (default: \code{TRUE})
#'
#' @return tibble con los resultados de la consulta
#' @export
#'
#' @examples
#' \dontrun{
#' # Consulta simple
#' result <- sbc_query(conn, "SELECT * FROM tabla WHERE id > 100")
#'
#' # Con glue para interpolación
#' num_estudio <- 42
#' result <- sbc_query(conn, glue::glue("SELECT * FROM encuestas_{num_estudio}"))
#'
#' # Consulta parametrizada (evita SQL injection)
#' result <- sbc_query(
#'   conn,
#'   "SELECT * FROM usuarios WHERE id = ? AND activo = ?",
#'   params = list(123, TRUE)
#' )
#'
#' # Sin normalizar columnas
#' result <- sbc_query(conn, "SELECT * FROM tabla", normalizar_columnas = FALSE)
#' }
sbc_query <- function(
  conn,
  sql,
  params = NULL,
  normalizar_columnas = TRUE,
  mostrar_info = TRUE
) {
  # Validaciones
  if (is.null(conn)) {
    stop("Conexión es NULL", call. = FALSE)
  }

  if (!DBI::dbIsValid(conn)) {
    stop("Conexión no válida o cerrada", call. = FALSE)
  }

  if (is.null(sql) || !is.character(sql) || trimws(sql) == "") {
    stop("SQL debe ser una cadena no vacía", call. = FALSE)
  }

  # Ejecutar consulta con manejo de errores
  tryCatch(
    {
      # Ejecutar query
      # Suprimir warnings de tipos MySQL no reconocidos (TIMESTAMP=tipo 7, JSON=tipo 245)
      # Estos campos se importan correctamente como character
      df <- suppressWarnings({
        if (!is.null(params)) {
          DBI::dbGetQuery(conn, sql, params = params)
        } else {
          DBI::dbGetQuery(conn, sql)
        }
      })

      # Convertir a tibble
      if (!requireNamespace("tibble", quietly = TRUE)) {
        warning("Paquete 'tibble' no está instalado. Devolviendo data.frame")
      } else {
        df <- tibble::as_tibble(df)
      }

      # Normalizar nombres de columnas si se solicita
      if (normalizar_columnas && ncol(df) > 0) {
        if (requireNamespace("dplyr", quietly = TRUE)) {
          df <- dplyr::rename_with(df, tolower)
        } else {
          names(df) <- tolower(names(df))
        }
      }

      # Mostrar información si se solicita
      if (mostrar_info) {
        n <- nrow(df)
        message(sprintf(
          "✓ %d registro%s obtenido%s",
          n,
          if (n != 1) "s" else "",
          if (n != 1) "s" else ""
        ))
      }

      df
    },
    error = function(e) {
      stop(
        "Error al ejecutar consulta SQL:\n",
        "  SQL: ",
        substr(sql, 1, 100),
        if (nchar(sql) > 100) "..." else "",
        "\n",
        "  Error: ",
        e$message,
        call. = FALSE
      )
    }
  )
}

#' Ejecutar función con gestión automática de conexión
#'
#' Abre una conexión a la base de datos, ejecuta la función proporcionada,
#' y cierra la conexión automáticamente incluso si ocurre un error.
#' Ideal para operaciones que requieren múltiples consultas.
#'
#' El tipo de base de datos se detecta automáticamente desde el objeto config
#' (campo \code{tipo}), el cual es obligatorio.
#'
#' @param config Configuración de conexión (lista con host, user, password, dbname, etc.)
#'   **Debe incluir campo \code{tipo}** con valor "mysql" o "db2"
#' @param query_fn Función que recibe la conexión como argumento y ejecuta consultas.
#'   Debe devolver el resultado deseado
#'
#' @return El valor devuelto por \code{query_fn}
#' @export
#'
#' @examples
#' \dontrun{
#' # Configuración con tipo incluido (RECOMENDADO)
#' config_mysql <- list(
#'   tipo = "mysql",
#'   host = "localhost",
#'   user = "root",
#'   password = "secret",
#'   dbname = "mydb"
#' )
#'
#' # Uso simple sin especificar tipo
#' datos <- sbc_with_connection(config_mysql, function(conn) {
#'   list(
#'     usuarios = sbc_query(conn, "SELECT * FROM usuarios"),
#'     productos = sbc_query(conn, "SELECT * FROM productos")
#'   )
#' })
#'
#' # Con DB2
#' config_db2 <- list(
#'   tipo = "db2",
#'   host = "servidor.com",
#'   user = "db2user",
#'   password = "pass",
#'   dbname = "estudios",
#'   driver_path = "/path/to/db2jcc4.jar"
#' )
#'
#' resultado <- sbc_with_connection(config_db2, function(conn) {
#'   sbc_query(conn, "SELECT COUNT(*) as total FROM tabla")
#' })
#' }
sbc_with_connection <- function(config, query_fn) {
  # Validar que query_fn sea una función
  if (!is.function(query_fn)) {
    stop(
      "query_fn debe ser una función que reciba la conexión como argumento",
      call. = FALSE
    )
  }

  # Determinar tipo de base de datos desde config$tipo
  if (is.null(config$tipo)) {
    stop(
      "No se pudo determinar el tipo de base de datos.\n\n",
      "La configuración debe incluir el campo 'tipo':\n",
      "   config <- list(tipo = 'mysql', host = ..., user = ..., ...)\n\n",
      "Tipos válidos: 'mysql' o 'db2'",
      call. = FALSE
    )
  }

  # Validar tipo
  tipo <- tolower(config$tipo)
  if (!tipo %in% c("mysql", "db2")) {
    stop(
      "Tipo de base de datos no válido: '",
      tipo,
      "'\n",
      "Tipos válidos: 'mysql' o 'db2'",
      call. = FALSE
    )
  }

  conn <- NULL

  tryCatch(
    {
      # Establecer conexión
      conn <- sbc_connect(config)

      # Ejecutar función del usuario
      resultado <- query_fn(conn)

      # Devolver resultado
      resultado
    },
    error = function(e) {
      stop(
        "Error durante ejecución con conexión automática:\n",
        "  Tipo BD: ",
        toupper(tipo),
        "\n",
        "  Host: ",
        config$host %||% "N/A",
        "\n",
        "  Base de datos: ",
        config$dbname %||% "N/A",
        "\n",
        "  Error: ",
        e$message,
        call. = FALSE
      )
    },
    finally = {
      # Siempre cerrar conexión si está abierta
      if (!is.null(conn) && DBI::dbIsValid(conn)) {
        sbc_disconnect(conn)
      }
    }
  )
}

#' Listar tablas disponibles en la base de datos
#'
#' Obtiene un vector con los nombres de todas las tablas disponibles
#' en la base de datos conectada.
#'
#' @param conn Conexión DBI válida y activa
#' @return Vector de caracteres con los nombres de las tablas
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- sbc_connect(config)
#'
#' # Listar todas las tablas
#' tablas <- sbc_list_tables(conn)
#' print(tablas)
#'
#' # Buscar tablas específicas
#' tablas_encuestas <- grep("^encuestas_", tablas, value = TRUE)
#' print(tablas_encuestas)
#'
#' sbc_disconnect(conn)
#' }
sbc_list_tables <- function(conn) {
  # Validaciones
  if (is.null(conn)) {
    stop("Conexión es NULL", call. = FALSE)
  }

  if (!DBI::dbIsValid(conn)) {
    stop("Conexión no válida o cerrada", call. = FALSE)
  }

  tryCatch(
    {
      # Obtener lista de tablas
      tablas <- DBI::dbListTables(conn)

      n <- length(tablas)

      # Mensaje informativo
      message(sprintf(
        "✓ %d tabla%s encontrada%s en la base de datos",
        n,
        if (n != 1) "s" else "",
        if (n != 1) "s" else ""
      ))

      # Ordenar alfabéticamente para facilitar búsqueda
      sort(tablas)
    },
    error = function(e) {
      stop(
        "Error al listar tablas:\n",
        "  Error: ",
        e$message,
        call. = FALSE
      )
    }
  )
}

#' Verificar si una tabla existe en la base de datos
#'
#' Comprueba si una tabla específica existe en la base de datos conectada.
#'
#' @param conn Conexión DBI válida
#' @param nombre_tabla Nombre de la tabla a verificar (sensible a mayúsculas/minúsculas)
#' @param ignorar_caso Ignorar diferencias entre mayúsculas y minúsculas (default: TRUE)
#'
#' @return \code{TRUE} si la tabla existe, \code{FALSE} en caso contrario
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- sbc_connect(config)
#'
#' # Verificar si existe una tabla
#' if (sbc_table_exists(conn, "usuarios")) {
#'   datos <- sbc_query(conn, "SELECT * FROM usuarios")
#' }
#'
#' sbc_disconnect(conn)
#' }
sbc_table_exists <- function(conn, nombre_tabla, ignorar_caso = TRUE) {
  # Validaciones
  if (is.null(conn)) {
    stop("Conexión es NULL", call. = FALSE)
  }

  if (!DBI::dbIsValid(conn)) {
    stop("Conexión no válida o cerrada", call. = FALSE)
  }

  if (is.null(nombre_tabla) || trimws(nombre_tabla) == "") {
    stop("nombre_tabla debe ser una cadena no vacía", call. = FALSE)
  }

  tryCatch(
    {
      # Obtener lista de tablas
      tablas <- DBI::dbListTables(conn)

      # Verificar existencia
      if (ignorar_caso) {
        tolower(nombre_tabla) %in% tolower(tablas)
      } else {
        nombre_tabla %in% tablas
      }
    },
    error = function(e) {
      warning(
        "Error al verificar existencia de tabla '",
        nombre_tabla,
        "':\n",
        "  Error: ",
        e$message
      )
      FALSE
    }
  )
}

#' Obtener información de columnas de una tabla
#'
#' Devuelve información sobre la estructura de una tabla, incluyendo
#' nombres de columnas y tipos de datos.
#'
#' @param conn Conexión DBI válida
#' @param nombre_tabla Nombre de la tabla a inspeccionar
#'
#' @return data.frame o tibble con información de las columnas
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- sbc_connect(config)
#'
#' # Ver estructura de una tabla
#' estructura <- sbc_table_info(conn, "usuarios")
#' print(estructura)
#'
#' sbc_disconnect(conn)
#' }
sbc_table_info <- function(conn, nombre_tabla) {
  # Validaciones
  if (is.null(conn)) {
    stop("Conexión es NULL", call. = FALSE)
  }

  if (!DBI::dbIsValid(conn)) {
    stop("Conexión no válida o cerrada", call. = FALSE)
  }

  if (is.null(nombre_tabla) || trimws(nombre_tabla) == "") {
    stop("nombre_tabla debe ser una cadena no vacía", call. = FALSE)
  }

  tryCatch(
    {
      # Obtener información de columnas
      info <- DBI::dbListFields(conn, nombre_tabla)

      # Crear data.frame con información básica
      df <- data.frame(
        columna = info,
        stringsAsFactors = FALSE
      )

      # Convertir a tibble si está disponible
      if (requireNamespace("tibble", quietly = TRUE)) {
        df <- tibble::as_tibble(df)
      }

      message(sprintf(
        "✓ Tabla '%s' tiene %d columna%s",
        nombre_tabla,
        nrow(df),
        if (nrow(df) != 1) "s" else ""
      ))

      df
    },
    error = function(e) {
      stop(
        "Error al obtener información de tabla '",
        nombre_tabla,
        "':\n",
        "  Error: ",
        e$message,
        call. = FALSE
      )
    }
  )
}
