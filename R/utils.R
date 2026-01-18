# Crea una funcion para cargar configuraciones utiles del usuario
library(tidyverse)

#' Obtener directorio de datos de proyectos
#'
#' Lee la variable de ambiente SBC_DATA_DIR y valida que existe
#'
#' @param subdir Subdirectorio opcional dentro del directorio de datos
#' @param create_if_missing Si TRUE, crea el directorio si no existe
#' @return Ruta absoluta al directorio de datos
#' @export
#' @examples
#' # Obtener directorio base
#' sbc_data_dir()
#'
#' # Obtener subdirectorio de un proyecto específico
#' sbc_data_dir("proyecto_2024")
#'
#' # Crear subdirectorio si no existe
#' sbc_data_dir("nuevo_proyecto", create_if_missing = TRUE)
sbc_data_dir <- function(subdir = NULL, create_if_missing = FALSE) {
  # Leer variable de ambiente
  data_dir <- Sys.getenv("SBC_DATA_DIR")

  # Validar que existe la variable
  if (data_dir == "") {
    stop(
      "Variable de ambiente SBC_DATA_DIR no configurada.\n\n",
      "Para configurarla:\n",
      "1. Ejecuta: usethis::edit_r_environ()\n",
      "2. Agrega: SBC_DATA_DIR=/ruta/a/tus/datos\n",
      "3. Reinicia la sesión de R\n\n",
      "Ejemplo: SBC_DATA_DIR=/Users/jramirez/Documents/EstudiosR/datos_proyectos",
      call. = FALSE
    )
  }

  # Construir ruta completa
  if (!is.null(subdir)) {
    full_path <- file.path(data_dir, subdir)
  } else {
    full_path <- data_dir
  }

  # Validar que el directorio existe
  if (!dir.exists(full_path)) {
    if (create_if_missing) {
      dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
      message("✓ Directorio creado: ", full_path)
    } else {
      stop(
        "El directorio no existe: ",
        full_path,
        "\n\n",
        "Opciones:\n",
        "1. Crear manualmente el directorio\n",
        "2. Usar create_if_missing = TRUE\n",
        "3. Verificar que SBC_DATA_DIR está configurado correctamente",
        call. = FALSE
      )
    }
  }

  return(normalizePath(full_path))
}

#' Obtener ruta a archivo de datos
#'
#' Helper para construir rutas a archivos dentro del directorio de datos
#'
#' @param ... Componentes de la ruta (carpetas y archivo)
#' @param must_exist Si TRUE, valida que el archivo existe
#' @return Ruta absoluta al archivo
#' @export
#' @examples
#' # Construir ruta a archivo CSV
#' sbc_data_path("proyecto_2024", "datos", "encuestas.csv")
#'
#' # Validar que el archivo existe
#' sbc_data_path("proyecto_2024", "datos", "encuestas.csv", must_exist = TRUE)
sbc_data_path <- function(..., must_exist = FALSE) {
  base_dir <- sbc_data_dir()
  file_path <- file.path(base_dir, ...)

  if (must_exist && !file.exists(file_path)) {
    stop(
      "El archivo no existe: ",
      file_path,
      call. = FALSE
    )
  }

  return(file_path)
}


#' Cargar configuración de base de datos MySQL
#'
#' Lee el archivo de configuración de base de datos MySQL guardado previamente
#'
#' @param config_name Nombre del archivo de configuración (sin extensión)
#' @return Lista con la configuración de la base de datos
#' @export
#' @examples
#' # Cargar configuración de MySQL
#' db_config <- sbc_get_mysql_config()
#'
#' # Cargar configuración con nombre personalizado
#' db_config <- sbc_get_mysql_config("DB_CONFIG_MYSQL")
sbc_get_mysql_config <- function(config_name = "DB_CONFIG_MYSQL") {
  config_file <- sbc_data_path("config", paste0(config_name, ".rds"))

  if (!file.exists(config_file)) {
    stop(
      "Archivo de configuración no encontrado: ",
      config_file,
      "\n\n",
      "Asegúrate de haber guardado la configuración previamente usando:\n",
      "write_rds(db_config, sbc_data_path('config', '",
      config_name,
      ".rds'))",
      call. = FALSE
    )
  }

  read_rds(config_file)
}


#' Cargar configuración de base de datos DB2
#'
#' Lee el archivo de configuración de base de datos DB2 guardado previamente
#'
#' @param config_name Nombre del archivo de configuración (sin extensión)
#' @return Lista con la configuración de la base de datos
#' @export
#' @examples
#' # Cargar configuración de DB2
#' db_config <- sbc_get_db2_config()
#'
#' # Cargar configuración con nombre personalizado
#' db_config <- sbc_get_db2_config("DB_CONFIG_DB2")
sbc_get_db2_config <- function(config_name = "DB_CONFIG_DB2") {
  config_file <- sbc_data_path("config", paste0(config_name, ".rds"))

  if (!file.exists(config_file)) {
    stop(
      "Archivo de configuración no encontrado: ",
      config_file,
      "\n\n",
      "Asegúrate de haber guardado la configuración previamente usando:\n",
      "write_rds(db_config, sbc_data_path('config', '",
      config_name,
      ".rds'))",
      call. = FALSE
    )
  }

  read_rds(config_file)
}




#' Cargar todas las tablas de un estudio desde MySQL
#'
#' Carga las 7 tablas estándar de un estudio desde la base de datos MySQL:
#' encuestas, propiedades, datos, estatus_supervision, logs_supervision,
#' logs y tiempos. Valida que el estudio existe antes de cargar y muestra
#' su nombre para confirmar. Mide y reporta el tiempo total de ejecución.
#'
#' @param num_estudio Número del estudio a cargar (debe ser positivo)
#' @return Lista nombrada con 7 tibbles, uno por cada tabla del estudio
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargar estudio 2 (Conveniancia 2025)
#' datos <- sbc_cargar_estudio_mysql(2)
#'
#' # Acceder a tablas individuales
#' encuestas <- datos$encuestas
#' propiedades <- datos$propiedades
#' datos_encuestas <- datos$datos
#'
#' # Ver estructura
#' names(datos)
#' lapply(datos, nrow)
#' }
sbc_cargar_estudio_mysql <- function(num_estudio) {
  # Validar entrada
  if (missing(num_estudio) || !is.numeric(num_estudio) || num_estudio <= 0) {
    stop("num_estudio debe ser un número positivo", call. = FALSE)
  }

  # Iniciar cronómetro
  tiempo_inicio <- Sys.time()

  # Verificar que el estudio existe y obtener su nombre
  nombre_estudio <- sbc_with_connection(sbc_get_mysql_config(), function(conn) {
    resultado <- sbc_query(
      conn,
      glue("SELECT estudio FROM estudios WHERE id_estudio = {num_estudio}"),
      mostrar_info = FALSE
    )

    if (nrow(resultado) == 0) {
      stop(
        "El estudio #", num_estudio, " no existe en la base de datos MySQL",
        call. = FALSE
      )
    }

    resultado$estudio[1]
  })

  message(sprintf(
    "→ Cargando estudio #%d: '%s' desde MySQL...",
    num_estudio,
    nombre_estudio
  ))

  # Cargar todas las tablas en una sola conexión
  datos_estudio <- sbc_with_connection(sbc_get_mysql_config(), function(conn) {
    list(
      encuestas = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_{num_estudio}")
      ),
      propiedades = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_propiedades_{num_estudio}")
      ),
      datos = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_datos_{num_estudio}")
      ),
      estatus_supervision = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_estatus_supervision_{num_estudio}")
      ),
      logs_supervision = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_estatus_supervision_log_{num_estudio}")
      ),
      logs = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_log_estatus_{num_estudio}")
      ),
      tiempos = sbc_query(
        conn,
        glue("SELECT * FROM encuestas_tiempos_{num_estudio}")
      )
    )
  })

  # Calcular tiempo transcurrido
  tiempo_total <- difftime(Sys.time(), tiempo_inicio, units = "secs")

  message(sprintf(
    "✓ Estudio #%d '%s' cargado exitosamente (7 tablas) en %.2f segundos",
    num_estudio,
    nombre_estudio,
    as.numeric(tiempo_total)
  ))

  datos_estudio
}



#' Cargar todas las tablas de un estudio desde DB2
#'
#' Carga las 5 tablas estándar de un estudio desde la base de datos DB2
#' en el esquema TABLET_SURVEYS: encuestas, propiedades, estatus,
#' estatus_log y tiempos. Valida que el estudio existe antes de cargar
#' y muestra su nombre para confirmar. Mide y reporta el tiempo total de ejecución.
#'
#' @param num_estudio Número del estudio a cargar (debe ser positivo)
#' @return Lista nombrada con 5 tibbles, uno por cada tabla del estudio
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargar estudio 118 (Farmacias 2025)
#' datos <- sbc_cargar_estudio_db2(118)
#'
#' # Acceder a tablas individuales
#' encuestas <- datos$encuestas
#' propiedades <- datos$propiedades
#' estatus <- datos$estatus
#'
#' # Ver estructura
#' names(datos)
#' lapply(datos, nrow)
#' }
sbc_cargar_estudio_db2 <- function(num_estudio) {
  # Validar entrada
  if (missing(num_estudio) || !is.numeric(num_estudio) || num_estudio <= 0) {
    stop("num_estudio debe ser un número positivo", call. = FALSE)
  }

  # Iniciar cronómetro
  tiempo_inicio <- Sys.time()

  # Verificar que el estudio existe y obtener su nombre
  nombre_estudio <- sbc_with_connection(sbc_get_db2_config(), function(conn) {
    resultado <- sbc_query(
      conn,
      glue("SELECT NOMBRE_ESTUDIO FROM TABLET_SURVEYS.ESTUDIOS WHERE ID_ESTUDIO = {num_estudio}"),
      mostrar_info = FALSE
    )

    if (nrow(resultado) == 0) {
      stop(
        "El estudio #", num_estudio, " no existe en la base de datos DB2",
        call. = FALSE
      )
    }

    resultado$nombre_estudio[1]
  })

  message(sprintf(
    "→ Cargando estudio #%d: '%s' desde DB2...",
    num_estudio,
    nombre_estudio
  ))

  # Cargar todas las tablas del esquema TABLET_SURVEYS en una sola conexión
  datos <- sbc_with_connection(
    sbc_get_db2_config(),
    function(conn) {
      list(
        encuestas = sbc_query(
          conn,
          glue("SELECT * FROM TABLET_SURVEYS.ENCUESTAS_{num_estudio}")
        ),
        propiedades = sbc_query(
          conn,
          glue("SELECT * FROM TABLET_SURVEYS.ENCUESTAS_PROPIEDADES_{num_estudio}")
        ),
        estatus = sbc_query(
          conn,
          glue("SELECT * FROM TABLET_SURVEYS.ENCUESTAS_ESTATUS_{num_estudio}")
        ),
        estatus_log = sbc_query(
          conn,
          glue("SELECT * FROM TABLET_SURVEYS.ENCUESTAS_LOG_ESTATUS_{num_estudio}")
        ),
        tiempos = sbc_query(
          conn,
          glue("SELECT * FROM TABLET_SURVEYS.ENCUESTAS_TIEMPOS_{num_estudio}")
        )
      )
    }
  )

  # Calcular tiempo transcurrido
  tiempo_total <- difftime(Sys.time(), tiempo_inicio, units = "secs")

  message(sprintf(
    "✓ Estudio #%d '%s' cargado exitosamente (5 tablas) en %.2f segundos",
    num_estudio,
    nombre_estudio,
    as.numeric(tiempo_total)
  ))

  datos
}


#' Filtrar datos de estudio por lista de IDs de encuestas válidas
#'
#' @description
#' Filtra todas las tablas de un estudio basándose en una lista de IDs de encuestas válidas.
#' Aplica el filtro automáticamente a todas las tablas que contengan la columna de ID especificada.
#'
#' @param datos_estudio Lista con tablas del estudio (resultado de sbc_cargar_estudio_mysql)
#' @param ids_validas Vector con los IDs de encuestas válidas a conservar
#' @param columna_id Nombre de la columna ID para aplicar filtro (default: "id_encuesta")
#' @param verbose Mostrar información del proceso (default: TRUE)
#'
#' @return Lista filtrada con las mismas tablas del estudio
#'
#' @examples
#' \dontrun{
#' # Cargar datos
#' datos <- sbc_cargar_estudio_mysql(num_estudio = 2)
#'
#' # Obtener IDs válidas (ejemplo: solo encuestas aceptadas)
#' ids_aceptadas <- datos$encuestas |>
#'   filter(id_estatus == 1) |>
#'   pull(id_encuesta)
#'
#' # Filtrar todas las tablas
#' datos_filtrados <- sbc_filtrar_por_ids(datos, ids_aceptadas)
#'
#' # También puedes pasar IDs específicas
#' datos_filtrados <- sbc_filtrar_por_ids(datos, c(1001, 1005, 1010))
#' }
#'
#' @export
sbc_filtrar_por_ids <- function(datos_estudio,
                                ids_validas,
                                columna_id = "id_encuesta",
                                verbose = TRUE) {

  # Validaciones
  if (!is.list(datos_estudio)) {
    stop("datos_estudio debe ser una lista de data.frames")
  }

  if (missing(ids_validas) || length(ids_validas) == 0) {
    stop("Debe proporcionar al menos un ID válido en ids_validas")
  }

  if (verbose) {
    cat(glue::glue("🔍 sbc_filtrar_por_ids: Filtrando por {length(ids_validas)} IDs válidas"), "\n")
  }

  # Aplicar filtro a todas las tablas que tengan la columna ID
  datos_filtrados <- purrr::map(datos_estudio, function(tabla) {
    if (is.data.frame(tabla) && columna_id %in% colnames(tabla)) {
      filas_antes <- nrow(tabla)
      tabla_filtrada <- tabla |> dplyr::filter(.data[[columna_id]] %in% ids_validas)
      filas_despues <- nrow(tabla_filtrada)

      if (verbose) {
        nombre_tabla <- names(datos_estudio)[which(sapply(datos_estudio, identical, tabla))[1]]
        cat(glue::glue("  ✅ {nombre_tabla}: {filas_antes} → {filas_despues} filas"), "\n")
      }

      return(tabla_filtrada)
    } else {
      return(tabla)
    }
  })

  if (verbose) {
    cat("\n✨ Filtrado completado\n\n")
  }

  return(datos_filtrados)
}


#' Limpiar formato Markdown de textos de encuestas
#'
#' @description
#' Convierte texto con formato Markdown a texto plano, manejando de forma 
#' inteligente elementos comunes como negritas, cursivas y listas. La función 
#' está diseñada para procesar respuestas de encuestas que contienen formato 
#' Markdown del software de captura.
#'
#' @details
#' La función aplica las siguientes transformaciones:
#' 
#' ## Formato de texto:
#' - **Bold**: `**texto**` → `texto`
#' - *Italic*: `*texto*` → `texto` (solo si está balanceado)
#' 
#' ## Listas:
#' - Convierte listas con título en formato `título: item1, item2, item3`
#' - Ejemplo: `**Título**\n- Item1\n- Item2` → `Título: Item1, Item2`
#' 
#' ## Preservación inteligente:
#' - Guiones en palabras como "Coca-Cola" se mantienen
#' - Asteriscos sueltos como "Marca *" se mantienen
#' - Solo elimina formato Markdown válido
#'
#' @param texto Vector de caracteres con texto en formato Markdown
#' @param preservar_saltos Lógico. Si TRUE, mantiene algunos saltos de línea 
#'   en lugar de convertirlos todos a espacios. Por defecto FALSE.
#'
#' @return Vector de caracteres con texto limpio en formato plano
#'
#' @examples
#' # Texto con formato bold
#' sbc_limpiar_markdown("**Producto Principal**")
#' #> "Producto Principal"
#' 
#' # Lista con título
#' texto_lista <- "**Categorías:**\n- Bebidas\n- Snacks\n- Lácteos"
#' sbc_limpiar_markdown(texto_lista)
#' #> "Categorías: Bebidas, Snacks, Lácteos"
#' 
#' # Texto con guiones preservados
#' sbc_limpiar_markdown("Coca-Cola es *refrescante*")
#' #> "Coca-Cola es refrescante"
#' 
#' # Aplicar a columna de data.frame
#' \dontrun{
#' datos <- datos |>
#'   mutate(respuesta_limpia = sbc_limpiar_markdown(respuesta_original))
#' }
#'
#' @family text_processing
#' @export
sbc_limpiar_markdown <- function(texto, preservar_saltos = FALSE) {
    
    if (length(texto) == 0) return(texto)
    
    # Vectorizar la función para manejar vectores
    resultado <- character(length(texto))
    
    for (i in seq_along(texto)) {
        if (is.na(texto[i]) || texto[i] == "") {
            resultado[i] <- texto[i]
            next
        }
        
        texto_actual <- texto[i]
        
        # 1. Eliminar ** bold ** (asteriscos dobles con contenido)
        texto_actual <- stringr::str_replace_all(texto_actual, "\\*\\*([^*]+)\\*\\*", "\\1")
        
        # 2. Eliminar * italic * (asteriscos simples con contenido, pero solo si están balanceados)
        texto_actual <- stringr::str_replace_all(texto_actual, "(?<!\\*)\\*([^*\n]+)\\*(?!\\*)", "\\1")
        
        # 3. Manejar listas: convertir título + lista en formato "título: item1, item2, item3"
        if (stringr::str_detect(texto_actual, "\\n\\s*-\\s+")) {
            # Separar texto antes de la lista y elementos de la lista
            partes <- stringr::str_split(texto_actual, "\\n(?=\\s*-\\s+)", n = 2)[[1]]
            
            if (length(partes) == 2) {
                titulo <- stringr::str_trim(partes[1])
                lista_texto <- partes[2]
                
                # Extraer elementos de la lista
                elementos <- stringr::str_extract_all(lista_texto, "(?<=^|\\n)\\s*-\\s+(.+?)(?=\\n|$)")[[1]]
                elementos <- stringr::str_replace_all(elementos, "^\\s*-\\s+", "")
                elementos <- stringr::str_trim(elementos)
                elementos <- elementos[elementos != ""]
                
                # Combinar título con lista usando ":" y ","
                if (length(elementos) > 0) {
                    texto_actual <- paste0(titulo, ": ", paste(elementos, collapse = ", "))
                } else {
                    texto_actual <- titulo
                }
            }
        } else {
            # 4. Para texto sin listas, eliminar guiones de lista simples
            texto_actual <- stringr::str_replace_all(texto_actual, "(^|\\n)\\s*-\\s+", "\\1")
        }
        
        # 5. Manejar saltos de línea
        if (preservar_saltos) {
            # Convertir múltiples saltos de línea en uno solo
            texto_actual <- stringr::str_replace_all(texto_actual, "\\n{2,}", "\\n")
        } else {
            # Eliminar todos los saltos de línea y reemplazar por espacio
            texto_actual <- stringr::str_replace_all(texto_actual, "\\n+", " ")
        }
        
        # 6. Eliminar espacios extra y trim
        resultado[i] <- stringr::str_squish(texto_actual)
    }
    
    return(resultado)
}


#' Limpiar formato Markdown de texto
#'
#' @description
#' Convierte texto con formato Markdown a texto plano, manejando inteligentemente
#' diferentes elementos de formato como negrita, cursiva, listas y saltos de línea.
#' Especialmente útil para limpiar respuestas de encuestas que vienen formateadas
#' en Markdown.
#'
#' @details
#' La función procesa los siguientes elementos de Markdown:
#'
#' ## Formato de texto:
#' - **Negrita**: `**texto**` se convierte en `texto`
#' - *Cursiva*: `*texto*` se convierte en `texto` (solo si está balanceado)
#'
#' ## Listas:
#' Convierte listas con formato:
#' ```
#' **Título**
#' - Elemento 1
#' - Elemento 2
#' ```
#' En: `Título: Elemento 1, Elemento 2`
#'
#' ## Preservación inteligente:
#' - Mantiene guiones en palabras: `Coca-Cola` permanece intacto
#' - Mantiene asteriscos sueltos: `Marca *` permanece intacto
#' - Solo procesa listas que siguen el patrón estándar de Markdown
#'
#' @param texto Vector de caracteres con texto en formato Markdown
#'
#' @return Vector de caracteres con texto limpio en formato plano
#'
#' @examples
#' # Texto con negrita
#' sbc_limpiar_markdown("**Texto importante**")
#' # Resultado: "Texto importante"
#'
#' # Lista con título
#' texto_lista <- "**Opciones disponibles**\n- Opción A\n- Opción B\n- Opción C"
#' sbc_limpiar_markdown(texto_lista)
#' # Resultado: "Opciones disponibles: Opción A, Opción B, Opción C"
#'
#' # Texto mixto
#' texto_mixto <- "**Marcas**: *Coca-Cola*, Pepsi-Cola"
#' sbc_limpiar_markdown(texto_mixto)
#' # Resultado: "Marcas: Coca-Cola, Pepsi-Cola"
#'
#' # Vectorizado
#' textos <- c("**Bold**", "*Italic*", "Normal text")
#' sbc_limpiar_markdown(textos)
#'
#' @family text_processing
#' @export
sbc_limpiar_markdown <- function(texto) {
    
    # Validación de input
    if (!is.character(texto)) {
        stop("El input debe ser un vector de caracteres")
    }
    
    # Función interna para procesar un solo texto
    limpiar_texto_individual <- function(txt) {
        if (is.na(txt) || txt == "") return(txt)
        
        # 1. Eliminar ** bold ** (asteriscos dobles con contenido)
        txt <- stringr::str_replace_all(txt, "\\*\\*([^*]+)\\*\\*", "\\1")
        
        # 2. Eliminar * italic * (asteriscos simples con contenido, pero solo si están balanceados)
        txt <- stringr::str_replace_all(txt, "(?<!\\*)\\*([^*\n]+)\\*(?!\\*)", "\\1")
        
        # 3. Manejar listas: convertir título + lista en formato "título: item1, item2, item3"
        if (stringr::str_detect(txt, "\\n\\s*-\\s+")) {
            # Separar texto antes de la lista y elementos de la lista
            partes <- stringr::str_split(txt, "\\n(?=\\s*-\\s+)", n = 2)[[1]]
            
            if (length(partes) == 2) {
                titulo <- stringr::str_trim(partes[1])
                lista_texto <- partes[2]
                
                # Extraer elementos de la lista (texto después de cada guión)
                elementos <- stringr::str_extract_all(lista_texto, "(?<=^|\\n)\\s*-\\s+(.+?)(?=\\n|$)")[[1]]
                elementos <- stringr::str_replace_all(elementos, "^\\s*-\\s+", "")
                elementos <- stringr::str_trim(elementos)
                elementos <- elementos[elementos != ""]
                
                # Combinar título con lista usando ":" y ","
                if (length(elementos) > 0) {
                    txt <- paste0(titulo, ": ", paste(elementos, collapse = ", "))
                } else {
                    txt <- titulo
                }
            }
        } else {
            # 4. Para texto sin listas, eliminar guiones de lista simples
            txt <- stringr::str_replace_all(txt, "(^|\\n)\\s*-\\s+", "\\1")
        }
        
        # 5. Eliminar múltiples saltos de línea y reemplazar por espacio
        txt <- stringr::str_replace_all(txt, "\\n+", " ")
        
        # 6. Eliminar espacios extra y trim
        txt <- stringr::str_squish(txt)
        
        return(txt)
    }
    
    # Aplicar la función a todo el vector
    purrr::map_chr(texto, limpiar_texto_individual)
}


#' Limpiar formato Markdown de texto de encuestas
#'
#' @description
#' Convierte texto con formato Markdown a texto plano de forma inteligente,
#' preservando contenido relevante y transformando listas en formato legible.
#'
#' @details
#' La función maneja los siguientes elementos de Markdown:
#'
#' ## Elementos que elimina:
#' - **Texto en negrita**: `**texto**` → `texto`
#' - *Texto en cursiva*: `*texto*` → `texto` (solo si está balanceado)
#' - Guiones de lista al inicio de línea
#' - Saltos de línea múltiples
#'
#' ## Elementos que preserva:
#' - Guiones dentro de palabras: `Coca-Cola` (sin cambios)
#' - Asteriscos sueltos: `Precio: $50*` (sin cambios)
#' - Asteriscos no balanceados: `3*4=12` (sin cambios)
#'
#' ## Transformación de listas:
#' Las listas se convierten al formato "título: item1, item2, item3"
#'
#' @param texto Vector de caracteres con formato Markdown
#' @return Vector de caracteres con texto plano limpio
#'
#' @examples
#' # Texto con formato bold
#' sbc_limpiar_markdown("**Importante:** Este es el texto")
#' # → "Importante: Este es el texto"
#'
#' # Lista con título
#' texto_lista <- "**Opciones disponibles:**\n- Opción A\n- Opción B\n- Opción C"
#' sbc_limpiar_markdown(texto_lista)
#' # → "Opciones disponibles: Opción A, Opción B, Opción C"
#'
#' # Preserva guiones en palabras
#' sbc_limpiar_markdown("Coca-Cola es una marca")
#' # → "Coca-Cola es una marca"
#'
#' # Múltiples textos
#' textos <- c("**Bold**", "*Italic*", "Normal text")
#' sbc_limpiar_markdown(textos)
#' # → c("Bold", "Italic", "Normal text")
#'
#' @family text_processing
#' @export
sbc_limpiar_markdown <- function(texto) {
    
    # Función interna para limpiar un texto individual
    limpiar_texto_individual <- function(txt) {
        if (is.na(txt) || txt == "") return(txt)
        
        # 1. Eliminar ** bold ** (asteriscos dobles con contenido)
        txt <- stringr::str_replace_all(txt, "\\*\\*([^*]+)\\*\\*", "\\1")
        
        # 2. Eliminar * italic * (asteriscos simples balanceados)
        txt <- stringr::str_replace_all(txt, "(?<!\\*)\\*([^*\n]+)\\*(?!\\*)", "\\1")
        
        # 3. Manejar listas: convertir título + lista en formato "título: item1, item2, item3"
        if (stringr::str_detect(txt, "\\n\\s*-\\s+")) {
            # Separar texto antes de la lista y elementos de la lista
            partes <- stringr::str_split(txt, "\\n(?=\\s*-\\s+)", n = 2)[[1]]
            
            if (length(partes) == 2) {
                titulo <- stringr::str_trim(partes[1])
                lista_texto <- partes[2]
                
                # Extraer elementos de la lista
                elementos <- stringr::str_extract_all(lista_texto, "(?<=^|\\n)\\s*-\\s+(.+?)(?=\\n|$)")[[1]]
                elementos <- stringr::str_replace_all(elementos, "^\\s*-\\s+", "")
                elementos <- stringr::str_trim(elementos)
                elementos <- elementos[elementos != ""]
                
                # Combinar título con lista usando ":" y ","
                if (length(elementos) > 0) {
                    txt <- paste0(titulo, ": ", paste(elementos, collapse = ", "))
                } else {
                    txt <- titulo
                }
            }
        } else {
            # 4. Para texto sin listas, eliminar guiones de lista simples
            txt <- stringr::str_replace_all(txt, "(^|\\n)\\s*-\\s+", "\\1")
        }
        
        # 5. Eliminar múltiples saltos de línea y reemplazar por espacio
        txt <- stringr::str_replace_all(txt, "\\n+", " ")
        
        # 6. Eliminar espacios extra y trim
        txt <- stringr::str_squish(txt)
        
        return(txt)
    }
    
    # Aplicar la función a todo el vector
    purrr::map_chr(texto, limpiar_texto_individual)
}
