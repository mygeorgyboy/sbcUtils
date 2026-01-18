#' Transformar datos de encuestas a múltiples tablas relacionales
#'
#' @description
#' Esta función transforma datos de encuestas desde un formato largo (key-value pairs)
#' a múltiples tablas relacionales basándose en un "libro de datos" (data_book)
#' que define la estructura destino.
#'
#' @details
#' La función maneja diferentes tipos de propiedades de encuestas:
#'
#' ## Tipos de Propiedades
#'
#' ### 1. Propiedades Simples (`inLoop = "NO"`)
#' - Una columna por propiedad
#' - Ejemplo: `sexo`, `edad`
#'
#' ### 2. Propiedades Múltiples (`questionClass = "MULTIPLE_*"` + `inLoop = "NO"`)
#' - Múltiples columnas con sufijos: `marca_preferida_1`, `marca_preferida_2`
#' - Para preguntas de selección múltiple
#'
#' ### 3. Propiedades Loop While (`inLoop = "YES"` + `questionClass = "MULTIPLE_*"`)
#' - Patrón: `property_loopIdx_multipleIdx`
#' - Ejemplo: `respuesta_1_1`, `respuesta_1_2`, `respuesta_2_1`
#' - Se expanden a columnas múltiples por cada iteración
#'
#' ### 4. Propiedades Loop ForEach (`inLoop = "YES"` + `forEachValue = "YES"`)
#' - Patrón simple: `property_loopIdx`
#' - Ejemplo: `bebidas_consumo_ultimo_mes_desglosado_1`, `frecuencia_relativa_bebida_1`
#' - Una columna por propiedad, agrupadas por `idx`
#'
#' ## Estructura del data_book_completo
#'
#' El archivo `data_book_completo.xlsx` debe contener las siguientes columnas:
#'
#' - `action`: Define la acción para cada propiedad. Valores posibles:
#'   - `NA` o vacío: Procesar normalmente
#'   - `"ignorar"`: Excluir de la transformación
#' - `table`: Nombre de la tabla destino (ej: "encuestas", "productos")
#' - `property`: Nombre de la propiedad base
#' - `questionClass`: Tipo de pregunta (SINGLE_FIXED_CHOICE, MULTIPLE_*, etc.)
#' - `forEachValue`: "YES" para forzar tratamiento como loop simple
#' - `maxOccurrences`: Máximo número de ocurrencias para preguntas múltiples
#' - `inLoop`: "YES" si la propiedad está en un loop, "NO" si no
#' - `loopMax`: Máximo número de iteraciones del loop
#' - `question`: Texto de la pregunta (opcional, para documentación)
#'
#' @param data_book tibble con definición de propiedades y estructura destino.
#'   Debe contener las columnas: action, table, property, questionClass,
#'   maxOccurrences, inLoop, loopMax, question. Opcionalmente: forEachValue.
#'
#' @param datos_propiedades tibble con datos en formato largo (key-value).
#'   Debe contener las columnas: id_estudio, id_encuesta, propiedad, valor.
#'
#' @param id_encuesta_filter vector opcional de IDs de encuestas a procesar.
#'   Si es NULL, se procesan todas las encuestas.
#'
#' @return Lista nombrada donde cada elemento es un tibble representando
#'   una tabla relacional. Los nombres de la lista corresponden a los
#'   valores únicos de la columna `table` en data_book.
#'
#' @examples
#' \dontrun{
#' # Cargar data_book desde Excel
#' data_book <- openxlsx::read.xlsx("datos/data_book_completo.xlsx") |>
#'   tibble::tibble()
#'
#' # Ejecutar transformación
#' tablas <- sbc_transformar_datos_a_tablas(data_book, estudio$propiedades)
#'
#' # Acceder a tablas específicas
#' encuestas <- tablas$encuestas
#' productos <- tablas$productos
#' consumo_bebidas <- tablas$consumo_bebidas
#' }
#'
#' @family data_transformation
#'
#' @seealso
#' \url{https://github.com/tu_usuario/sbcUtils} para más información sobre
#' el paquete y ejemplos adicionales.
#'
#' @export
sbc_transformar_datos_a_tablas <- function(data_book, datos_propiedades, id_encuesta_filter = NULL) {

    # Validación de inputs ------------------------------------------------
    if (!is.data.frame(data_book) || !is.data.frame(datos_propiedades)) {
        stop("data_book y datos_propiedades deben ser data.frames o tibbles")
    }

    # Columnas requeridas en data_book
    required_cols_book <- c("action", "table", "property", "questionClass",
                           "maxOccurrences", "inLoop", "loopMax", "question")
    missing_cols_book <- setdiff(required_cols_book, names(data_book))
    if (length(missing_cols_book) > 0) {
        stop("data_book debe contener las columnas: ", paste(missing_cols_book, collapse = ", "))
    }

    # Agregar manejo opcional de linkedProperty y linkType
    has_linked_props <- "linkedProperty" %in% names(data_book) && "linkType" %in% names(data_book)

    # Columnas requeridas en datos_propiedades
    required_cols_datos <- c("id_estudio", "id_encuesta", "propiedad", "valor")
    missing_cols_datos <- setdiff(required_cols_datos, names(datos_propiedades))
    if (length(missing_cols_datos) > 0) {
        stop("datos_propiedades debe contener las columnas: ", paste(missing_cols_datos, collapse = ", "))
    }

    message("\U1F680 Iniciando transformación de datos...")

    # Filtrar encuestas si se especifica
    if (!is.null(id_encuesta_filter)) {
        datos_propiedades <- datos_propiedades |>
            dplyr::filter(id_encuesta %in% id_encuesta_filter)
        message("\U1F4CB Filtrando ", length(id_encuesta_filter), " encuestas específicas")
    }

    message("\U1F4CA Procesando ", nrow(datos_propiedades), " registros de propiedades")

    # 1. FILTRAR PROPIEDADES A IGNORAR ---------------------------------
    message("\U1F6AB Identificando propiedades a ignorar...")

    # Propiedades con action == "ignorar"
    props_ignorar <- data_book |>
        dplyr::filter(!is.na(action) & action == "ignorar") |>
        dplyr::pull(property)

    if (length(props_ignorar) > 0) {
        message("   \U2192 Ignorando ", length(props_ignorar), " propiedades marcadas como 'ignorar'")
    }

    # Propiedades con table == NA
    props_sin_tabla <- data_book |>
        dplyr::filter(is.na(table) & (is.na(action) | action != "ignorar")) |>
        dplyr::pull(property)

    if (length(props_sin_tabla) > 0) {
        warning("\U26A0\UFE0F  Encontradas ", length(props_sin_tabla), " propiedades sin tabla asignada: ",
                paste(head(props_sin_tabla, 5), collapse = ", "),
                if(length(props_sin_tabla) > 5) "..." else "")
    }

    # Data book filtrado (solo propiedades válidas)
    data_book_valido <- data_book |>
        dplyr::filter(
            (is.na(action) | action != "ignorar") &
            !is.na(table)
        )

    message("\U2705 ", nrow(data_book_valido), " propiedades válidas para procesar")

    # 2. IDENTIFICAR PROPIEDADES EN DATOS VS DATA_BOOK -----------------
    message("\U1F50D Verificando correspondencia entre datos y data_book...")

    # Extraer propiedades base de los datos (eliminar sufijos _1, _2, etc.)
    props_en_datos <- unique(datos_propiedades$propiedad)
    props_base_datos <- stringr::str_remove(props_en_datos, "_\\d+$")

    # Propiedades en data_book
    props_en_book <- data_book_valido$property

    # Advertencias por propiedades faltantes
    props_datos_no_book <- setdiff(unique(props_base_datos), props_en_book)
    if (length(props_datos_no_book) > 0) {
        warning("\U26A0\UFE0F  Propiedades en datos pero no en data_book: ",
                paste(head(props_datos_no_book, 10), collapse = ", "),
                if(length(props_datos_no_book) > 10) "..." else "")
    }

    props_book_no_datos <- setdiff(props_en_book, unique(props_base_datos))
    if (length(props_book_no_datos) > 0) {
        warning("\U26A0\UFE0F  Propiedades en data_book pero no en datos: ",
                paste(head(props_book_no_datos, 10), collapse = ", "),
                if(length(props_book_no_datos) > 10) "..." else "")
    }

    # 3. FUNCIONES AUXILIARES ------------------------------------------

    # Determinar máximo sufijo para una propiedad base
    # @param property_base Nombre base de la propiedad
    # @param propiedades_disponibles Vector de todas las propiedades disponibles
    # @return Número máximo de sufijo encontrado
    obtener_max_sufijo <- function(property_base, propiedades_disponibles) {
        pattern <- paste0("^", escapeRegex(property_base), "_(\\d+)$")
        matches <- stringr::str_match(propiedades_disponibles, pattern)
        sufijos <- as.numeric(matches[!is.na(matches[,2]), 2])
        if (length(sufijos) > 0) max(sufijos) else 1
    }

    # Escapar caracteres especiales en regex
    # @param string Cadena a escapar
    # @return Cadena con caracteres especiales escapados
    escapeRegex <- function(string) {
        gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
    }

    # 4. PROCESAR CADA TABLA -------------------------------------------
    message("\U1F504 Transformando datos a tablas relacionales...")

    tablas_resultado <- list()
    tablas_unicas <- unique(data_book_valido$table)

    for (tabla_nombre in tablas_unicas) {
        message("\U1F4CB Procesando tabla: ", tabla_nombre)

        # Propiedades de esta tabla
        props_tabla <- data_book_valido |>
            dplyr::filter(table == tabla_nombre)

        # Separar propiedades en diferentes categorías basándose únicamente en inLoop
        props_loop <- props_tabla |> dplyr::filter(inLoop == "YES")
        props_simples <- props_tabla |> dplyr::filter(inLoop != "YES")

        # INICIALIZAR TABLA
        if (nrow(props_loop) > 0) {
            # Tabla con idx para loops
            tabla_actual <- tibble::tibble(
                id_encuesta = integer(),
                idx = integer()
            )
        } else {
            # Tabla simple
            tabla_actual <- tibble::tibble(
                id_encuesta = unique(datos_propiedades$id_encuesta)
            )
        }

        # PROCESAR PROPIEDADES SIMPLES
        if (nrow(props_simples) > 0) {
            message("   \U2192 ", nrow(props_simples), " propiedades simples")

            for (i in 1:nrow(props_simples)) {
                prop_info <- props_simples[i, ]
                prop_base <- prop_info$property

                # Determinar si es MULTIPLE/RANKING (manejar NAs)
                es_multiple <- !is.na(prop_info$questionClass) &&
                               stringr::str_detect(prop_info$questionClass, "MULTIPLE|RANKING")

                if (es_multiple) {
                    # Buscar máximo sufijo en datos
                    max_sufijo <- obtener_max_sufijo(prop_base, props_en_datos)

                    # Crear columnas con sufijo
                    for (sufijo in 1:max_sufijo) {
                        col_name <- paste0(prop_base, "_", sufijo)
                        prop_completa <- col_name

                        # Obtener valores
                        valores <- datos_propiedades |>
                            dplyr::filter(propiedad == prop_completa) |>
                            dplyr::select(id_encuesta, valor)

                        # Agregar columna a tabla
                        if (nrow(valores) > 0) {
                            tabla_actual <- tabla_actual |>
                                dplyr::left_join(valores |> dplyr::rename(!!col_name := valor), by = "id_encuesta")
                        } else {
                            tabla_actual[[col_name]] <- NA_character_
                        }
                    }
                } else {
                    # Propiedad simple sin sufijo
                    valores <- datos_propiedades |>
                        dplyr::filter(propiedad == prop_base) |>
                        dplyr::select(id_encuesta, valor)

                    if (nrow(valores) > 0) {
                        tabla_actual <- tabla_actual |>
                            dplyr::left_join(valores |> dplyr::rename(!!prop_base := valor), by = "id_encuesta")
                    } else {
                        tabla_actual[[prop_base]] <- NA_character_
                    }
                }
            }
        }

        # PROCESAR PROPIEDADES EN LOOP
        if (nrow(props_loop) > 0) {
            message("   \U2192 ", nrow(props_loop), " propiedades en loop")

            # Recopilar todas las propiedades loop en una estructura unificada
            todas_las_props_loop <- list()

            for (i in 1:nrow(props_loop)) {
                prop_info <- props_loop[i, ]
                prop_base <- prop_info$property

                # Verificar si debe ser forzada como loop simple (es un forEach value)
                force_simple <- "forEachValue" %in% names(data_book) &&
                               !is.na(prop_info$forEachValue) &&
                               prop_info$forEachValue == "YES"

                # Verificar si la propiedad en loop también es múltiple
                es_loop_multiple <- !force_simple &&
                                   !is.na(prop_info$questionClass) &&
                                   stringr::str_detect(prop_info$questionClass, "MULTIPLE|RANKING")

                if (es_loop_multiple) {
                    # Patrón para propiedades loop + múltiples: property_loopIdx_multipleIdx
                    pattern <- paste0("^", escapeRegex(prop_base), "_(\\d+)_(\\d+)$")
                    registros_prop <- datos_propiedades |>
                        dplyr::filter(stringr::str_detect(propiedad, pattern)) |>
                        dplyr::mutate(
                            # Extraer el índice del loop (primer número)
                            idx = as.numeric(stringr::str_extract(propiedad, "(?<=_)\\d+(?=_\\d+$)")),
                            # Extraer el índice múltiple (segundo número)
                            multiple_idx = as.numeric(stringr::str_extract(propiedad, "\\d+$")),
                            # Crear nombre de columna con sufijo múltiple
                            col_name = paste0(prop_base, "_", multiple_idx)
                        ) |>
                        dplyr::select(id_encuesta, idx, col_name, valor)

                    # Pivotar para crear columnas separadas por cada índice múltiple
                    if (nrow(registros_prop) > 0) {
                        registros_pivot <- registros_prop |>
                            tidyr::pivot_wider(names_from = col_name, values_from = valor, values_fill = NA_character_)

                        todas_las_props_loop[[prop_base]] <- registros_pivot
                    }

                } else {
                    # Propiedades loop simples: property_loopIdx
                    pattern <- paste0("^", escapeRegex(prop_base), "_(\\d+)$")
                    registros_prop <- datos_propiedades |>
                        dplyr::filter(stringr::str_detect(propiedad, pattern)) |>
                        dplyr::mutate(
                            # Extraer el sufijo numérico
                            idx = as.numeric(stringr::str_extract(propiedad, "\\d+$"))
                        ) |>
                        dplyr::select(id_encuesta, idx, valor) |>
                        dplyr::rename(!!prop_base := valor)

                    if (nrow(registros_prop) > 0) {
                        todas_las_props_loop[[prop_base]] <- registros_prop
                    }
                }
            }

            # Combinar todas las propiedades loop usando full_join en lugar de bind_rows
            if (length(todas_las_props_loop) > 0) {
                tabla_actual <- todas_las_props_loop[[1]]

                if (length(todas_las_props_loop) > 1) {
                    for (j in 2:length(todas_las_props_loop)) {
                        tabla_actual <- tabla_actual |>
                            dplyr::full_join(todas_las_props_loop[[j]], by = c("id_encuesta", "idx"))
                    }
                }

                # Limpiar filas donde todas las variables (excepto id_encuesta e idx) son NA
                cols_to_check <- setdiff(names(tabla_actual), c("id_encuesta", "idx"))
                if (length(cols_to_check) > 0) {
                    tabla_actual <- tabla_actual |>
                        dplyr::filter(dplyr::if_any(dplyr::all_of(cols_to_check), ~ !is.na(.)))
                }
            }
        }

        # Guardar tabla resultado
        tablas_resultado[[tabla_nombre]] <- tabla_actual
        message("   \U2705 Tabla '", tabla_nombre, "' creada con ", nrow(tabla_actual), " filas")
    }

    message("\U1F389 Transformación completada!")
    message("\U1F4CB Se generaron ", length(tablas_resultado), " tablas: ",
            paste(names(tablas_resultado), collapse = ", "))

    return(tablas_resultado)
}


#' Crear plantilla de data_book para configuración de transformación
#'
#' @description
#' Crea una plantilla base del archivo data_book con las columnas necesarias
#' para configurar la transformación de datos de encuestas.
#'
#' @param propiedades Vector de nombres de propiedades para incluir en la plantilla
#' @param archivo_salida Ruta donde guardar el archivo Excel (opcional)
#'
#' @return tibble con la estructura base del data_book
#'
#' @examples
#' \dontrun{
#' # Crear plantilla básica
#' plantilla <- sbc_crear_plantilla_data_book(c("sexo", "edad", "preferencia"))
#'
#' # Guardar en Excel
#' sbc_crear_plantilla_data_book(
#'   propiedades = c("sexo", "edad"),
#'   archivo_salida = "mi_data_book.xlsx"
#' )
#' }
#'
#' @family data_transformation
#' @export
sbc_crear_plantilla_data_book <- function(propiedades, archivo_salida = NULL) {

    if (length(propiedades) == 0) {
        stop("Debe proporcionar al menos una propiedad")
    }

    plantilla <- tibble::tibble(
        action = NA_character_,
        table = NA_character_,
        property = propiedades,
        questionClass = NA_character_,
        forEachValue = NA_character_,
        maxOccurrences = NA_real_,
        inLoop = "NO",
        loopMax = NA_real_,
        question = NA_character_
    )

    if (!is.null(archivo_salida)) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("El paquete 'openxlsx' es necesario para guardar archivos Excel")
        }
        openxlsx::write.xlsx(plantilla, archivo_salida)
        message("Plantilla guardada en: ", archivo_salida)
    }

    return(plantilla)
}