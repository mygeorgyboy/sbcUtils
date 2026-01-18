#' Ejemplo de estructura data_book completo
#'
#' Un dataset de ejemplo que muestra la estructura requerida para el archivo
#' data_book_completo.xlsx usado por la función \code{\link{sbc_transformar_datos_a_tablas}}.
#'
#' @format Un data frame con 9 variables:
#' \describe{
#'   \item{action}{Acción a realizar con la propiedad. Valores posibles:
#'     \itemize{
#'       \item \code{NA} o vacío: Procesar normalmente
#'       \item \code{"ignorar"}: Excluir de la transformación
#'     }
#'   }
#'   \item{table}{Nombre de la tabla destino donde se almacenará la propiedad.
#'     Ejemplos: "encuestas", "productos", "promociones", "consumo_bebidas"}
#'   \item{property}{Nombre base de la propiedad tal como aparece en el cuestionario}
#'   \item{questionClass}{Tipo de pregunta que determina cómo se procesa:
#'     \itemize{
#'       \item \code{SINGLE_FIXED_CHOICE}: Pregunta de opción única
#'       \item \code{MULTIPLE_FIXED_CHOICE}: Pregunta de opción múltiple
#'       \item \code{TEXT}: Pregunta de texto libre
#'       \item \code{NUMBER}: Pregunta numérica
#'       \item \code{PHOTO}: Pregunta de fotografía
#'       \item \code{MULTIPLE_FIXED_CHOICE_OPEN}: Opción múltiple con texto abierto
#'     }
#'   }
#'   \item{forEachValue}{Indica si la propiedad en loop debe tratarse como simple:
#'     \itemize{
#'       \item \code{"YES"}: Forzar tratamiento como loop simple (forEach)
#'       \item \code{NA}: Usar comportamiento por defecto basado en questionClass
#'     }
#'   }
#'   \item{maxOccurrences}{Máximo número de ocurrencias para preguntas múltiples}
#'   \item{inLoop}{Indica si la propiedad está dentro de un loop:
#'     \itemize{
#'       \item \code{"YES"}: La propiedad se repite en un loop
#'       \item \code{"NO"}: La propiedad aparece una sola vez
#'     }
#'   }
#'   \item{loopMax}{Máximo número de iteraciones del loop (solo si inLoop = "YES")}
#'   \item{question}{Texto completo de la pregunta (para documentación)}
#' }
#'
#' @details
#' ## Flujo de trabajo típico:
#'
#' 1. **Generación automática**: El archivo data_book.tsv se genera automáticamente
#'    desde el XML del cuestionario usando una herramienta Java
#'
#' 2. **Creación de plantilla**: Se crea data_book.xlsx agregando propiedades del
#'    sistema (GPS, timestamps) al archivo base
#'
#' 3. **Configuración manual**: Se edita manualmente data_book.xlsx para crear
#'    data_book_completo.xlsx, agregando las columnas \code{action} y \code{table}
#'
#' 4. **Transformación**: Se usa data_book_completo.xlsx con
#'    \code{\link{sbc_transformar_datos_a_tablas}} para transformar los datos
#'
#' ## Tipos de tablas comunes:
#'
#' - **encuestas**: Datos demográficos y de contexto de la encuesta
#' - **productos**: Información sobre productos mencionados/comprados
#' - **promociones**: Datos sobre promociones y ofertas
#' - **consumo_bebidas**: Patrones de consumo (estructura de loop)
#' - **atributos**: Atributos y características evaluadas
#'
#' @family data_transformation
#' @seealso \code{\link{sbc_transformar_datos_a_tablas}}, \code{\link{sbc_crear_plantilla_data_book}}
#'
#' @examples
#' # Ver estructura del data_book
#' head(ejemplo_data_book)
#'
#' # Filtrar por tabla
#' data_book_encuestas <- subset(ejemplo_data_book, table == "encuestas")
#'
#' # Ver propiedades que se ignoran
#' propiedades_ignoradas <- subset(ejemplo_data_book, action == "ignorar")
#'
"ejemplo_data_book"