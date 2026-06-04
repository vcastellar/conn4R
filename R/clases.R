#' Clase S4 para representar el árbol de búsqueda minimax
#'
#' @description La clase \code{arbol} almacena todos los nodos evaluados durante
#'   una llamada al algoritmo minimax con \code{guardar_arbol = TRUE}. Cada
#'   nodo ocupa una posición en los vectores de slots; el índice común es
#'   \code{idNodo}.
#'
#'   Se puede recorrer el árbol de forma top-down con
#'   \code{\link{encontrar_mejor_variante}} para extraer la línea de juego
#'   óptima, y visualizarla paso a paso con
#'   \code{\link{mostrar_mejor_variante}}.
#'
#' @slot idNodo     Vector entero. Identificador único del nodo (1, 2, 3, …).
#' @slot idPadre    Vector entero. \code{idNodo} del nodo padre;
#'   \code{NA} para la raíz.
#' @slot jugada     Vector entero. Columna (1-7) jugada para alcanzar el nodo;
#'   \code{NA} para la raíz.
#' @slot turno      Vector lógico. \code{TRUE} si en este nodo mueve la IA
#'   (nodo MAX); \code{FALSE} si mueve el humano (nodo MIN).
#' @slot profundidad Vector entero. Nivel del nodo en el árbol (0 = raíz).
#' @slot puntuacion  Vector numérico. Evaluación minimax del nodo, asignada
#'   tras explorar el subárbol correspondiente.
#'
#' @examples
#' \dontrun{
#' tablero <- crear_posicion_aleatoria(7)
#' res <- minimax(tablero, profundidad = 4, maximizandoIA = TRUE,
#'                guardar_arbol = TRUE)
#' arbol <- res$arbol
#' length(arbol@idNodo)          # número total de nodos explorados
#' head(data.frame(
#'   nodo  = arbol@idNodo,
#'   padre = arbol@idPadre,
#'   col   = arbol@jugada,
#'   punt  = arbol@puntuacion
#' ))
#' }
#'
#' @seealso \code{\link{minimax}}, \code{\link{encontrar_mejor_variante}},
#'   \code{\link{mostrar_mejor_variante}}
#'
#' @export
setClass("arbol",
         slots = c(
           idNodo      = "integer",
           idPadre     = "integer",
           jugada      = "integer",
           turno       = "logical",
           profundidad = "integer",
           puntuacion  = "numeric"
         ),
         prototype = list(
           idNodo      = integer(),
           idPadre     = integer(),
           jugada      = integer(),
           turno       = logical(),
           profundidad = integer(),
           puntuacion  = numeric()
         )
)

#' Añadir un nodo al árbol de búsqueda
#'
#' @description Método genérico que agrega un nuevo nodo al objeto
#'   \code{\link{arbol}} incrementando en 1 el máximo \code{idNodo} existente.
#'   Usado internamente por \code{\link{minimax}} cuando \code{guardar_arbol = TRUE}.
#'
#' @param x      Objeto de clase \code{arbol}.
#' @param idPadre    Entero. \code{idNodo} del nodo padre; \code{NA_integer_}
#'   para la raíz.
#' @param turno      Lógico. \code{TRUE} = turno de la IA (MAX).
#' @param jugada     Entero. Columna jugada para llegar a este nodo.
#' @param profundidad Entero. Nivel del nodo en el árbol.
#' @param puntuacion  Numérico. Puntuación del nodo (puede ser \code{NA_real_}
#'   si todavía no se ha calculado).
#' @param ...    Argumentos adicionales (ignorados).
#'
#' @return Objeto \code{arbol} actualizado con el nuevo nodo añadido.
#'
#' @seealso \code{\link{arbol}}, \code{\link{minimax}}
setGeneric("actualizar", function(x, ...) standardGeneric("actualizar"))

setMethod("actualizar", "arbol",
          function(x,
                   idPadre     = NA_integer_,
                   turno       = NULL,
                   jugada      = NULL,
                   profundidad = NULL,
                   puntuacion  = NULL) {

            stopifnot(isS4(x), is(x, "arbol"))

            nuevo_id <- ifelse(length(x@idNodo) == 0, 0L, max(x@idNodo)) + 1L

            x@idNodo      <- c(x@idNodo,      as.integer(nuevo_id))
            x@idPadre     <- c(x@idPadre,     as.integer(idPadre))
            x@turno       <- c(x@turno,       as.logical(turno))
            x@jugada      <- c(x@jugada,       as.integer(jugada))
            x@profundidad <- c(x@profundidad, as.integer(profundidad))
            x@puntuacion  <- c(x@puntuacion,  as.numeric(puntuacion))

            return(x)
          }
)

#' Modificar el último nodo del árbol de búsqueda
#'
#' @description Actualiza el valor de un slot en el último nodo del objeto
#'   \code{\link{arbol}}. Usado internamente por \code{\link{minimax}} para
#'   asignar la puntuación definitiva de un nodo tras explorar su subárbol.
#'
#' @param obj      Objeto de clase \code{arbol}.
#' @param slotName Cadena de texto. Nombre del slot a modificar (por ejemplo,
#'   \code{"puntuacion"}).
#' @param value    Valor que se asignará al último elemento del slot.
#' @param ...      Argumentos adicionales (ignorados).
#'
#' @return Objeto \code{arbol} con el slot modificado.
#'
#' @seealso \code{\link{arbol}}, \code{\link{actualizar}}
setGeneric("actUltNodo", function(obj, ...) standardGeneric("actUltNodo"))

setMethod("actUltNodo", "arbol",
          function(obj, slotName, value) {
            l <- length(slot(obj, slotName))
            slot(obj, slotName)[l] <- value
            return(obj)
          })

#' Extraer la mejor variante del árbol de búsqueda
#'
#' @description Recorre el objeto \code{\link{arbol}} generado por
#'   \code{\link{minimax}} con \code{guardar_arbol = TRUE} y devuelve la
#'   secuencia de nodos que constituye la línea de juego óptima (variante
#'   principal): desde la raíz hasta la hoja, eligiendo en cada nodo MAX el
#'   hijo con mayor puntuación y en cada nodo MIN el de menor puntuación.
#'
#' @param arbol Objeto de clase \code{\link{arbol}} obtenido de
#'   \code{minimax(..., guardar_arbol = TRUE)$arbol}.
#'
#' @return Lista de listas. Cada elemento representa un nodo de la variante y
#'   contiene:
#' \describe{
#'   \item{nodo}{Identificador del nodo (\code{idNodo}).}
#'   \item{jugada}{Columna (1-7) jugada para llegar al nodo, o \code{NA} en
#'     la raíz.}
#'   \item{turno}{Lógico. \code{TRUE} si este nodo es MAX (turno IA).}
#'   \item{puntuacion}{Evaluación minimax del nodo.}
#' }
#'
#' @examples
#' \dontrun{
#' tablero <- crear_posicion_aleatoria(7)
#' res <- minimax(tablero, profundidad = 4, maximizandoIA = TRUE,
#'                guardar_arbol = TRUE)
#' variante <- encontrar_mejor_variante(res$arbol)
#' sapply(variante, function(n) n$jugada)  # secuencia de columnas
#' }
#'
#' @seealso \code{\link{arbol}}, \code{\link{mostrar_mejor_variante}}
#'
#' @export
encontrar_mejor_variante <- function(arbol) {

  nodo_actual <- which(is.na(arbol@idPadre))[1]
  if (is.na(nodo_actual)) stop("No se encontró nodo raíz")

  mejor_variante <- list()

  repeat {
    mejor_variante <- c(mejor_variante, list(list(
      nodo       = arbol@idNodo[nodo_actual],
      jugada     = arbol@jugada[nodo_actual],
      turno      = arbol@turno[nodo_actual],
      puntuacion = arbol@puntuacion[nodo_actual]
    )))

    hijos <- which(arbol@idPadre == arbol@idNodo[nodo_actual])

    if (length(hijos) == 0) break

    if (arbol@turno[nodo_actual]) {
      mejor_idx <- which.max(arbol@puntuacion[hijos])
    } else {
      mejor_idx <- which.min(arbol@puntuacion[hijos])
    }

    nodo_actual <- hijos[mejor_idx]
  }

  return(mejor_variante)
}

#' Reproducir visualmente la mejor variante de una partida
#'
#' @description Toma la variante principal extraída por
#'   \code{\link{encontrar_mejor_variante}} y reproduce las jugadas una a una
#'   sobre el tablero inicial, imprimiendo el gráfico del tablero en cada paso
#'   con una pausa configurable entre movimientos.
#'
#' @param tablero_inicial Matriz 6×7 con el estado del tablero en el momento
#'   en que se inició el análisis minimax.
#' @param variante Lista devuelta por \code{\link{encontrar_mejor_variante}}.
#' @param pausa_ms Numérico. Milisegundos de espera entre cada jugada mostrada.
#'   Por defecto \code{1000}.
#'
#' @return Invisible. La función se llama por su efecto secundario (visualización).
#'
#' @examples
#' \dontrun{
#' tablero <- crear_posicion_aleatoria(7)
#' res <- minimax(tablero, profundidad = 4, maximizandoIA = TRUE,
#'                guardar_arbol = TRUE)
#' variante <- encontrar_mejor_variante(res$arbol)
#' mostrar_mejor_variante(tablero, variante, pausa_ms = 800)
#' }
#'
#' @seealso \code{\link{encontrar_mejor_variante}}, \code{\link{visualizar_tablero}}
#'
#' @export
mostrar_mejor_variante <- function(tablero_inicial, variante, pausa_ms = 1000) {
  tablero <- tablero_inicial

  for (paso in variante) {
    jugada <- paso$jugada
    turno  <- ifelse(paso$turno, 1, 2)

    if (!is.na(jugada)) {
      tablero <- realizar_jugada(tablero, jugada, turno)
    }

    print(visualizar_tablero(tablero))
    Sys.sleep(pausa_ms / 1000)
  }
}
