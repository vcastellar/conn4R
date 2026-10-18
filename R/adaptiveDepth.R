# Fracción de columnas disponibles (suma de ocupación relativa por columna)
.columnasValidas <- function(tablero) {
  sum(apply(tablero, 2, function(x) { sum(x == 0) / 6 }))
}

#' Profundidad de búsqueda adaptativa
#'
#' @description Calcula la profundidad óptima del árbol minimax en función del
#'   número efectivo de columnas disponibles, manteniendo un presupuesto fijo de
#'   nodos evaluados equivalente a \code{7^profundidad_base}.
#'
#'   A medida que el tablero se llena, las ramas posibles disminuyen y la misma
#'   profundidad cuesta menos nodos; esta función aprovecha ese margen para
#'   aumentar automáticamente la profundidad sin superar el presupuesto original.
#'
#' @param tablero Matriz de 6 x 7 con el estado actual del tablero.
#' @param profundidad_base Entero. Profundidad mínima garantizada y referencia
#'   para calcular el presupuesto de nodos (\code{7^profundidad_base}).
#'   Por defecto \code{7}.
#' @param profundidad_max Entero. Profundidad máxima permitida. Por defecto
#'   \code{15}.
#'
#' @return Entero. Profundidad ajustada para la siguiente llamada a minimax.
#'
#' @details
#' El algoritmo calcula \eqn{b} (columnas válidas ponderadas) y busca la
#' profundidad \eqn{d \in [profundidad\_base, profundidad\_max]} tal que
#' \eqn{b^d} sea lo más cercano posible a \eqn{7^{profundidad\_base}}.
#' Si \eqn{b \leq 1} devuelve directamente \code{profundidad_max}.
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(10)
#' .adaptativa(tablero, profundidad_base = 5)
#'
#' # Con tablero casi lleno la profundidad sube
#' tablero_lleno <- crear_posicion_aleatoria(35)
#' .adaptativa(tablero_lleno, profundidad_base = 5)
.adaptativa <- function(tablero, profundidad_base = 7, profundidad_max = 15) {
  b <- .columnasValidas(tablero)
  if (b <= 1) return(profundidad_max)

  N        <- 7 ^ profundidad_base
  d_range  <- profundidad_base:profundidad_max
  d_mejor  <- d_range[which.min(abs(b ^ d_range - N))]

  return(d_mejor)
}

# Cuenta el total de piezas colocadas en el tablero
.numJugadas <- function(tablero) {
  sum(tablero != 0)
}
