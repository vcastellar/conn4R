#' Crear un tablero vacío
#'
#' @description Inicializa y devuelve un tablero de Conecta 4 vacío: una matriz
#'   de 6 filas y 7 columnas rellena con ceros.
#'
#'   Convenio de valores de celda:
#'   \describe{
#'     \item{0}{Casilla vacía.}
#'     \item{1}{Ficha del jugador 1 (humano).}
#'     \item{2}{Ficha del jugador 2 (IA).}
#'   }
#'
#'   La fila 1 corresponde a la parte superior del tablero y la fila 6 a la
#'   parte inferior (las piezas caen por gravedad hasta la primera fila libre
#'   desde abajo).
#'
#' @return Matriz entera 6×7 inicializada a 0.
#'
#' @examples
#' tablero <- reiniciar_tablero()
#' dim(tablero)   # 6 7
#' all(tablero == 0)
#'
#' @export
reiniciar_tablero <- function() {
  tablero <- matrix(0L, nrow = 6, ncol = 7)
  return(tablero)
}
