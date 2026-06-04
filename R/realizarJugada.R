#' Colocar una ficha en el tablero
#'
#' @description Simula la física del juego: deja caer una ficha del jugador
#'   indicado en la columna seleccionada. La ficha ocupa la celda más baja
#'   disponible (fila libre más cercana al fondo). Si la columna está llena
#'   devuelve el tablero sin modificar.
#'
#' @param tablero Matriz 6×7 con el estado actual. Celdas: 0 vacío, 1 humano,
#'   2 IA.
#' @param columna Entero 1-7. Columna donde se deposita la ficha.
#' @param jugador Entero. Identificador del jugador: \code{1} (humano) o
#'   \code{2} (IA).
#'
#' @return Copia del tablero con la ficha insertada. Si la columna está llena
#'   se devuelve el tablero original sin cambios.
#'
#' @examples
#' tablero <- reiniciar_tablero()
#' tablero <- realizar_jugada(tablero, columna = 4, jugador = 1)
#' tablero <- realizar_jugada(tablero, columna = 4, jugador = 2)
#' visualizar_tablero(tablero)
#'
#' @seealso \code{\link{jugadas_disponibles}}, \code{\link{realizar_jugada_r}}
#'   (versión C++ equivalente)
#'
#' @export
realizar_jugada <- function(tablero, columna, jugador) {
  for (fila in 6:1) {
    if (tablero[fila, columna] == 0) {
      tablero[fila, columna] <- jugador
      return(tablero)
    }
  }
  return(tablero)
}
