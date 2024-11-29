#' Jugadas disponibles
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @examples
#' tablero <- crear_posicion_aleatoria(20)
#' visualizar_tablero(tablero)
#' jugadas_disponibles(tablero)


jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  columnas_disponibles <- which(tablero[1, ] == 0)
  return(columnas_disponibles)
}
