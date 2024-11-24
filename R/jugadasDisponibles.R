#' Jugadas disponibles
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @examples
#' tablero <- crear_posicion_aleatoria(20)
#' visualizar_tablero(tablero)
#' jugadas_disponibles(tablero)

# función que en cada turno liste las jugadas disponibles del jugador en turno
# Cada jugador realiza un movimiento por turnos. Los movimientos
# consisten en dejar caer en una de las siete columnas una ficha rojo o amarilla, según el jugador.
# Un movimiento es legal si la columna donde juega el jugador no está completamente llenada.


jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  columnas_disponibles <- which(tablero[1, ] == 0)
  return(columnas_disponibles)
}

# Ejemplo de uso:
# Supongamos que es el turno del jugador 1
# Lista las columnas disponibles donde el jugador 1 puede jugar
