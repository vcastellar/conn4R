#' Detectar si la partida ha terminado
#'
#' @description Comprueba si el tablero refleja un estado terminal: victoria de
#'   algún jugador (cuatro fichas en línea) o empate (tablero lleno sin ganador).
#'   Para detectar victorias utiliza \code{.evaluar_turno()} internamente: una
#'   puntuación ≥ 50 000 indica cuatro fichas en línea del jugador evaluado.
#'
#' @param tablero Matriz 6×7 con el estado del tablero. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#'
#' @return Lista con dos elementos:
#' \describe{
#'   \item{finalizado}{Lógico. \code{TRUE} si la partida ha concluido.}
#'   \item{resultado}{Entero o \code{NA}. Si \code{finalizado = TRUE}:
#'     \code{1} gana el jugador 1 (humano), \code{2} gana el jugador 2 (IA),
#'     \code{0} empate. Si la partida continúa, \code{NA}.}
#' }
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' visualizar_tablero(tablero)
#' juego_terminado(tablero)
#'
#' # Tablero vacío: la partida no ha terminado
#' juego_terminado(reiniciar_tablero())
#'
#' @seealso \code{\link{juego_terminado_cpp}} (versión C++ equivalente)
#'
#' @export
juego_terminado <- function(tablero) {
  for (jugador in 1:2) {
    if (abs(.evaluar_turno(tablero, jugador)) >= 50000) {
      if (jugador == 1) {
        return(list(finalizado = TRUE, resultado = 1))
      } else {
        return(list(finalizado = TRUE, resultado = 2))
      }
    }
  }

  if (length(jugadas_disponibles(tablero)) == 0) {
    return(list(finalizado = TRUE, resultado = 0))
  }

  return(list(finalizado = FALSE, resultado = NA))
}
