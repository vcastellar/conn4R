#' solicita una jugada al humano.
#'
#' @description la función invita al jugador humano a introducir una jugada
#' Si la jugada es legal representeala jugada del humano en el tablero mediante
#' un 1 en la casilla que corresponda. La jugada de la IA será representada en el
#' tablero mediante un 2.
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @param jugada un entero del 1 al 7 correspondiente a cada una de las 7 columnas
#' si jugada = NULL (valor por defecto), entonces invita al jugador a introducir su jugada
#' @details
#'# Si la jugada es legal representa la jugada del humano en el tablero mediante un 1
#' en la casilla que corresponda a la jugada
#' Si la jugada introducida es ilegal, vuelve a solicitar una jugada legal
#'
#'
#' @examples
#' tablero <- reiniciar_tablero()
#' tablero <- turno_humano(tablero, jugada = 4)
#' visualizar_tablero(tablero)
#' # tambien se puede hacer lo siguiente (no ejecutar):
#' # tablero <- reiniciar_tablero()
#' # tablero <- turno_humano(tablero)
#' # visualizar_tablero(tablero)



turno_humano <- function(tablero, jugada = NULL) {

  jugadas_posibles <- jugadas_disponibles(tablero)

  if (length(jugadas_posibles) == 0) {
    return(tablero) # No hay jugadas disponibles
  }

  repeat {

    if (is.null(jugada)) {
      cat("Tu turno. Elige una columna (1-7): ")
      jugada_humano <- as.integer(readLines(con = stdin(), n = 1))
      # jugada_humano <- as.integer(readline())

    } else {
      jugada_humano <- jugada
    }

    if (jugada_humano %in% jugadas_posibles) {
      # Encuentra la primera fila vacía en la columna elegida
      for (fila in 6:1) {
        if (tablero[fila, jugada_humano] == 0) {
          tablero[fila, jugada_humano] <- 1  # Representa la jugada del humano
          return(tablero)
        }
      }

    } else {
      cat("Jugada no válida. Intenta de nuevo.\n")
    }
  }
}
