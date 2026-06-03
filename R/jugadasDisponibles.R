#' Jugadas disponibles
#'
#' @description dada una situación en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @examples
#' tablero <- crear_posicion_aleatoria(11)
#' tablero <- readRDS("tableroPruebas.rds")
#' visualizar_tablero(tablero)
#' (jugadas_candidatas <- jugadas_disponibles(tablero))
#' ordenar_jugadas(tablero, turno = 2)
#' ordenar_jugadas_cpp(tablero, turno = 2, profundidad = 0)



jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  jugadas_candidatas <- which(tablero[1, ] == 0)
  return(jugadas_candidatas)
}


ordenar_jugadas <- function(tablero, turno) {
  jugadas_candidatas <- jugadas_disponibles(tablero)

  n <- length(jugadas_candidatas)
  puntuaciones <- numeric(n)

  oponente <- ifelse(turno == 1L, 2L, 1L)

  # Prioridad máxima: victoria inmediata o bloqueo de victoria rival
  WIN_SCORE  <- 1e9
  BLOCK_SCORE <- 1e8

  for (i in seq_len(n)) {
    col <- jugadas_candidatas[i]

    tablero_j <- realizar_jugada(tablero, col, turno)
    if (juego_terminado(tablero_j)$finalizado && !is.na(juego_terminado(tablero_j)$resultado) && juego_terminado(tablero_j)$resultado == turno) {
      puntuaciones[i] <- WIN_SCORE
      next
    }

    tablero_o <- realizar_jugada(tablero, col, oponente)
    if (juego_terminado(tablero_o)$finalizado && !is.na(juego_terminado(tablero_o)$resultado) && juego_terminado(tablero_o)$resultado == oponente) {
      puntuaciones[i] <- BLOCK_SCORE
      next
    }

    eval_j <- evaluar_posicion(tablero_j)
    eval_o <- evaluar_posicion(tablero_o)
    puntuaciones[i] <- abs(eval_j) + abs(eval_o)
  }

  df <- data.frame(jugadas = jugadas_candidatas, puntuacion = puntuaciones)
  df <- df[order(df$puntuacion, decreasing = TRUE), ]

  return(df)
}
