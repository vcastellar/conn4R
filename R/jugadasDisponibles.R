#' Jugadas disponibles
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @examples
#' tablero <- crear_posicion_aleatoria(11)
#' tablero <- readRDS("tableroPruebas.rds")
#' visualizar_tablero(tablero)
#' (jugadas_candidatas <- jugadas_disponibles(tablero))
#' ordenar_jugadas(tablero, turno = 2)


jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  jugadas_candidatas <- which(tablero[1, ] == 0)
  return(jugadas_candidatas)
}




ordenar_jugadas <- function(tablero, turno) {
  jugadas_candidatas <- jugadas_disponibles(tablero)
  n <- length(jugadas_candidatas)
  puntuaciones <- numeric(n)
  
  # Definir pesos: jugador actual y oponente
  pesos <- if (turno == 1) c(1.0, 0.5) else c(0.5, 1.0)
  oponente <- ifelse(turno == 1, 2, 1)
  
  for (i in seq_len(n)) {
    col <- jugadas_candidatas[i]
    
    # Jugada del jugador actual
    tablero_j <- realizar_jugada(tablero, col, turno)
    eval_j <- evaluar_posicion(tablero_j)
    
    # Jugada simulada del oponente
    tablero_o <- realizar_jugada(tablero, col, oponente)
    eval_o <- evaluar_posicion(tablero_o)
    
    # Suma ponderada
    puntuaciones[i] <- pesos[1] * abs(eval_j) + pesos[2] * abs(eval_o)
  }
  
  # Crear data.frame ordenado por puntuación descendente
  df <- data.frame(jugadas = jugadas_candidatas, puntuacion = puntuaciones)
  df <- df[order(df$puntuacion, decreasing = TRUE), ]
  
  # Poda opcional: mantener solo jugadas con evaluación fuerte
  if (any(abs(df$puntuacion) > 10000)) {
    df <- df[which(abs(df$puntuacion) > 10000), ]
  }
  
  return(df)
}
