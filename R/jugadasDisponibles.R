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


# función ordenar jugadas de más prometedoras a menos
ordenar_jugadas <- function(tablero, turno) {
  
  jugadas_candidatas <- jugadas_disponibles(tablero)
  puntuacion <- rep(0, length(jugadas_candidatas))
  
  # consecuencia de jugar columna i y de no jugar columna i
  for (turno in c(1, 2)) {
    for (i in 1:length(jugadas_candidatas)) {
      columna <- jugadas_candidatas[i]
      tablero_aux <- realizar_jugada(tablero, columna, turno)
      puntuacion[i]  <- abs(puntuacion[i]) + abs(evaluar_posicion(tablero_aux))
    }
  }

  
  df <- data.frame(jugadas = jugadas_candidatas, puntuacion = puntuacion)
  df <- df[order(df$puntuacion, decreasing = TRUE), ]
  
  # poda de jugadas
  if (any(abs(df$puntuacion) > 10000)) {
    df <- df[which(abs(df$puntuacion) > 10000), ]
  }
    
  return(df)
  
}
