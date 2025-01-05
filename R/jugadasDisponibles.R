#' Jugadas disponibles
#'
#' @description dada una situacvión en el tablero de juego, devuelve las posibles
#'   jugadas existentes: columnas no completadas
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' tablero <- readRDS("tableroPruebas")
#' visualizar_tablero(tablero)
#' (jugadas_candidatas <- jugadas_disponibles(tablero))
#' ordenar_jugadas(tablero, jugadas_candidatas)


jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  jugadas_candidatas <- which(tablero[1, ] == 0)
  return(jugadas_candidatas)
}


# función ordenar jugadas de más prometedoras a menos
ordenar_jugadas <- function(tablero, turno) {
  puntuacion <- c()
  
  jugadas_candidatas <- jugadas_disponibles(tablero)
  
  for (i in jugadas_candidatas) {
    tablero_aux <- realizar_jugada(tablero, i, turno)
    puntuacion <- c(puntuacion, evaluar_posicion(tablero_aux))
  }
  
  
  df <- data.frame(jugadas = jugadas_candidatas, puntuacion = puntuacion)
  df <- df[order(df$puntuacion, decreasing = ifelse(turno == 2, TRUE, FALSE)), ]
  
  # poda de jugadas
  if (any(abs(df$puntuacion) > 10000)) {
    df <- df[which(abs(df$puntuacion) > 10000), ]
  }
    
  return(df)
  
}
