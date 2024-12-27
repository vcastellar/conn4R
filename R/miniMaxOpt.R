#' algoritmo minimax
#'
#' @description función que mediante un algoritmo mini-max la IA decide cuál es su mejor
#' jugada dada una cierta posición del tablero 

#' @param tablero a matrix representing the state of the game board
#' @param profundidad un entero que fija la profundidad del árbol de jugadas a analizar
#' @param maximizandoIA Booleano. TRUE significa que se maximiza la puntuación de la IA
#'   FALSE se minimiza la puntuación del jugador humano
#' @param alpha parámetro de la poda alpha-beta. Por defecto -Inf
#' @param beta parámetro de la poda alpha-beta. Por defecto +Inf

#' @return returns a list with the following contents
#' \itemize{
#' \item{puntuacion}: puntuación optenida al evaluar la posición al realizar la 'jugada'
#' \item{jugada}: jugada elegida por el algoritmo
#' }
#' @details
#' \itemize{
#' \item{If the game is over is TRUE}: result can be "WIN HUMAN", "WIN IA" or "DRAW’.
#' \item{If the game is over is TRUE}: result is NA
#' }
#' la poda alpha-beta reduce drásticamente el número de nodos que se evaluan:
#' a profundidad 5, con tres movimientos realizados en el tablero, el algoritmo minimax calcula:
#' \itemize{
#' \item con poda alpha-beta: 4.677 nodos
#' \item sin poda alpha-beta: 19.607 nodos
#' }
#' @examples
#' tablero <- crear_posicion_aleatoria(25)
#' visualizar_tablero(tablero)
#' kk <- minimax(tablero = tablero, profundidad = 3, maximizandoIA = TRUE)
#' kk
#' x <- kk$env$arbol
#' x

minimaxOpt <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL) {
  
  # Inicializar el entorno solo si no fue pasado como argumento
  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")
  }
  
  # Verificar condición de parada (sin realizar evaluación adicional si ya es final)
  if (profundidad == 0 || juego_terminado(tablero)$finalizado) {
    return(list(puntuacion = evaluar_posicion(tablero), jugada = NA, env = env))
  }
  
  # Variables de control
  mejor_puntuacion <- ifelse(maximizandoIA, -Inf, Inf)
  mejor_jugada <- NA
  jugadas <- jugadas_disponibles(tablero)  # Calcular jugadas disponibles solo una vez
  
  # Función recursiva Minimax
  for (columna in jugadas) {
    # Realizar la jugada
    nuevo_tablero <- realizar_jugada(tablero, columna, ifelse(maximizandoIA, 2, 1))
    punt <- evaluar_posicion(nuevo_tablero)
    
    # Actualizar el árbol (solo si es necesario)
    env$arbol <- actualizar(env$arbol, 
                            turno = maximizandoIA, 
                            jugada = as.integer(columna), 
                            profundidad = as.integer(profundidad),
                            puntuacion = punt)
    
    # Recursión Minimax
    res <- minimaxOpt(nuevo_tablero, profundidad - 1, !maximizandoIA, alpha, beta, env)
    
    # Actualización de puntuación y poda
    if (maximizandoIA) {
      if (res$puntuacion > mejor_puntuacion) {
        mejor_puntuacion <- res$puntuacion
        mejor_jugada <- columna
      }
      alpha <- max(alpha, mejor_puntuacion)
    } else {
      if (res$puntuacion < mejor_puntuacion) {
        mejor_puntuacion <- res$puntuacion
        mejor_jugada <- columna
      }
      beta <- min(beta, mejor_puntuacion)
    }
    
    # Poda Alpha-Beta
    if (beta <= alpha) {
      break
    }
  }
  
  return(list(puntuacion = mejor_puntuacion, jugada = mejor_jugada, env = env))
}
