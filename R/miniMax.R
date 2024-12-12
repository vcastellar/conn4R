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
#' tablero <- crear_posicion_aleatoria(3)
#' kk <- minimax(tablero = tablero, profundidad = 3, maximizandoIA = TRUE)
#' kk
#' kk$env$arbol



minimax <- minimax <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL) {
  
  # Inicializar el entorno solo si no fue pasado como argumento
  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")
  }
  
  # Verificar condición de parada
  if (profundidad == 0 | juego_terminado(tablero)$finalizado) {
    return(list(puntuacion = evaluar_posicion(tablero), 
                jugada     = NA, 
                env        = env)
           )
  }
  
  if (maximizandoIA) {
    mejor_puntuacion <- -Inf
    mejor_jugada <- NA
    
    for (columna in jugadas_disponibles(tablero)) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 2)
      
      # actualizar arbol
      env$arbol <- actualizar(env$arbol, 
                              turno       = TRUE, 
                              jugada      = as.integer(columna), 
                              profundidad = as.integer(profundidad))
      
      res        <- minimax(nuevo_tablero, profundidad - 1, FALSE, alpha, beta, env)
      
      if (res$puntuacion > mejor_puntuacion) {
        mejor_puntuacion <- res$puntuacion
        mejor_jugada     <- columna
      }
      
      alpha <- max(alpha, mejor_puntuacion)
      if (beta <= alpha) {
        break  # Poda alpha-beta
      }
      
    }
    
    return(list(puntuacion = mejor_puntuacion,
                jugada     = mejor_jugada,
                env        = env))
    
  } else {
    mejor_puntuacion <- Inf
    mejor_jugada <- NA
    
    for (columna in jugadas_disponibles(tablero)) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 1)
      
      env$arbol  <- actualizar(env$arbol, FALSE, as.integer(columna), as.integer(profundidad))
      
      res        <- minimax(nuevo_tablero, profundidad - 1, TRUE, alpha, beta, env)
      
      if (res$puntuacion < mejor_puntuacion) {
        mejor_puntuacion <- res$puntuacion
        mejor_jugada     <- columna

      }
      
      beta <- min(beta, mejor_puntuacion)
      if (beta <= alpha) {
        break  # Poda alpha-beta
      }
    }
    
    return(list(puntuacion = mejor_puntuacion,
                jugada     = mejor_jugada,
                env        = env))
  }
}

.actualizarEnv <- function(env, turno, jugada, profundidad, puntuacion = NA) {
  env$info$numNodos          <- env$info$numNodos + 1 # Incrementar el contador
  env$info$arbol$turno       <- c(env$info$arbol$turno, turno)
  env$info$arbol$jugada      <- c(env$info$arbol$jugada, jugada)
  env$info$arbol$profundidad <- c(env$info$arbol$profundidad, profundidad)
  env$info$arbol$idNodo      <- c(env$info$arbol$idNodo, env$info$numNodos)
  env$info$arbol$puntuacion  <- c(env$info$arbol$puntuacion, puntuacion)
}

