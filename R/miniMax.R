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
#' tablero <- crear_posicion_aleatoria(10)
#' visualizar_tablero(tablero)
#' system.time({
#'   kk <- minimax(tablero = tablero, profundidad = 9, maximizandoIA = TRUE)
#' })
#' kk$env$contador
#' kk$puntuacion
#' kk$jugada


minimax <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL) {
  
  # Inicializar el entorno solo si no fue pasado como argumento
  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")
    env$contador <- 0
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
    
    jugadas_candidatas <- jugadas_disponibles(tablero) 
    jugadas_candidatas <- ordenar_jugadas(tablero, jugadas_candidatas, turno = 2)$jugadas
    for (columna in jugadas_candidatas) {
      
      nuevo_tablero <- realizar_jugada(tablero, columna, 2)
      
      # actualizar arbol
      env$contador <- env$contador + 1
      env$arbol <- actualizar(env$arbol,
                              turno       = TRUE,
                              jugada      = as.integer(columna),
                              profundidad = as.integer(profundidad),
                              puntuacion  = evaluar_posicion(nuevo_tablero))
      
      res <- minimax(nuevo_tablero, profundidad - 1, FALSE, alpha, beta, env)
      
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
    
    jugadas_candidatas <- jugadas_disponibles(tablero) 
    jugadas_candidatas <- rev(ordenar_jugadas(tablero, jugadas_candidatas, turno = 1)$jugadas)
    for (columna in jugadas_candidatas) {
      nuevo_tablero <- realizar_jugada(tablero, columna, 1)
      
      # actualizar arbol
      env$contador <- env$contador + 1
      env$arbol <- actualizar(env$arbol,
                              turno       = FALSE,
                              jugada      = as.integer(columna),
                              profundidad = as.integer(profundidad),
                              puntuacion  = evaluar_posicion(nuevo_tablero))
      
      res <- minimax(nuevo_tablero, profundidad - 1, TRUE, alpha, beta, env)
      
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

