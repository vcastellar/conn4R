#' algoritmo minimax prueba2
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
#' tablero <- crear_posicion_aleatoria(7)
#' visualizar_tablero(tablero)
#' system.time({
#'   kk <- minimax(tablero = tablero, profundidad = 7, maximizandoIA = TRUE)
#' })
#' max(kk$env$arbol@idNodo)
#' kk$puntuacion
#' kk$jugada
#' kk$env$nodos
#' 

minimax <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL) {
  
  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")
    env$nodos <- 0
  }
  
  env$nodos <- env$nodos + 1
  
  turno <- ifelse(maximizandoIA, 2, 1)
  mejor_puntuacion <- if (maximizandoIA) -Inf else Inf
  mejor_jugada <- NA
  
  if (profundidad == 0 || juego_terminado(tablero)$finalizado) {
    return(list(
      puntuacion = evaluar_posicion(tablero, turno),  # 👈 Siempre desde perspectiva del jugador 2 (IA)
      jugada = NA,
      env = env
    ))
  }
  
  comparar <- if (maximizandoIA) `>` else `<`
  
  jugadas_candidatas <- ordenar_jugadas(tablero, turno)$jugadas
  
  for (columna in jugadas_candidatas) {
    nuevo_tablero <- realizar_jugada(tablero, columna, turno)
    
    res <- minimax(nuevo_tablero, profundidad - 1, !maximizandoIA, alpha, beta, env)
    
    if (comparar(res$puntuacion, mejor_puntuacion)) {
      mejor_puntuacion <- res$puntuacion
      mejor_jugada <- columna
      
      env$arbol <- actualizar(env$arbol,
                              turno       = turno,
                              jugada      = as.integer(columna),
                              profundidad = as.integer(profundidad),
                              puntuacion  = mejor_puntuacion)
    }
    
    if (maximizandoIA) {
      alpha <- max(alpha, mejor_puntuacion)
    } else {
      beta <- min(beta, mejor_puntuacion)
    }
    
    if (beta <= alpha) break
  }
  
  return(list(puntuacion = mejor_puntuacion, jugada = mejor_jugada, env = env))
}

# minimax <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL, jugadorIA = 2) {
#   
#   if (is.null(env)) {
#     env <- new.env()
#     env$arbol <- new("arbol")
#     env$nodos <- 0
#   }
#   
#   env$nodos <- env$nodos + 1
#   
#   turno <- ifelse(maximizandoIA, jugadorIA, ifelse(jugadorIA == 1, 2, 1))
#   mejor_puntuacion <- if (maximizandoIA) -Inf else Inf
#   mejor_jugada <- NA
#   
#   if (profundidad == 0 | juego_terminado(tablero)$finalizado) {
#     return(list(
#       puntuacion = evaluar_posicion(tablero, jugadorIA),  # Evaluar siempre para IA
#       jugada = NA,
#       env = env
#     ))
#   }
#   
#   comparar <- if (maximizandoIA) `>` else `<`
#   
#   jugadas_candidatas <- ordenar_jugadas(tablero, turno)
#   
#   for (columna in jugadas_candidatas$jugadas) {
#     nuevo_tablero <- realizar_jugada(tablero, columna, turno)
#     res <- minimax(nuevo_tablero, profundidad - 1, !maximizandoIA, alpha, beta, env, jugadorIA)
#     
#     if (comparar(res$puntuacion, mejor_puntuacion)) {
#       mejor_puntuacion <- res$puntuacion
#       mejor_jugada <- columna
#       
#       env$arbol <- actualizar(env$arbol, turno = turno, jugada = as.integer(columna),
#                               profundidad = as.integer(profundidad), puntuacion = mejor_puntuacion)
#     }
#     
#     if (maximizandoIA) alpha <- max(alpha, mejor_puntuacion) else beta <- min(beta, mejor_puntuacion)
#     if (beta <= alpha) break
#   }
#   
#   return(list(puntuacion = mejor_puntuacion, jugada = mejor_jugada, env = env))
# }
# 
# 
# seleccionar_jugadas_candidatas <- function(tablero, maxIA, profundidad_corta = 2, top_k = 3) {
#   jugadas <- ordenar_jugadas(tablero)$jugadas
#   resultados <- data.frame(jugada = integer(), valor = numeric())
#   
#   for (col in jugadas) {
#     # Simular jugada
#     tablero_nuevo <- realizar_jugada(tablero, col, if (maxIA) 2 else 1)
#     
#     # Evaluar a poca profundidad
#     valor <- minimax(tablero_nuevo, profundidad_corta, !maxIA)
#     
#     resultados <- rbind(resultados, data.frame(jugada = col, valor = valor$puntuacion))
#   }
#   
#   # Ordenar según si estamos maximizando o minimizando
#   resultados <- resultados[order(if (maxIA) -resultados$valor else resultados$valor), ]
#   
#   # Devolver las k mejores jugadas
#   return(head(resultados$jugada, top_k))
# }
# 

