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
#'   kk <- minimax_parallel(tablero = tablero, profundidad = 7, maximizandoIA = TRUE)
#' })
#' max(kk$env$arbol@idNodo)
#' kk$puntuacion
#' kk$jugada
#' kk$env$nodos


minimax <- function(tablero, profundidad, maximizandoIA, alpha = -Inf, beta = Inf, env = NULL, idPadre = NA_integer_) {
  
  if (is.null(env)) {
    env <- new.env()
    env$arbol <- new("arbol")
  }
  
  turno <- ifelse(maximizandoIA, 2, 1)
  mejor_puntuacion <- if (maximizandoIA) -Inf else Inf
  mejor_jugada <- NA
  
  # Caso base: profundidad 0 o juego terminado
  if (profundidad == 0 || juego_terminado(tablero)$finalizado) {
    return(list(
      puntuacion = evaluar_posicion(tablero, turno),
      jugada = NA,
      env = env
    ))
  }
  
  comparar <- if (maximizandoIA) `>` else `<`
  
  jugadas_candidatas <- ordenar_jugadas(tablero, turno, profundidad)$jugadas
  
  for (columna in jugadas_candidatas) {
    nuevo_tablero <- realizar_jugada(tablero, columna, turno)
    
    # Evaluación preliminar de la jugada actual para el árbol
    puntuacion_jugada <- evaluar_posicion(nuevo_tablero, turno)
    
    # 🔁 Actualizar el árbol con el nodo actual
    env$arbol <- actualizar(env$arbol,
                            idPadre     = idPadre,
                            turno       = maximizandoIA,  # O usa turno == 2
                            jugada      = as.integer(columna),
                            profundidad = as.integer(profundidad),
                            puntuacion  = puntuacion_jugada)
    
    # Obtener ID del nodo recién añadido (último)
    nuevo_id <- tail(env$arbol@idNodo, 1)
    
    # 🧠 Llamada recursiva pasando nuevo_id como padre
    res <- minimax(nuevo_tablero,
                   profundidad - 1,
                   !maximizandoIA,
                   alpha, beta,
                   env,
                   idPadre = nuevo_id)
    
    if (comparar(res$puntuacion, mejor_puntuacion)) {
      mejor_puntuacion <- res$puntuacion
      mejor_jugada <- columna
    }
    
    # Actualizar alpha/beta
    if (maximizandoIA) {
      alpha <- max(alpha, mejor_puntuacion)
    } else {
      beta <- min(beta, mejor_puntuacion)
    }
    
    # ✂️ Poda
    if (beta <= alpha) break
  }
  
  return(list(
    puntuacion = mejor_puntuacion,
    jugada = mejor_jugada,
    env = env
  ))
}