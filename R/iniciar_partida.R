#' Iniciar una partida de conecta 4
#'
#' @description inicia una partida de conecta 4. Se especifica la profundidad
#'              de búsqueda y el jugador que inicia la partida
#' @param profundidad profundidad de búsqueda del algoritmo minimax.
#' @param turno qué jugador comienza la partida: 1 para humano, 2 para IA.
#' @examples
#' lo siguiente inicia una partida en la que  el jugador humano es el primero en jugar
#' iniciar_partida(profundidad = 7, turno = 2)
#'
#' para que sea la IA quien realice la primera jugada:
#' iniciar_partida(autoplay = TRUE, profundidad = 5)



iniciar_partida <- function(profundidad = 5, turno = 1, profAdaptative = TRUE, autoplay = FALSE) {
  resultado <- "DRAW"
  tablero <- reiniciar_tablero()
  p <- visualizar_tablero(tablero)
  print(p)
  puntuacion <- 0
  i <- 1
  j <- 0
  
  if (!autoplay) {
    if (turno == 2) {
      # introducir jugada de la IA
      mejor_jugada_IA <- minimax(tablero, profundidad, maximizandoIA = TRUE)
      
      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
      p <- visualizar_tablero(tablero)
      print(p)
      print(paste0("valoracion IA: ", evaluar_posicion(tablero)))
      print(paste0("jugada realizada: ", mejor_jugada_IA$jugada))
      
      
      if (juego_terminado(tablero)$finalizado) {
        resultado <- "gana IA"
        break
      }
      i <- i + 1
      j <- 1
      
    }
    
    while (i <= (42 - j)) {
      
      # introducir jugada del jugador
      #--------------------------------------------------------------------------
      tablero <- turno_humano(tablero)
      p <- visualizar_tablero(tablero)
      print(p)
      
      print(paste0("valoracion HU: ", evaluar_posicion(tablero)))
      
      
      if (juego_terminado(tablero)$finalizado) {
        resultado <- juego_terminado(tablero)$resultado
        break
      }
      i <- i + 1
      
      
      # introducir jugada de la IA
      #---------------------------------------------------------------------------
      if (profAdaptative) {
        prof <- .adaptativa(tablero, profundidad_base = profundidad)
      } else {
        prof <- profundidad
      }
      
      tik <- system.time({
        mejor_jugada_IA <- minimax(tablero, prof, maximizandoIA = TRUE)
      })
      
      
      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
      p <- visualizar_tablero(tablero)
      print(p)
      
      cat("\n")
      print("-----------------------------------------------------------------")
      print(paste0("valoracion IA:    ", mejor_jugada_IA$puntuacion))
      print(paste0("jugada realizada: ", mejor_jugada_IA$jugada))
      print(paste0("profundidad:      ", prof))
      print(paste0("num. nodos:       ", mejor_jugada_IA$env$nodos))
      print(paste0("tiempo:           ", round(tik[[3]], 2)))
      print("-----------------------------------------------------------------")
      cat("\n")
      
      
      
      if (juego_terminado(tablero)$finalizado) {
        resultado <- juego_terminado(tablero)$resultado
        break
      }
      
      i <- i + 1
    }
    
    cat(resultado)
  } 
  
  if(autoplay) {
    while (i <= 42) {
    # introducir jugada de la IA
      turno <- ((i-1) %% 2) + 1 
      
      if (profAdaptative) {
        prof <- .adaptativa(tablero, profundidad_base = profundidad)
      } else {
        prof <- profundidad
      }
      
      tik <- system.time({
        mejor_jugada_IA <- minimax(tablero, prof, maximizandoIA = TRUE)
      })
      
      tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, turno)
      
      p <- visualizar_tablero(tablero)
      print(p)
      
      cat("\n")
      print("-----------------------------------------------------------------")
      print(paste0("valoracion IA:    ", mejor_jugada_IA$puntuacion))
      print(paste0("jugada realizada: ", mejor_jugada_IA$jugada))
      print(paste0("profundidad:      ", prof))
      print(paste0("num. nodos:       ", mejor_jugada_IA$env$nodos))
      print(paste0("tiempo:           ", round(tik[[3]], 2)))
      print("-----------------------------------------------------------------")
      cat("\n")
      
      
      if (juego_terminado(tablero)$finalizado) {
        resultado <- "gana IA"
        break
      }
      i <- i + 1
    }
  }
  
  return(tablero)

}


