iniciar_partida <- function(profundidad = 5) {
  resultado <- "tablas"
  tablero <- reiniciar_tablero()
  p <- visualizar_tablero(tablero)
  print(p)
  puntuacion <- 0
  i <- 1
  while (i <= 42) {
    # introducir jugada del jugador
    tablero <- turno_humano(tablero)
    p <- visualizar_tablero(tablero)
    print(p)
    print(paste0("valoracion HU: ", evaluar_posicion(tablero, 1)))

    if (juego_terminado(tablero)) {
      resultado <- "gana humano"
      break
    }
    i <- i + 1

    # introducir jugada de la IA
    mejor_jugada_IA <- mejor_jugada_minmax(tablero, profundidad, maximizando_jugador = TRUE)
    tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
    tablero_aux <<- tablero
    p <- visualizar_tablero(tablero)
    print(p)
    print(paste0("valoracion IA: ", evaluar_posicion(tablero, 2)))
    print(paste0("jugada realizada: ", mejor_jugada_IA$jugada))


    if (juego_terminado(tablero)) {
      resultado <- "gana IA"
      break
    }
    i <- i + 1
  }
  cat(resultado)
}


