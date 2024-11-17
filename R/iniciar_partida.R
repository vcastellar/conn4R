iniciar_partida(profundidad = 6) {
  tablero <- reiniciar_tablero()
  i <- 1
  while (i <= 42) {
    # introducir jugada del jugador
    tablero <- turno_humano(tablero)
    p <- visualizar_tablero(tablero)
    print(p)

    if (juego_terminado(tablero)) {
      cat("gana humano")
      break
    }
    i <- i + 1

    # introducir jugada de la IA
    mejor_jugada_IA <- mejor_jugada_minmax(tablero, 10, TRUE)
    tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
    p <- visualizar_tablero(tablero)
    print(p)

    if (juego_terminado(tablero)) {
      cat("gana IA")
      break
    }
    i <- i + 1
  }
  cat("empate")
}


