tablero <- reiniciar_tablero()
for (i in 1:42) {
  # introducir jugada del jugador
  tablero <- turno_humano(tablero)
  cat("\014")
  print(tablero)
  visualizar_tablero(tablero)

  if (juego_terminado(tablero)) {
    cat("gana humano")
    break
  }


  # introducir jugada de la IA
  mejor_jugada_IA <- mejor_jugada_minmax(tablero, 6, TRUE)
  tablero <- realizar_jugada(tablero, mejor_jugada_IA$jugada, 2)
  print(tablero)
  visualizar_tablero(tablero)

}
