iniciar_partida <- function(profundidad = 6) {
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
    # cat("\014")
    cat(paste0("puntuación IA:", mejor_jugada_IA$puntuacion, "\n"))
    cat(paste0("jugada IA:", mejor_jugada_IA$jugada, "\n"))

    if (juego_terminado(tablero)) {
      resultado <- "gana IA"
      break
    }
    i <- i + 1
  }
  cat(resultado)
}


