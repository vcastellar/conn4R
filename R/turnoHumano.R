# prompt: crea una funcioón que solicite una jugada al humano.
# Si la jugada es legal (está dentro de las jugadas disponibles calculadas con la anterior función)
# represente la jugada del humano en el tablero mediante un 1 en la casilla que corresponda.
# La jugada de la IA será representada en el tablero mediante un -1.
# Del mismo modo el turno del humano será representado por 1 y el turno de la IA por -1

turno_humano <- function(tablero, jugada) {
  jugadas_posibles <- jugadas_disponibles(tablero)

  if (length(jugadas_posibles) == 0) {
    return(tablero) # No hay jugadas disponibles
  }

  repeat {
    print("Tu turno. Elige una columna (1-7): ")
    jugada_humano <- as.integer(readline())

    if (jugada_humano %in% jugadas_posibles) {
      # Encuentra la primera fila vacía en la columna elegida
      for (fila in 6:1) {
        if (tablero[fila, jugada_humano] == 0) {
          tablero[fila, jugada_humano] <- 1  # Representa la jugada del humano
          return(tablero)
        }
      }
      break
    } else {
      cat("Jugada no válida. Intenta de nuevo.\n")
      return(tablero)
    }
  }
}
