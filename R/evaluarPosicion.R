# función que dado el turno del humano o de la IA evalue la posición en el tablero
# en función de las lineas horizaontales, verticales y diagonales de longitud 4.
# Cuantas mas fichas existan conectadas en linea del mismo jugador sin que existan 
# fichas intermedias del otro jugador, mayor puntuacion
evaluar_posicion <- function(tablero, turno) {
  puntuacion <- 0

  # Evaluar líneas horizontales
  for (fila in 1:6) {
    for (columna in 1:4) {
      linea <- tablero[fila, columna:(columna + 3)]
      puntuacion <- puntuacion + evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas verticales
  for (columna in 1:7) {
    for (fila in 1:3) {
      linea <- tablero[fila:(fila + 3), columna]
      puntuacion <- puntuacion + evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de izquierda a derecha)
  for (fila in 1:3) {
    for (columna in 1:4) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna + 1],
                 tablero[fila + 2, columna + 2], tablero[fila + 3, columna + 3])
      puntuacion <- puntuacion + evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de derecha a izquierda)
  for (fila in 1:3) {
    for (columna in 4:7) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna - 1],
                 tablero[fila + 2, columna - 2], tablero[fila + 3, columna - 3])
      puntuacion <- puntuacion + evaluar_linea(linea, turno)
    }
  }

  return(puntuacion)
}

# prompt: crea la función evaluar_linea necesaria para que funcióne la función anterior

evaluar_linea <- function(linea, turno) {
  puntuacion <- 0

  if (all(linea == turno)) {
    puntuacion <- 100000 # Línea completa del jugador actual
  } else if (sum(linea == turno) == 3 && sum(linea == 0) == 1) {
    puntuacion <- 5 # Tres fichas en línea con una casilla vacía
  } else if (sum(linea == turno) == 2 && sum(linea == 0) == 2) {
    puntuacion <- 2 # Dos fichas en línea con dos casillas vacías
  }

  return(puntuacion)
}
