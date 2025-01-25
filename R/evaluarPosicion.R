#' evaluación estática de una posición representada en tablero
#'
#' @description evalua una posición mediante criterios estáticos basados en el
#' número de casillas conectadas del jugador en turno
#' @param tablero matriz que representa el estado de un tablero
#' @param turno qué jugador comienza pa partida: 1 para humano, 2 para IA.
#' @examples
#' tablero <- crear_posicion_aleatoria(5)
#' visualizar_tablero(tablero)
#' evaluar_posicion(tablero)
#' .evaluar_turno(tablero, 1)
#' .evaluar_turno(tablero, 2)



bitboards <- matrix(c(
  0.0, 0.5, 1.0, 1.5, 1.0, 0.5, 0.0,
  0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5,
  1.0, 1.5, 2.0, 2.5, 2.0, 1.5, 1.0,
  1.5, 2.0, 2.5, 3.0, 2.5, 2.0, 1.5,
  2.0, 2.5, 3.0, 3.5, 3.0, 2.5, 2.0,
  0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5
), nrow = 6, byrow = TRUE)


# constantes
punt1 <- 1
punt2 <- 10
punt3 <- 100
punt4 <- 100000

# función principal
evaluar_posicion <- function(tablero) {
  puntBit <- sum(bitboards * (1 * (tablero == 2))) - sum(bitboards * (1 * (tablero == 1)))
  eval_turno <- .evaluar_turno(tablero, 2) + .evaluar_turno(tablero, 1)
  return(eval_turno + puntBit)
}


#función evaluar turno
.evaluar_turno <- function(tablero, turno) {
  puntuacion <- 0

  # Evaluar líneas horizontales
  for (fila in 1:6) {
    for (columna in 1:4) {
      linea <- tablero[fila, columna:(columna + 3)]
      attr(linea, "ini") <- c(fila, columna)
      attr(linea, "fin") <- c(fila, columna + 3)
      attr(linea, "dir") <- "h"
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas verticales
  for (columna in 1:7) {
    for (fila in 1:3) {
      linea <- tablero[fila:(fila + 3), columna]
      attr(linea, "ini") <- c(fila, columna)
      attr(linea, "fin") <- c(fila + 3, columna)
      attr(linea, "dir") <- "v"
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de izquierda a derecha)
  for (fila in 1:3) {
    for (columna in 1:4) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna + 1],
                 tablero[fila + 2, columna + 2], tablero[fila + 3, columna + 3])
      attr(linea, "ini") <- c(fila, columna)
      attr(linea, "fin") <- c(fila + 3, columna + 3)
      attr(linea, "dir") <- "did"
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  # Evaluar líneas diagonales (de derecha a izquierda)
  for (fila in 1:3) {
    for (columna in 4:7) {
      linea <- c(tablero[fila, columna], tablero[fila + 1, columna - 1],
                 tablero[fila + 2, columna - 2], tablero[fila + 3, columna - 3])
      attr(linea, "ini") <- c(fila, columna)
      attr(linea, "fin") <- c(fila + 3, columna - 3)
      attr(linea, "dir") <- "ddi"
      puntuacion <- puntuacion + .evaluar_linea(linea, turno)
    }
  }

  return(puntuacion)
}



# función evaluar linea
.evaluar_linea <- function(linea, turno) {
  puntuacion <- 0

  if (all(linea == turno)) {
    puntuacion <- punt4 # Línea completa del jugador actual

  } else if (sum(linea == turno) == 3 && sum(linea == 0) == 1) {
    puntuacion <- punt3 # Tres fichas en línea con una casilla vacía

  } else if (sum(linea == turno) == 2 && sum(linea == 0) == 2) {
    puntuacion <- punt2 # Dos fichas en línea con dos casillas vacías


  } else if (sum(linea == turno) == 1 && sum(linea == 0) == 3) {
    puntuacion <- punt1  # Una ficha con tres casillas vacias
  }

  puntuacion <- (-1)^(turno) * puntuacion

  return(puntuacion)
}



# #función evaluar turno
# .evaluar_turno <- function(tablero, turno) {
#   puntuacion <- 0
#   
#   # Evaluar líneas horizontales
#   for (fila in 1:6) {
#     for (columna in 1:4) {
#       linea <- tablero[fila, columna:(columna + 3)]
#       puntuacion <- puntuacion + .evaluar_linea(linea, turno)
#     }
#   }
#   
#   # Evaluar líneas verticales
#   for (columna in 1:7) {
#     for (fila in 1:3) {
#       linea <- tablero[fila:(fila + 3), columna]
#       puntuacion <- puntuacion + .evaluar_linea(linea, turno)
#     }
#   }
#   
#   # Evaluar líneas diagonales (de izquierda a derecha)
#   for (fila in 1:3) {
#     for (columna in 1:4) {
#       linea <- c(tablero[fila, columna], tablero[fila + 1, columna + 1],
#                  tablero[fila + 2, columna + 2], tablero[fila + 3, columna + 3])
#       puntuacion <- puntuacion + .evaluar_linea(linea, turno)
#     }
#   }
#   
#   # Evaluar líneas diagonales (de derecha a izquierda)
#   for (fila in 1:3) {
#     for (columna in 4:7) {
#       linea <- c(tablero[fila, columna], tablero[fila + 1, columna - 1],
#                  tablero[fila + 2, columna - 2], tablero[fila + 3, columna - 3])
#       puntuacion <- puntuacion + .evaluar_linea(linea, turno)
#     }
#   }
#   
#   return(puntuacion)
# }
# 
# # función evaluar linea
# .evaluar_linea <- function(linea, turno) {
#   puntuacion <- 0
#   
#   if (all(linea == turno)) {
#     puntuacion <- punt4 # Línea completa del jugador actual
#     
#   } else if (sum(linea == turno) == 3 && sum(linea == 0) == 1) {
#     puntuacion <- punt3 # Tres fichas en línea con una casilla vacía
#     
#   } else if (sum(linea == turno) == 2 && sum(linea == 0) == 2) {
#     puntuacion <- punt2 # Dos fichas en línea con dos casillas vacías
#     
#   } else if (sum(linea == turno) == 1 && sum(linea == 0) == 3) {
#     puntuacion <- punt1 # Una ficha en línea con tres casillas vacías
#   }
#   
#   puntuacion <- (-1)^(turno) * puntuacion
#   
#   return(puntuacion)
# }


#------------------------------------------------------------------------------
# intento de optimización de la función evaluar posicion
# NO MEJORA
#------------------------------------------------------------------------------

# # Precomputar índices de diagonales (de izquierda a derecha)
# .list_izq_der <- list()
# .diag_izq_der <- matrix(0, 4, 2)
# for (fila in 1:3) {
#   for (columna in 1:4) {
#     for (i in 0:3) {
#       .diag_izq_der[i + 1,] <- rbind(c(fila + i, columna + i))
#     }
#     .list_izq_der <- c(.list_izq_der, list(.diag_izq_der))
#   }
# }
# 
# # Precomputar índices de diagonales (de derecha a izqauierda)
# .list_der_izq <- list()
# .diag_der_izq <- matrix(0, 4, 2)
# for (fila in 1:3) {
#   for (columna in 4:7) {
#     for (i in 0:3) {
#       .diag_der_izq[i + 1,] <- rbind(c(fila + i, columna - i))
#     }
#     .list_der_izq <- c(.list_der_izq, list(.diag_der_izq))
#   }
# }

# # función principal
# evaluar_posicion <- function(tablero) {
#   puntBit <- sum(bitboards * (tablero == 2)) - sum(bitboards * (tablero == 1))
#   return(.evaluar_turno(tablero, 1) + .evaluar_turno(tablero, 2) + puntBit)
# }
# 
# 
# # funcion evaluar turno
# .evaluar_turno <- function(tablero, turno) {
#   puntuacion <- 0
# 
# 
#   # Evaluar líneas horizontales
#   horizontales <- apply(tablero, 1, function(fila) {
#     sapply(1:4, function(columna) {
#       .evaluar_linea(fila[columna:(columna + 3)], turno)
#     })
#   })
#   puntuacion <- puntuacion + sum(horizontales)
# 
# 
#   # Evaluar líneas verticales
#   verticales <- apply(tablero, 2, function(columna) {
#     sapply(1:3, function(fila) {
#       .evaluar_linea(columna[fila:(fila + 3)], turno)
#     })
#   })
#   puntuacion <- puntuacion + sum(verticales)
# 
# 
#   # Evaluar líneas diagonales (de izquierda a derecha)
#   diag_izq_der <- sapply(.list_izq_der, function(linea) {
#     .evaluar_linea(tablero[linea], turno)
#   })
#   puntuacion <- puntuacion + sum(diag_izq_der)
# 
# 
#   # Evaluar líneas diagonales (de derecha a izquierda)
#   diag_der_izq <- sapply(.list_der_izq, function(linea) {
#     .evaluar_linea(tablero[linea], turno)
#   })
#   puntuacion <- puntuacion + sum(diag_der_izq)
# 
#   return(puntuacion)
# }

# # función evaluar_linea 
# .evaluar_linea <- function(linea, turno) {
#   # Calcular conteos una sola vez
#   count_turno <- sum(linea == turno)
#   count_vacios <- sum(linea == 0)
# 
#   # Evaluar según los conteos
#   puntuacion <- switch(
#     paste(count_turno, count_vacios, sep = "-"),
#     "4-0" = 100000, # Línea completa del jugador actual
#     "3-1" = 100,    # Tres fichas y una vacía
#     "2-2" = 10,     # Dos fichas y dos vacías
#     "1-3" = 1,      # Una ficha y tres vacías
#     0                # Caso por defecto: sin puntuación
#   )
# 
#   # Ajustar el signo según el turno
#   return((-1)^(turno) * puntuacion)
# }

