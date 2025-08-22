#-------------------------------------------------------------------------------
# tests
#-------------------------------------------------------------------------------

tablero <- matrix(c(0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 2, 0, 0,
                    0, 2, 1, 2, 1, 0, 0,
                    0, 1, 1, 1, 2, 2, 0,
                    0, 2, 2, 2, 1, 1, 0), byrow = TRUE, ncol = 7)

visualizar_tablero(tablero)
kk <- minimax(tablero = tablero, profundidad = 5, maximizandoIA = FALSE)
kk <- minimax(tablero = tablero, profundidad = 5, maximizandoIA = TRUE)


#-------------------------------------------------------------------------------
tablero <- matrix(c(0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 2, 0, 0,
                    0, 2, 2, 1, 1, 0, 0,
                    0, 1, 1, 2, 2, 2, 0,
                    0, 2, 1, 2, 1, 1, 0), byrow = TRUE, ncol = 7)

visualizar_tablero(tablero)
system.time(
  kk <- minimax(tablero = tablero, profundidad = 5, maximizandoIA = TRUE)
)

system.time(
  kk <- minimax(tablero = tablero, profundidad = 5, maximizandoIA = TRUE)
)
mostrar_mejor_variante(tablero, encontrar_mejor_variante(kk$env$arbol))
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
tablero <- matrix(c(0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 2, 1, 2, 0,
                    0, 0, 0, 1, 2, 2, 0,
                    0, 0, 2, 2, 1, 2, 0,
                    0, 0, 1, 1, 2, 1, 1), byrow = TRUE, ncol = 7)
visualizar_tablero(tablero)
kk <- minimax(tablero = tablero, profundidad = 7, maximizandoIA = FALSE)


#-------------------------------------------------------------------------------
tablero <- matrix(c(0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 2, 1, 2, 0,
                    0, 0, 0, 1, 2, 2, 0,
                    0, 0, 2, 2, 1, 2, 0,
                    0, 0, 1, 1, 2, 1, 1), byrow = TRUE, ncol = 7)
visualizar_tablero(tablero)
realizar_jugada_cpp(tablero, 6, 1)
