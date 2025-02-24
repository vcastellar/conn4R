setClass("arbol",
         slots = c(idNodo      = "integer",
                   jugada      = "integer",
                   turno       = "logical",
                   profundidad = "integer",
                   puntuacion  = "numeric"),
         prototype = list(
           idNodo      = c(),
           jugada      = c(),
           turno       = c(),
           profundidad = c(),
           puntuacion  = c()
           )
         )

setGeneric("actualizar", function(x, ...) standardGeneric("actualizar"))


# setMethod("inicializar", "arbol",
#           function(.Object))

setMethod("actualizar", "arbol", 
          function(x = arbol, 
                   turno       = NULL,
                   jugada      = NULL,
                   profundidad = NULL,
                   puntuacion  = NULL) {
            x@turno       <- c(x@turno, turno)
            x@jugada      <- c(x@jugada, jugada)
            x@profundidad <- c(x@profundidad, profundidad)
            x@idNodo      <- c(x@idNodo, ifelse(is.null(x@idNodo[1]), 0L,  max(x@idNodo)) + 1L)
            x@puntuacion  <- c(x@puntuacion, puntuacion)
            
            return(x)
            }
          )




# # Definir el método específico para la clase arbol
setGeneric("actUltNodo", function(obj, ...) standardGeneric("actUltNodo"))

setMethod("actUltNodo", "arbol", 
          function(obj, 
                   slotName, 
                   value) {
          l <- length(slot(obj, slotName))
          slot(obj, slotName)[l] <- value
          return(obj)
  })



# Suponiendo que el objeto es una lista llamada `arbol`
encontrar_mejor_variante <- function(arbol) {

  # Inicializar la mejor variante
  mejor_variante <- list()

  # Identificar el nodo raíz
  nodo_actual <- which(arbol@profundidad == max(arbol@profundidad))[1] # Nodo raíz más profundo

  while (arbol@profundidad[nodo_actual] > 1) {
    # Agregar jugada actual a la mejor variante
    mejor_variante <- c(list(list(
      nodo       = arbol@idNodo[nodo_actual],
      jugada     = arbol@jugada[nodo_actual],
      turno      = arbol@turno[nodo_actual],
      puntuacion = arbol@puntuacion[nodo_actual]
    )), mejor_variante)

    # Filtrar nodos hijos del actual
    hijos <- which(arbol@idNodo > arbol@idNodo[nodo_actual] &
                     arbol@profundidad == (arbol@profundidad[nodo_actual] - 1))

    # Elegir el mejor hijo según el turno
    if (arbol@turno[nodo_actual]) {
      # Turno del jugador (MAX): buscar mayor puntuación
      nodo_actual <- hijos[which.min(arbol@puntuacion[hijos])]
    } else {
      # Turno del oponente (MIN): buscar menor puntuación
      nodo_actual <- hijos[which.max(arbol@puntuacion[hijos])]
    }
  }

  # Agregar el nodo terminal
  mejor_variante <- c(list(list(
    nodo       = arbol@idNodo[nodo_actual],
    jugada     = arbol@jugada[nodo_actual],
    turno      = arbol@turno[nodo_actual],
    puntuacion = arbol@puntuacion[nodo_actual]
  )), mejor_variante)

  return(mejor_variante)
}

# # Llamar a la función y mostrar el resultado
# mejor_variante <- encontrar_mejor_variante(arbol)
# print(mejor_variante)
# 
# tablero_aux <- tablero
# for (i in length(mejor_variante):1) {
# 
#   tablero_aux <- realizar_jugada(tablero_aux,
#                                  columna = mejor_variante[[i]]$jugada,
#                                  jugador = ifelse(mejor_variante[[i]]$turno == TRUE, 2, 1)
#   )
#   p <- visualizar_tablero(tablero_aux)
#   print(p)
#   Sys.sleep(3)
# 
# }
# 
# mostrar_variante <- function(mejor_variante) {
#   for (i in seq_along(mejor_variante)) {
#     nodo <- mejor_variante[[i]]
#     cat(sprintf("Paso %d: Nodo %d, Jugada %d, Turno %s, Puntuación %.1f\n",
#                 i, nodo$nodo, nodo$jugada, ifelse(nodo$turno, "MAX", "MIN"), nodo$puntuacion))
#   }
# }
# 
# # Mostrar la variante
# # mostrar_variante(mejor_variante)
# 



