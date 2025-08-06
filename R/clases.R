setClass("arbol",
         slots = c(
           idNodo      = "integer",
           idPadre     = "integer",
           jugada      = "integer",
           turno       = "logical",
           profundidad = "integer",
           puntuacion  = "numeric"
         ),
         prototype = list(
           idNodo      = integer(),
           idPadre     = integer(),
           jugada      = integer(),
           turno       = logical(),
           profundidad = integer(),
           puntuacion  = numeric()
         )
)


setGeneric("actualizar", function(x, ...) standardGeneric("actualizar"))

setMethod("actualizar", "arbol", 
          function(x, 
                   idPadre    = NA_integer_,  
                   turno       = NULL,
                   jugada      = NULL,
                   profundidad = NULL,
                   puntuacion  = NULL) {
            
            # Asegúrate de que 'x' es de clase 'arbol'
            stopifnot(isS4(x), is(x, "arbol"))
            
            nuevo_id <- ifelse(length(x@idNodo) == 0, 0L, max(x@idNodo)) + 1L
            
            x@idNodo      <- c(x@idNodo, as.integer(nuevo_id))
            x@idPadre     <- c(x@idPadre, as.integer(idPadre))
            x@turno       <- c(x@turno, as.logical(turno))
            x@jugada      <- c(x@jugada, as.integer(jugada))
            x@profundidad <- c(x@profundidad, as.integer(profundidad))
            x@puntuacion  <- c(x@puntuacion, as.numeric(puntuacion))
            
            return(x)  # ✅ Devuelve objeto S4, no lista
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


encontrar_mejor_variante <- function(arbol) {
  
  # Encontrar nodo raíz (idPadre == NA o NA_integer_)
  nodo_actual <- which(is.na(arbol@idPadre))[1]
  if (is.na(nodo_actual)) stop("No se encontró nodo raíz")
  
  mejor_variante <- list()
  
  repeat {
    # Añadir información del nodo actual
    mejor_variante <- c(mejor_variante, list(list(
      nodo       = arbol@idNodo[nodo_actual],
      jugada     = arbol@jugada[nodo_actual],
      turno      = arbol@turno[nodo_actual],
      puntuacion = arbol@puntuacion[nodo_actual]
    )))
    
    # Encontrar hijos de nodo_actual
    hijos <- which(arbol@idPadre == arbol@idNodo[nodo_actual])
    
    # Si no tiene hijos, es hoja, termina
    if (length(hijos) == 0) break
    
    # Elegir mejor hijo según turno (max busca mayor, min busca menor)
    if (arbol@turno[nodo_actual]) {
      # Turno MAX: elige hijo con mayor puntuación
      mejor_idx <- which.max(arbol@puntuacion[hijos])
    } else {
      # Turno MIN: elige hijo con menor puntuación
      mejor_idx <- which.min(arbol@puntuacion[hijos])
    }
    
    nodo_actual <- hijos[mejor_idx]
  }
  
  return(mejor_variante)
}

mostrar_mejor_variante <- function(tablero_inicial, variante, pausa_ms = 1000) {
  tablero <- tablero_inicial
  
  for (paso in variante) {
    jugada <- paso$jugada
    turno <- ifelse(paso$turno, 2, 1)  # TRUE = IA = 2, FALSE = jugador = 1
    
    if (!is.na(jugada)) {
      tablero <- realizar_jugada(tablero, jugada, turno)
    }
    
    print(visualizar_tablero(tablero))
    
    Sys.sleep(pausa_ms / 1000)  # convertir milisegundos a segundos
  }
}