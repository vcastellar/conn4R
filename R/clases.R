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
          function(x = arbol, turno, jugada, profundidad, puntuacion) {
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

setMethod("actUltNodo", "arbol", function(obj, slotName, value) {
  l <- length(slot(obj, slotName))
  slot(obj, slotName)[l] <- value
  return(obj)
  })




