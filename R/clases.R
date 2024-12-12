setClass("arbol",
         slots = c(turno       = "logical",
                   jugada      = "integer",
                   profundidad = "integer",
                   idNodo      = "integer"),
         prototype = list(
           turno       = c(),
           jugada      = c(),
           profundidad = c(),
           idNodo      = c()
           )
         )

setGeneric("actualizar", function(x, ...) standardGeneric("actualizar"))


setClass("info", 
         slots = c(numNodos = "integer",
                   arbol = "arbol"
                   ),
         prototype = list(numNodos = 0L,
                          arbol = new("arbol"))
         )

info <- new("info")
str(info)

# setMethod("inicializar", "arbol",
#           function(.Object))
setMethod("actualizar", "arbol", 
          function(x = arbol, turno, jugada, profundidad) {
            x@turno       <- c(x@turno, turno)
            x@jugada      <- c(x@jugada, jugada)
            x@profundidad <- c(x@profundidad, profundidad)
            x@idNodo      <- c(x@idNodo, ifelse(is.null(x@idNodo[1]), 0L,  max(x@idNodo)) + 1L)
            
            return(x)
            }
          )

