#' solicita una jugada al humano.
#'
#' @description la función invita al jugador humano a introducir una jugada
#' Si la jugada es legal representeala jugada del humano en el tablero mediante
#' un 1 en la casilla que corresponda. La jugada de la IA será representada en el
#' tablero mediante un 2.
#' @param tablero matriz 6 x 7 que representa la situación del tablero de juego.
#' @param jugada un entero del 1 al 7 correspondiente a cada una de las 7 columnas
#' si jugada = NULL (valor por defecto), entonces invita al jugador a introducir su jugada
#' @param raton Lógico. Si \code{TRUE} (valor por defecto) y el dispositivo
#'   gráfico activo lo permite, la jugada se elige pulsando con el ratón sobre
#'   cualquier celda de la columna deseada en el tablero dibujado por
#'   \code{\link{visualizar_tablero}}. Si \code{FALSE}, o si el dispositivo no
#'   admite clics, la columna se escribe por teclado.
#' @details
#'# Si la jugada es legal representa la jugada del humano en el tablero mediante un 1
#' en la casilla que corresponda a la jugada
#' Si la jugada introducida es ilegal, vuelve a solicitar una jugada legal
#'
#' Al jugar con el ratón, pulsar fuera del tablero o sobre una columna llena
#' vuelve a solicitar la jugada. Si se cancela la selección (tecla Esc o botón
#' derecho, según el dispositivo), la jugada se pide por teclado.
#'
#'
#' @examples
#' tablero <- reiniciar_tablero()
#' tablero <- turno_humano(tablero, jugada = 4)
#' visualizar_tablero(tablero)
#' # tambien se puede hacer lo siguiente (no ejecutar):
#' # tablero <- reiniciar_tablero()
#' # print(visualizar_tablero(tablero))
#' # tablero <- turno_humano(tablero)  # pulsar sobre una columna del tablero
#' # visualizar_tablero(tablero)



#' @export
turno_humano <- function(tablero, jugada = NULL, raton = TRUE) {

  jugadas_posibles <- jugadas_disponibles(tablero)

  if (length(jugadas_posibles) == 0) {
    return(tablero) # No hay jugadas disponibles
  }

  usar_raton <- is.null(jugada) && isTRUE(raton) && .admite_clics()

  repeat {

    if (!is.null(jugada)) {
      jugada_humano <- jugada
    } else if (usar_raton) {
      cat("Tu turno. Pulsa sobre una columna del tablero.\n")
      flush.console()
      jugada_humano <- .leer_jugada_raton()
      if (is.null(jugada_humano)) {
        # Selección cancelada o el clic no pudo leerse: seguir por teclado.
        usar_raton <- FALSE
        next
      }
    } else {
      cat("Tu turno. Elige una columna (1-7): ")
      entrada <- readLines(con = stdin(), n = 1)
      if (length(entrada) == 0L) {
        stop("No se pudo leer la jugada: la entrada estándar está cerrada.",
             call. = FALSE)
      }
      jugada_humano <- suppressWarnings(as.integer(entrada))
      # jugada_humano <- as.integer(readline())
    }

    if (length(jugada_humano) == 1L && jugada_humano %in% jugadas_posibles) {
      return(realizar_jugada(tablero, jugada_humano, 1L))
    } else {
      cat("Jugada no válida. Intenta de nuevo.\n")
    }
  }
}

# Indica si el dispositivo gráfico activo puede devolver la posición de un clic.
.admite_clics <- function() {
  if (!interactive() || grDevices::dev.cur() == 1L) {
    return(FALSE)
  }
  capacidad <- tryCatch(
    grDevices::dev.capabilities("locator")$locator,
    error = function(e) NA
  )
  # Algunos dispositivos (por ejemplo el de RStudio) no informan de esta
  # capacidad aunque la tengan; solo se descarta si la niegan expresamente.
  !isFALSE(capacidad)
}

# Espera un clic sobre el tablero dibujado y devuelve la columna pulsada
# (NA si el clic cae fuera del tablero) o NULL si se cancela la selección.
.leer_jugada_raton <- function() {
  panel <- grep("^panel\\.", .nombres_viewports(grid::current.vpTree()),
                value = TRUE)
  if (length(panel) == 0L) {
    return(NULL) # No hay ningún tablero dibujado en el dispositivo.
  }

  grid::seekViewport(panel[[1]])
  on.exit(grid::upViewport(0), add = TRUE)

  clic <- tryCatch(grid::grid.locator(unit = "npc"), error = function(e) NULL)
  if (is.null(clic)) {
    return(NULL)
  }

  .columna_desde_npc(as.numeric(clic$x), as.numeric(clic$y))
}

# Nombres de todos los viewports de un árbol de grid (el gráfico de ggplot2
# dibuja su panel en un viewport llamado "panel.<fila>-<col>-...").
.nombres_viewports <- function(arbol) {
  if (inherits(arbol, "vpTree")) {
    c(arbol$parent$name, .nombres_viewports(arbol$children))
  } else if (inherits(arbol, c("vpList", "vpStack"))) {
    unlist(lapply(arbol, .nombres_viewports), use.names = FALSE)
  } else {
    arbol$name
  }
}

# Convierte una posición en coordenadas relativas al panel del gráfico (0-1)
# en la columna del tablero que se encuentra debajo, o NA si queda fuera.
.columna_desde_npc <- function(x, y) {
  lim <- .limites_tablero
  x_datos <- lim$x[[1]] + x * diff(lim$x)
  y_datos <- lim$y[[1]] + y * diff(lim$y)

  if (x_datos < 0.5 || x_datos >= 7.5 || y_datos < 0.5 || y_datos > lim$y[[2]]) {
    return(NA_integer_)
  }
  as.integer(floor(x_datos + 0.5))
}
