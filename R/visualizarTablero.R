#' Detectar y devolver las coordenadas de la línea ganadora
#'
#' @description Recorre todas las posibles líneas de 4 del tablero y devuelve
#'   las coordenadas de visualización de la primera línea ganadora encontrada
#'   (puntuación ≥ 10 000), o \code{NULL} si no existe ninguna. Usada
#'   internamente por \code{\link{visualizar_tablero}} para resaltar en rojo
#'   las fichas ganadoras.
#'
#' @param tablero Matriz 6×7 con el estado del tablero.
#'
#' @return Data frame con columnas \code{x}, \code{y} y \code{ficha} (valor 3)
#'   para las 4 celdas de la línea ganadora, o \code{NULL} si no hay ganador.
.detectar_lineas <- function(tablero) {
  direcciones <- rbind(c(0L, 1L), c(1L, 0L), c(1L, 1L), c(1L, -1L))

  for (fila in seq_len(6L)) {
    for (columna in seq_len(7L)) {
      jugador <- tablero[fila, columna]
      if (jugador == 0L) next

      for (i in seq_len(nrow(direcciones))) {
        filas <- fila + (0:3) * direcciones[i, 1]
        columnas <- columna + (0:3) * direcciones[i, 2]
        if (any(filas < 1L | filas > 6L | columnas < 1L | columnas > 7L)) next

        if (all(tablero[cbind(filas, columnas)] == jugador)) {
          return(data.frame(x = 7L - filas, y = columnas, ficha = 3L))
        }
      }
    }
  }

  NULL
}

#' Visualizar el tablero de Conecta 4
#'
#' @description Genera un gráfico \pkg{ggplot2} del estado actual del tablero.
#'   Las fichas del jugador 1 (humano) se muestran en color \emph{papayawhip}
#'   (crema), las del jugador 2 (IA) en gris oscuro y las casillas vacías en
#'   blanco. Si existe una línea ganadora, sus cuatro celdas se resaltan en
#'   rojo.
#'
#' @param tablero Matriz 6×7 con el estado del tablero. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#'
#' @return Objeto \code{ggplot} listo para imprimir con \code{print()} o
#'   \code{plot()}.
#'
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' p <- visualizar_tablero(tablero)
#' print(p)
#'
#' # Tablero vacío
#' print(visualizar_tablero(reiniciar_tablero()))
#'
#' @seealso \code{\link{crear_posicion_aleatoria}}, \code{\link{reiniciar_tablero}}
#'
#' @export
visualizar_tablero <- function(tablero) {
  library(ggplot2)

  df_fichas <- data.frame(
    x     = rev(rep(1:7, each = 6)),
    y     = rep(1:6, times = 7),
    ficha = factor(rev(as.vector(tablero)), levels = c(0, 1, 2, 3))
  )

  df_lineas_h <- data.frame(
    xini = rep(1, 7) - .5,
    xend = rep(7, 7) + .5,
    yini = (1:7) - .5,
    yend = (1:7) - .5
  )

  df_lineas_v <- data.frame(
    yini = rep(1, 8) - .5,
    yend = rep(7, 8) - .5,
    xini = (1:8) - .5,
    xend = (1:8) - .5
  )

  df_columnas_labels <- data.frame(
    x      = 1:7,
    y      = 7,
    labels = paste0("col.", 1:7)
  )

  df_fichas <- transform(df_fichas,
                         x_centro = x,
                         y_centro = y,
                         alpha    = ifelse(ficha == 0, 0, 1))

  linea <- .detectar_lineas(tablero)

  if (!is.null(linea)) {
    for (k in 1:4) {
      r <- which(df_fichas$x == linea[k, ]$y & df_fichas$y == linea[k, ]$x)
      df_fichas$ficha[r] <- 3
      df_fichas$alpha[r] <- 1
    }
  }

  p <- ggplot() +
    geom_point(data = df_fichas,
               aes(x = x_centro, y = y_centro, fill = ficha, alpha = alpha),
               shape = 21, size = 23) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "papayawhip", "2" = "grey15", "3" = "red"),
      name   = "ficha"
    ) +
    geom_segment(data = df_lineas_h,
                 aes(x = xini, y = yini, xend = xend, yend = yend),
                 linewidth = 0.5, color = "black") +
    geom_segment(data = df_lineas_v,
                 aes(x = xini, y = yini, xend = xend, yend = yend),
                 linewidth = 0.5, color = "black") +
    geom_text(data = df_columnas_labels,
              aes(x = x, y = y, label = labels)) +
    coord_fixed() +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:6) +
    theme_void() +
    labs(title = "Tablero de Conecta 4") +
    theme(
      plot.title    = element_text(hjust = 0.5),
      legend.position = "none"
    )

  return(p)
}
