# Paleta y medidas compartidas por las funciones de visualización. Los radios
# están expresados en unidades de celda (cada casilla mide 1 x 1).
.estilo_tablero <- list(
  fondo         = "#F4F6FA",
  tablero       = "#2155CD",
  tablero_borde = "#173E9C",
  sombra        = "#0B1B3F",
  hueco         = "#E8EDF5",
  hueco_borde   = "#163A8F",
  humano        = "#FFC93C",
  ia            = "#E84855",
  ficha_borde   = "#00000038",
  resalte       = "#FFFFFF",
  texto         = "#0F172A",
  texto_suave   = "#64748B",
  radio_hueco   = 0.42,
  radio_ficha   = 0.39,
  radio_interior = 0.27,
  radio_ultima  = 0.465,
  radio_ganadora = 0.085
)

#' Detectar y devolver las coordenadas de la línea ganadora
#'
#' @description Recorre todas las posibles líneas de 4 del tablero y devuelve
#'   las coordenadas de visualización de la primera línea ganadora encontrada
#'   (puntuación mayor o igual que 10 000), o \code{NULL} si no existe ninguna. Usada
#'   internamente por \code{\link{visualizar_tablero}} para resaltar las
#'   fichas ganadoras.
#'
#' @param tablero Matriz de 6 x 7 con el estado del tablero.
#'
#' @return Data frame con columnas \code{x} (altura, 1 en la fila inferior),
#'   \code{y} (columna) y \code{ficha} (valor 3) para las 4 celdas de la línea
#'   ganadora, ordenadas de un extremo a otro, o \code{NULL} si no hay ganador.
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

# Vértices de un círculo por cada centro (cx, cy). Se dibujan como polígonos
# para que su tamaño dependa de los ejes y no del dispositivo gráfico. Las
# columnas pasadas en `...` se replican en todos los vértices de su círculo.
.circulos <- function(cx, cy, r, ..., n = 64L) {
  angulo <- seq(0, 2 * pi, length.out = n + 1L)[-1L]
  id <- rep(seq_along(cx), each = n)
  df <- data.frame(
    x     = cx[id] + r * cos(angulo),
    y     = cy[id] + r * sin(angulo),
    grupo = id,
    cx    = cx[id],
    cy    = cy[id]
  )
  for (nombre in names(list(...))) {
    v <- list(...)[[nombre]]
    df[[nombre]] <- if (length(v) == 1L) rep(v, nrow(df)) else v[id]
  }
  df
}

# Contorno de un rectángulo con las esquinas redondeadas de radio `r`.
.rectangulo_redondeado <- function(xmin, xmax, ymin, ymax, r, n = 16L) {
  esquina <- function(cx, cy, desde) {
    angulo <- seq(desde, desde + pi / 2, length.out = n)
    data.frame(x = cx + r * cos(angulo), y = cy + r * sin(angulo))
  }
  rbind(
    esquina(xmax - r, ymax - r, 0),
    esquina(xmin + r, ymax - r, pi / 2),
    esquina(xmin + r, ymin + r, pi),
    esquina(xmax - r, ymin + r, 3 * pi / 2)
  )
}

#' Visualizar el tablero de Conecta 4
#'
#' @description Genera un gráfico \pkg{ggplot2} del estado actual del tablero
#'   con un estilo plano y moderno: un panel azul con esquinas redondeadas y
#'   sombra, huecos claros para las casillas vacías, fichas ámbar para el
#'   jugador 1 (humano) y coral para el jugador 2 (IA). Las fichas se dibujan
#'   en coordenadas de datos, por lo que el tablero se adapta al tamaño del
#'   dispositivo gráfico.
#'
#'   El subtítulo resume el estado de la partida (número de jugadas, victoria o
#'   empate). Si existe una línea ganadora, se traza una línea blanca sobre sus
#'   cuatro fichas y se atenúan las demás. Cuando se proporciona
#'   \code{ultima_jugada}, esa ficha se rodea con un aro blanco y su número de
#'   columna se destaca.
#'
#' @param tablero Matriz de 6 x 7 con el estado del tablero. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#' @param ultima_jugada Coordenadas \code{c(fila, columna)} de la última ficha
#'   colocada, o \code{NULL} para no destacarla.
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
visualizar_tablero <- function(tablero, ultima_jugada = NULL) {
  if (!is.null(ultima_jugada) &&
      (length(ultima_jugada) != 2L || anyNA(ultima_jugada) ||
       any(!is.finite(ultima_jugada)) ||
       any(ultima_jugada != as.integer(ultima_jugada)) ||
       !ultima_jugada[[1]] %in% 1:6 || !ultima_jugada[[2]] %in% 1:7 ||
       tablero[ultima_jugada[[1]], ultima_jugada[[2]]] == 0L)) {
    stop("`ultima_jugada` debe señalar una ficha del tablero mediante c(fila, columna).",
         call. = FALSE)
  }

  est <- .estilo_tablero

  # Casillas en coordenadas de dibujo: x = columna, y = altura (1 abajo).
  celdas <- data.frame(
    cx    = rep(1:7, each = 6),
    cy    = rep(6:1, times = 7),
    ficha = as.vector(tablero)
  )
  ocupadas <- celdas[celdas$ficha != 0L, ]

  linea <- .detectar_lineas(tablero)
  ganadora <- rep(FALSE, nrow(ocupadas))
  if (!is.null(linea)) {
    ganadora <- paste(ocupadas$cx, ocupadas$cy) %in% paste(linea$y, linea$x)
  }
  # Con una línea ganadora, el resto de fichas se atenúa para destacarla.
  opacidad <- if (is.null(linea)) rep(1, nrow(ocupadas)) else ifelse(ganadora, 1, 0.4)

  df_marco <- .rectangulo_redondeado(0.38, 7.62, 0.38, 6.62, r = 0.32)
  df_sombra <- transform(df_marco, x = x + 0.05, y = y - 0.10)
  df_huecos <- .circulos(celdas$cx, celdas$cy, est$radio_hueco)
  df_fichas <- .circulos(
    ocupadas$cx, ocupadas$cy, est$radio_ficha,
    jugador = factor(ocupadas$ficha, levels = 1:2, labels = c("Humano", "IA")),
    alpha   = opacidad
  )
  df_interior <- .circulos(ocupadas$cx, ocupadas$cy, est$radio_interior,
                           alpha = opacidad)

  df_columnas <- data.frame(x = 1:7, y = 6.98, etiqueta = as.character(1:7))

  # Estado de la partida para el subtítulo.
  n_jugadas <- sum(tablero != 0L)
  if (!is.null(linea)) {
    ganador <- tablero[7L - linea$x[[1]], linea$y[[1]]]
    estado <- if (ganador == 1L) "Victoria del humano" else "Victoria de la IA"
  } else if (n_jugadas == 42L) {
    estado <- "Empate: tablero completo"
  } else {
    estado <- sprintf("%d %s", n_jugadas, if (n_jugadas == 1L) "jugada" else "jugadas")
  }
  if (!is.null(ultima_jugada)) {
    estado <- sprintf("%s  ·  Última jugada: columna %d",
                      estado, as.integer(ultima_jugada[[2]]))
  }

  p <- ggplot() +
    geom_polygon(data = df_sombra, aes(x = x, y = y),
                 fill = est$sombra, alpha = 0.18) +
    geom_polygon(data = df_marco, aes(x = x, y = y),
                 fill = est$tablero, colour = est$tablero_borde, linewidth = 0.6) +
    geom_polygon(data = df_huecos, aes(x = x, y = y, group = grupo),
                 fill = est$hueco, colour = est$hueco_borde, linewidth = 0.4)

  # La capa de fichas se añade aunque no haya ninguna para que la leyenda
  # aparezca siempre y el tablero no cambie de tamaño durante la partida.
  p <- p +
    geom_polygon(data = df_fichas,
                 aes(x = x, y = y, group = grupo, fill = jugador, alpha = alpha),
                 colour = est$ficha_borde, linewidth = 0.5, key_glyph = "point") +
    geom_polygon(data = df_interior,
                 aes(x = x, y = y, group = grupo, alpha = alpha * 0.08),
                 fill = "black", colour = NA)

  if (!is.null(ultima_jugada)) {
    df_ultima <- .circulos(
      as.integer(ultima_jugada[[2]]), 7L - as.integer(ultima_jugada[[1]]),
      est$radio_ultima, elemento = "ultima_jugada"
    )
    p <- p + geom_polygon(data = df_ultima, aes(x = x, y = y, group = grupo),
                          fill = NA, colour = est$resalte, linewidth = 1.3)
  }

  if (!is.null(linea)) {
    df_linea <- data.frame(x = linea$y[[1]], y = linea$x[[1]],
                           xend = linea$y[[4]], yend = linea$x[[4]])
    df_puntos <- .circulos(linea$y, linea$x, est$radio_ganadora,
                           elemento = "linea_ganadora")
    p <- p +
      geom_segment(data = df_linea, aes(x = x, y = y, xend = xend, yend = yend),
                   colour = est$resalte, alpha = 0.9, linewidth = 2.2,
                   lineend = "round") +
      geom_polygon(data = df_puntos, aes(x = x, y = y, group = grupo),
                   fill = est$resalte, colour = NA)
  }

  p <- p +
    geom_text(data = df_columnas, aes(x = x, y = y, label = etiqueta),
              colour = est$texto_suave, size = 3.6)

  if (!is.null(ultima_jugada)) {
    p <- p + geom_text(data = df_columnas[ultima_jugada[[2]], ],
                       aes(x = x, y = y, label = etiqueta),
                       colour = est$tablero, size = 4.2, fontface = "bold")
  }

  p +
    scale_fill_manual(
      values = c(Humano = est$humano, IA = est$ia),
      limits = c("Humano", "IA"),
      drop   = FALSE,
      name   = NULL
    ) +
    scale_alpha_identity() +
    guides(fill = guide_legend(
      override.aes = list(shape = 21, size = 6, stroke = 0.6,
                        colour = est$ficha_borde, alpha = 1)
    )) +
    coord_fixed(xlim = c(0.3, 7.7), ylim = c(0.25, 7.2), expand = FALSE) +
    labs(title = "Conecta 4", subtitle = estado) +
    theme_void(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = est$fondo, colour = NA),
      panel.background  = element_rect(fill = est$fondo, colour = NA),
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 18,
                                       colour = est$texto,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(hjust = 0.5, size = 11,
                                       colour = est$texto_suave,
                                       margin = margin(b = 6)),
      legend.position   = "bottom",
      legend.text       = element_text(colour = est$texto, size = 11,
                                       margin = margin(r = 14, l = 2)),
      legend.key        = element_rect(fill = NA, colour = NA),
      legend.margin     = margin(t = 2),
      plot.margin       = margin(14, 14, 10, 14)
    )
}

#' Visualizar una variante jugada paso a paso
#'
#' @description Reproduce sobre un tablero una secuencia de jugadas alternando
#'   los jugadores. Muestra primero la posición inicial mediante
#'   \code{\link{visualizar_tablero}} y, después de cada pausa, muestra la
#'   posición resultante de la siguiente jugada.
#'
#' @param tablero Matriz de 6 x 7 con la posición inicial. Celdas: 0 vacío,
#'   1 humano, 2 IA.
#' @param turno Jugador que realiza la primera jugada de \code{variante}:
#'   \code{1} (humano) o \code{2} (IA).
#' @param variante Vector de columnas (1-7) que se reproducirán en orden.
#' @param lapso Segundos de espera entre posiciones. Por defecto, 1 segundo.
#'   Puede usarse \code{0} para reproducir la variante sin espera.
#'
#' @return Invisiblemente, la matriz de 6 x 7 resultante después de reproducir toda
#'   la variante.
#'
#' @examples
#' \dontrun{
#' tablero <- reiniciar_tablero()
#' visualizar_variante(tablero, turno = 1, variante = c(4, 4, 3, 5))
#' }
#'
#' @seealso \code{\link{visualizar_tablero}}, \code{\link{realizar_jugada}}
#'
#' @export
visualizar_variante <- function(tablero, turno, variante, lapso = 1) {
  if (!is.matrix(tablero) || !identical(dim(tablero), c(6L, 7L)) ||
      anyNA(tablero) || any(!tablero %in% 0:2)) {
    stop("`tablero` debe ser una matriz 6x7 con valores 0, 1 o 2.",
         call. = FALSE)
  }
  if (length(turno) != 1L || is.na(turno) || !turno %in% c(1, 2)) {
    stop("`turno` debe ser 1 o 2.", call. = FALSE)
  }
  if (!is.numeric(variante) || anyNA(variante) ||
      any(!is.finite(variante)) || any(variante != as.integer(variante)) ||
      any(!variante %in% 1:7)) {
    stop("`variante` debe contener columnas enteras entre 1 y 7.",
         call. = FALSE)
  }
  if (length(lapso) != 1L || !is.numeric(lapso) || is.na(lapso) ||
      !is.finite(lapso) || lapso < 0) {
    stop("`lapso` debe ser un número finito mayor o igual que 0.",
         call. = FALSE)
  }

  tablero_actual <- matrix(as.integer(tablero), nrow = 6L, ncol = 7L)
  jugador <- as.integer(turno)
  print(visualizar_tablero(tablero_actual))

  for (i in seq_along(variante)) {
    columna <- as.integer(variante[[i]])
    if (!columna %in% jugadas_disponibles(tablero_actual)) {
      stop(sprintf("La jugada %d no es válida: la columna %d está llena.",
                   i, columna), call. = FALSE)
    }

    Sys.sleep(lapso)
    tablero_actual <- realizar_jugada(tablero_actual, columna, jugador)
    fila <- min(which(tablero_actual[, columna] != 0L))
    print(visualizar_tablero(tablero_actual, c(fila, columna)))
    jugador <- 3L - jugador
  }

  invisible(tablero_actual)
}
