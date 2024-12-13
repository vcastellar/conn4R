#' visualiza gráficamente la posición de un tablero
#'
#' @description representa gráficamente el estado de un "tablero"
#' @param tablero matriz 6x7 que representa la posición de un tablero
#' @return devuelve un objeto ggplot listo para representar gráficamente el tablero
#' @examples
#' tablero <- crear_posicion_aleatoria(21)
#' p <- visualizar_tablero(tablero)
#' print(p)



visualizar_tablero <- function(tablero) {
  library(ggplot2)

  # Creamos un data frame con las coordenadas de las fichas
  df_fichas <- data.frame(
    x = rev(rep(1:7, each = 6)),
    y = rep(1:6, times = 7),
    ficha = factor(rev(as.vector(tablero)), levels = c(0,1,2))
  )


  # Creamos un data frame con las coordenadas de las líneas del grid
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
    x = 1:7,
    y = 7,
    labels = paste0("col.", 1:7)
  )
  
  df_fichas <- transform(df_fichas,
                         x_centro = x,
                         y_centro = y, 
                         alpha = ifelse(ficha == 0, 0, 1))

  
  p <- ggplot() +
    geom_point(data = df_fichas, 
               aes(x = x_centro, y = y_centro, fill = ficha, alpha = alpha),
               shape = 21, size = 23) +
    scale_fill_manual(values = c( "0" = "white", "1" = "papayawhip", "2" = "grey15"), name = "ficha") +
    geom_segment(data = df_lineas_h, aes(x = xini, y = yini, xend = xend, yend = yend), size = 0.5, color = "black") +
    geom_segment(data = df_lineas_v, aes(x = xini, y = yini, xend = xend, yend = yend), size = 0.5, color = "black") +
    geom_text(data = df_columnas_labels, aes(x = x, y = y, label = labels)) +
    coord_fixed() +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:6) +
    theme_void() +
    labs(title = "Tablero de Conecta 4") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  return(p)
}

