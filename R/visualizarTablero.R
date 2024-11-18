# prompt: crea una función que haga una representación gráfica del tablero y su estado,
# representando las fichas del humano de color rojo y las de la IA de color azul.
# Puedes usar ggplot2 o plotly


visualizar_tablero <- function(tablero) {
  library(ggplot2)

  # Creamos un data frame con las coordenadas de las fichas
  df_fichas <- data.frame(
    x = rev(rep(1:7, each = 6)),
    y = rep(1:6, times = 7),
    ficha = rev(as.vector(tablero))
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


  p <- ggplot() +
    geom_tile(data = df_fichas, aes(x = x, y = y, fill = factor(ficha))) +
    scale_fill_manual(values = c("white", "red", "blue"), name = "Ficha") +
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
      legend.position = "bottom"
    )
  return(p)
}

# Ejemplo de uso:
# Supongamos que ya tienes el tablero definido
#representar_tablero(tablero)
