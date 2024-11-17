# prompt: supón que van a jugar dos jugadores. Cada jugador realiza un movimiento por turnos. Los movimientos
# consisten en dejar caer en una de las siete columnas una ficha rojo o amarilla, según el jugador.
# Un movimiento es legal si la columna donde juega el ju8gador no está completamente llenada.
# Crea una función que en cada turno liste las jugadas disponibles del jugador en turno

jugadas_disponibles <- function(tablero) {
  # Encuentra las columnas que no están llenas
  columnas_disponibles <- which(tablero[1, ] == 0)
  return(columnas_disponibles)
}

# Ejemplo de uso:
# Supongamos que es el turno del jugador 1
# Lista las columnas disponibles donde el jugador 1 puede jugar
