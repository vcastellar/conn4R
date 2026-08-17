#' conn4R: Conecta 4 con motor de IA en C++
#'
#' @description
#' El paquete \pkg{conn4R} implementa el juego de Conecta 4 con una
#' inteligencia artificial basada en el algoritmo minimax con poda alfa-beta,
#' tabla de transposición y búsqueda de quiescencia. El motor de búsqueda está
#' escrito en C++ (via \pkg{Rcpp}) para máximo rendimiento, mientras que la
#' capa R aporta la interfaz de juego, la introducción y el análisis de
#' posiciones y la visualización.
#'
#' @section Convenio del tablero:
#' El tablero se representa como una matriz entera de 6 filas y 7 columnas.
#' Cada celda toma uno de tres valores: \code{0} (vacía), \code{1} (ficha del
#' jugador humano) o \code{2} (ficha de la IA). Las fichas caen por gravedad
#' hasta la fila libre más baja de la columna elegida.
#'
#' @section Funciones principales:
#' \describe{
#'   \item{\code{\link{iniciar_partida}}}{Jugar una partida completa
#'     (humano contra IA o IA contra sí misma).}
#'   \item{\code{\link{crear_posicion}}}{Introducir una posición a partir de
#'     una secuencia de jugadas.}
#'   \item{\code{\link{analizar_posicion}}}{Analizar una posición con el motor
#'     minimax y obtener la jugada recomendada y la variante principal.}
#'   \item{\code{\link{visualizar_tablero}}, \code{\link{visualizar_variante}}}{Visualizar
#'     posiciones y variantes con \pkg{ggplot2}.}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 aes coord_fixed element_text geom_point geom_segment
#' @importFrom ggplot2 geom_text ggplot labs scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme theme_void
#' @importFrom Rcpp evalCpp
#' @useDynLib conn4R, .registration = TRUE
## usethis namespace: end
NULL

# Variables usadas dentro de aes() en visualizar_tablero(); se declaran para
# evitar las notas de "no visible binding for global variable" en R CMD check.
utils::globalVariables(c(
  "x", "y", "xini", "yini", "xend", "yend",
  "x_centro", "y_centro", "ficha", "alpha", "label", "labels"
))
