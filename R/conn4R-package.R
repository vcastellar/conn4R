#' conn4R: Connect 4 with an AI engine in C++
#'
#' @description
#' The \pkg{conn4R} package implements the game of Connect 4 with an
#' artificial intelligence based on the minimax algorithm with alpha-beta
#' pruning, a transposition table and quiescence search. The search engine is
#' written in C++ (via \pkg{Rcpp}) for maximum performance, while the
#' R layer provides the game interface, position entry and analysis, and
#' visualization.
#'
#' @section Board convention:
#' The board is represented as an integer matrix with 6 rows and 7 columns.
#' Each cell takes one of three values: \code{0} (empty), \code{1} (piece of
#' the human player) or \code{2} (piece of the AI). Pieces fall by gravity
#' to the lowest free row of the chosen column.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{start_game}}}{Play a full game
#'     (human against AI or AI against itself).}
#'   \item{\code{\link{create_position}}}{Enter a position from
#'     a sequence of moves.}
#'   \item{\code{\link{analyze_position}}}{Analyze a position with the minimax
#'     engine and get the recommended move and the principal variation.}
#'   \item{\code{\link{visualize_board}}, \code{\link{visualize_variation}}}{Visualize
#'     positions and variations with \pkg{ggplot2}.}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 aes coord_fixed element_rect element_text geom_polygon
#' @importFrom ggplot2 geom_segment geom_text ggplot guide_legend guides labs
#' @importFrom ggplot2 margin scale_alpha_identity scale_fill_manual theme
#' @importFrom ggplot2 theme_void
#' @importFrom Rcpp evalCpp
#' @useDynLib conn4R, .registration = TRUE
## usethis namespace: end
NULL

# Variables used inside aes() in visualize_board(); they are declared to
# avoid the "no visible binding for global variable" notes in R CMD check.
utils::globalVariables(c(
  "x", "y", "xend", "yend", "group", "player", "alpha", "label"
))
