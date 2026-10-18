#' Create an empty board
#'
#' @description Initializes and returns an empty Connect 4 board: a matrix
#'   with 6 rows and 7 columns filled with zeros.
#'
#'   Cell value convention:
#'   \describe{
#'     \item{0}{Empty cell.}
#'     \item{1}{Piece of player 1 (human).}
#'     \item{2}{Piece of player 2 (AI).}
#'   }
#'
#'   Row 1 is the top of the board and row 6 the
#'   bottom (pieces fall by gravity to the first free row
#'   from the bottom).
#'
#' @return Integer 6 x 7 matrix initialized to 0.
#'
#' @examples
#' board <- reset_board()
#' dim(board)   # 6 7
#' all(board == 0)
#'
#' @export
reset_board <- function() {
  board <- matrix(0L, nrow = 6, ncol = 7)
  return(board)
}
