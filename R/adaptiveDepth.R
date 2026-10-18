# Fraction of available columns (sum of the relative occupancy per column)
.validColumns <- function(board) {
  sum(apply(board, 2, function(x) { sum(x == 0) / 6 }))
}

#' Adaptive search depth
#'
#' @description Computes the optimal depth of the minimax tree from the
#'   effective number of available columns, keeping a fixed budget of
#'   evaluated nodes equivalent to \code{7^base_depth}.
#'
#'   As the board fills up, the possible branches decrease and the same
#'   depth costs fewer nodes; this function uses that margin to
#'   increase the depth automatically without exceeding the original budget.
#'
#' @param board 6 x 7 matrix with the current state of the board.
#' @param base_depth Integer. Guaranteed minimum depth and reference
#'   for computing the node budget (\code{7^base_depth}).
#'   Defaults to \code{7}.
#' @param max_depth Integer. Maximum allowed depth. Defaults to
#'   \code{15}.
#'
#' @return Integer. Adjusted depth for the next call to minimax.
#'
#' @details
#' The algorithm computes \eqn{b} (weighted valid columns) and looks for the
#' depth \eqn{d \in [base\_depth, max\_depth]} such that
#' \eqn{b^d} is as close as possible to \eqn{7^{base\_depth}}.
#' If \eqn{b \leq 1} it returns \code{max_depth} directly.
#'
#' @examples
#' board <- create_random_position(10)
#' conn4R:::.adaptive_depth(board, base_depth = 5)
#'
#' # With an almost full board the depth goes up
#' full_board <- create_random_position(35)
#' conn4R:::.adaptive_depth(full_board, base_depth = 5)
#'
#' @keywords internal
.adaptive_depth <- function(board, base_depth = 7, max_depth = 15) {
  b <- .validColumns(board)
  if (b <= 1) return(max_depth)

  N        <- 7 ^ base_depth
  d_range  <- base_depth:max_depth
  d_best   <- d_range[which.min(abs(b ^ d_range - N))]

  return(d_best)
}

# Counts the total number of pieces placed on the board
.numMoves <- function(board) {
  sum(board != 0)
}
