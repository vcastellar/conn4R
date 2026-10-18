#' Enter a position from a sequence of moves
#'
#' @description Builds a Connect 4 board by replaying, on an empty board, a
#'   sequence of moves (columns) that alternate between the two players. It is
#'   the recommended way to enter a specific position in order to analyze it
#'   afterwards with \code{\link{analyze_position}} or to continue the game:
#'   since legal moves are replayed, the resulting position is always
#'   reachable in a real game.
#'
#' @param moves Vector of columns (integers between 1 and 7) played in
#'   order. The first move is made by \code{first_player}, and from then on
#'   the players alternate. Defaults to \code{integer()} (empty
#'   board).
#' @param first_player Player who makes the first move of \code{moves}:
#'   \code{1} (human) or \code{2} (AI). Defaults to \code{1}.
#'
#' @return Integer 6 x 7 matrix with the resulting position. Cells: 0 empty,
#'   1 human, 2 AI.
#'
#' @details The function validates each move before applying it: the column
#'   must be between 1 and 7 and must not be full, and the game cannot be over
#'   before the sequence is complete. If any move is illegal, it stops with
#'   a message indicating the offending move.
#'
#' @examples
#' # Central opening followed by alternating replies (the human opens)
#' board <- create_position(c(4, 4, 3, 5))
#' visualize_board(board)
#'
#' # The AI opens the game
#' board <- create_position(c(4, 3), first_player = 2)
#' sum(board == 2)  # 1 AI piece
#'
#' @seealso \code{\link{analyze_position}}, \code{\link{make_move}},
#'   \code{\link{reset_board}}
#'
#' @export
create_position <- function(moves = integer(), first_player = 1) {
  if (length(first_player) != 1L || is.na(first_player) ||
      !first_player %in% c(1, 2)) {
    stop("`first_player` must be 1 (human) or 2 (AI).", call. = FALSE)
  }

  board <- reset_board()

  if (length(moves) == 0L) {
    return(board)
  }

  if (!is.numeric(moves) || anyNA(moves) || any(!is.finite(moves)) ||
      any(moves != as.integer(moves)) || any(!moves %in% 1:7)) {
    stop("`moves` must contain integer columns between 1 and 7.", call. = FALSE)
  }

  player <- as.integer(first_player)

  for (i in seq_along(moves)) {
    if (game_over(board)$finished) {
      stop(sprintf("The game was already over before move %d.", i),
           call. = FALSE)
    }

    column <- as.integer(moves[[i]])
    if (!column %in% available_moves(board)) {
      stop(sprintf(
        "Move %d is not valid: column %d is full.", i, column),
        call. = FALSE)
    }

    board <- make_move(board, column, player)
    player <- 3L - player
  }

  board
}
