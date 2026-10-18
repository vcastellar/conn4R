#' Create a random position
#'
#' @description Creates a random board by simulating \code{depth} random
#'   legal moves between the two players (the human opens). If the sequence
#'   produces a win before all the moves have been made, it is discarded and
#'   generated again, so the returned board is not a terminal
#'   position (unless the board fills up completely).
#' @param depth number of moves simulated at random, an integer between 0
#'   and 42. With \code{0} the empty board is returned.
#' @return Integer 6 x 7 matrix with the generated position. Cells: 0 empty,
#'   1 human, 2 AI.
#' @examples
#' board <- create_random_position(21)
#' visualize_board(board)
#' sum(board == 1)
#' sum(board == 2)
#' @export
create_random_position <- function(depth = 10) {
  if (length(depth) != 1L || !is.numeric(depth) || is.na(depth) ||
      depth != as.integer(depth) || depth < 0 || depth > 42) {
    stop("`depth` must be an integer between 0 and 42.", call. = FALSE)
  }

  restart <- TRUE

  while (restart) {
    restart <- FALSE

    turn <- 1
    board <- reset_board()

    for (i in seq_len(depth)) {

      # pick a move at random
      possible_moves <- available_moves(board)

      move <- unlist(sample(as.list(possible_moves), size = 1))
      board <- make_move(board, move, turn)

      # check whether the game is over. If so, the board is reset
      if (game_over(board)$finished & i < 42) {
        restart <- TRUE
        break

      }

      turn <- (turn %% 2) + 1

    }

  }

  return(board)

}
