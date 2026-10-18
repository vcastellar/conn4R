#' Start a game of Connect 4
#'
#' @description Main entry point of the package. Starts a game of
#'   Connect 4 using the C++ engine compiled via Rcpp for the minimax
#'   computation, the static evaluation and the detection of the end of the
#'   game. The interface (visualization, user input, adaptive depth) is
#'   handled from R.
#'
#' @param depth Search depth of the minimax algorithm. Defaults to 5.
#' @param turn Player who starts: 1 for human, 2 for AI. Defaults to 1.
#' @param adaptiveDepth Logical. If \code{TRUE}, the depth is adjusted
#'   dynamically according to the available columns. Defaults to \code{TRUE}.
#' @param auto Logical. If \code{TRUE}, the AI plays against itself without
#'   human intervention. Defaults to \code{FALSE}.
#' @param mouse Logical. If \code{TRUE}, the human player chooses their move
#'   by clicking with the mouse on the desired column of the board (if the
#'   graphics device allows it); if \code{FALSE}, the move is typed on the
#'   keyboard. Defaults to \code{TRUE}. See \code{\link{human_turn}}.
#' @param memory_mb Transposition table budget in MiB. Defaults to 64.
#'   Each call starts a new engine and reuses its table for the whole game.
#'
#' @return The 6 x 7 matrix with the final state of the board (invisibly).
#'
#' @examples
#' \dontrun{
#' # Human vs AI game (human first, depth 6)
#' start_game(depth = 6)
#'
#' # The AI opens the game
#' start_game(turn = 2)
#'
#' # AI against itself
#' start_game(auto = TRUE, depth = 5)
#' }
#'
#' @seealso \code{\link{minimax}}, \code{\link{evaluate_position}}
#'
#' @export
start_game <- function(depth = 5, turn = 1,
                       adaptiveDepth = TRUE, auto = FALSE,
                       mouse = TRUE, memory_mb = 64) {

  if (length(turn) != 1L || is.na(turn) || !is.numeric(turn) ||
      !turn %in% c(1, 2)) {
    stop("`turn` must be 1 (human) or 2 (AI).", call. = FALSE)
  }
  engine <- create_engine(memory_mb = memory_mb, first_player = turn)

  # RStudio finishes painting its graphics device from the event loop.
  # If minimax enters C++ immediately, that repaint is left pending and the
  # panel may stay blank during the whole search. Forcing the flush
  # and giving the device enough time to complete the repaint makes
  # the board visible before the AI starts computing. During
  # the search, the C++ engine also services the event loop periodically.
  show_board <- function(tab, last_move = NULL) {
    grDevices::dev.hold()
    device_held <- TRUE
    on.exit(if (device_held) grDevices::dev.flush(), add = TRUE)
    print(visualize_board(tab, last_move))
    grDevices::dev.flush()
    device_held <- FALSE
    flush.console()
    Sys.sleep(0.1)
    invisible(NULL)
  }

  board  <- reset_board()
  show_board(board)
  i      <- 1
  j      <- 0
  result <- 0L

  # Internal helpers that call the C++ engine
  ai_move <- function(tab, search_depth, maximizingAI) {
    tik <- system.time({
      res <- minimax(tab, search_depth, maximizingAI, engine = engine)
    })
    list(move           = res$move,
         score          = res$score,
         nodes          = res$nodes,
         normal_nodes   = res$normal_nodes,
         tactical_nodes = res$tactical_nodes,
         variation      = res$variation,
         time           = tik[[3]])
  }

  end_turn <- function(tab, current_turn, search_depth, max_ai) {
    res <- ai_move(tab, search_depth, max_ai)
    tab <- make_move(tab, res$move, current_turn)
    row <- min(which(tab[, res$move] != 0L))
    show_board(tab, c(row, res$move))
    cat("\n")
    nodes_s <- if (res$time > 0) round(res$nodes / res$time) else NA
    cat("-----------------------------------------------------------------\n")
    cat(sprintf("evaluation:       %d\n",     res$score))
    cat(sprintf("move played:      %d\n",     res$move))
    cat(sprintf("depth:            %d\n",     search_depth))
    cat(sprintf("nodes evaluated:  %.0f\n",   res$nodes))
    cat(sprintf("minimax nodes:    %.0f\n",   res$normal_nodes))
    cat(sprintf("tactical nodes:   %.0f\n",   res$tactical_nodes))
    cat(sprintf("nodes/s:          %.0f\n",   nodes_s))
    cat(sprintf("time:             %.2f s\n", res$time))
    cat(sprintf("variation:        %s\n",     paste(res$variation, collapse = " -> ")))
    cat("-----------------------------------------------------------------\n\n")
    tab
  }

  finished <- function(tab) {
    game_over(tab)
  }

  # Human vs AI mode
  if (!auto) {
    if (turn == 2) {
      search_depth <- if (adaptiveDepth) .adaptive_depth(board, depth) else depth
      board        <- end_turn(board, 2L, search_depth, TRUE)
      outcome      <- finished(board)
      if (outcome$finished) {
        cat(sprintf("Result: %s\n", .result_label(outcome$result)))
        return(invisible(board))
      }
      i <- i + 1; j <- 1
    }

    while (i <= (42 - j)) {
      previous_board <- board
      board <- human_turn(board, mouse = mouse)
      last_move <- which(board != previous_board, arr.ind = TRUE)
      show_board(board, last_move[1, ])
      cat(sprintf("evaluation (human): %d\n", evaluate_position(board)))

      outcome <- finished(board)
      if (outcome$finished) { result <- outcome$result; break }
      i <- i + 1

      search_depth <- if (adaptiveDepth) .adaptive_depth(board, depth) else depth
      board        <- end_turn(board, 2L, search_depth, TRUE)

      outcome <- finished(board)
      if (outcome$finished) { result <- outcome$result; break }
      i <- i + 1
    }

    cat(sprintf("Result: %s\n", .result_label(result)))
  }

  # Automatic mode (AI vs AI)
  if (auto) {
    while (i <= 42) {
      current_turn <- if ((i - 1) %% 2 == 0) turn else 3L - turn
      max_ai       <- (current_turn == 2)
      search_depth <- if (adaptiveDepth) .adaptive_depth(board, depth) else depth

      board <- end_turn(board, current_turn, search_depth, max_ai)

      outcome <- finished(board)
      if (outcome$finished) {
        cat(sprintf("Result: %s\n", .result_label(outcome$result)))
        break
      }
      i <- i + 1
    }
  }

  invisible(board)
}

# Text shown at the end of a game for a game_over() result code
# (1 = human wins, 2 = AI wins, 0 = draw).
.result_label <- function(result) {
  switch(as.character(result),
         "1" = "HUMAN WINS (player 1)",
         "2" = "AI WINS (player 2)",
         "0" = "DRAW")
}
