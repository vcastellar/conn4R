# Infers which player is to move from the number of pieces on the board,
# assuming that the human (player 1) opens the game. Returns NA if the
# piece count does not correspond to a legal human-first game.
.player_to_move <- function(board) {
  n1 <- sum(board == 1L)
  n2 <- sum(board == 2L)
  if (n1 == n2)      return(1L)
  if (n1 == n2 + 1L) return(2L)
  NA_integer_
}

.analysis_verdict <- function(score, outcome) {
  if (isTRUE(outcome$finished)) {
    return(switch(as.character(outcome$result),
      "1" = "Game over: the human wins (player 1).",
      "2" = "Game over: the AI wins (player 2).",
      "0" = "Game over: draw.",
      "Game over."))
  }
  if (score >=  1e9) return("Forced win for the AI (player 2).")
  if (score <= -1e9) return("Forced win for the human (player 1).")
  if (score > 0)     return("Advantage for the AI (player 2).")
  if (score < 0)     return("Advantage for the human (player 1).")
  "Balanced position."
}

#' Analyze a position with the minimax engine
#'
#' @description High-level interface on top of \code{\link{minimax}} designed
#'   for studying a position: it computes the recommended move for the player
#'   to move, the minimax score, the static evaluation and the principal
#'   variation, and wraps them in a printable object with a readable verdict.
#'   Unlike \code{minimax}, there is no need to manually translate the turn
#'   into \code{maximizingAI}: just say who moves (or let it be inferred).
#'
#' @param board 6 x 7 matrix with the position to analyze. Cells: 0 empty,
#'   1 human, 2 AI. It can be built with \code{\link{create_position}}.
#' @param turn Player to move: \code{1} (human) or \code{2} (AI).
#'   If \code{NULL} (the default), it is inferred from the number of pieces on
#'   the board, assuming that the human opens the game.
#' @param depth Maximum minimax search depth. Defaults to
#'   \code{7}.
#' @param show Logical. If \code{TRUE}, replays the principal variation on
#'   the board using \code{\link{visualize_variation}}. Defaults to
#'   \code{FALSE}.
#' @param delay Seconds to wait between positions when \code{show = TRUE}.
#'   Defaults to \code{1}.
#'
#' @return An object of class \code{"conn4R_analysis"} (a list) with, among
#'   others, the elements:
#' \describe{
#'   \item{turn}{Player analyzed (1 or 2).}
#'   \item{depth}{Search depth used.}
#'   \item{move}{Recommended column (1-7), or \code{NA} if there is no move.}
#'   \item{score}{Minimax score (from the AI's perspective: positive =
#'     AI advantage).}
#'   \item{evaluation}{Static evaluation of the position.}
#'   \item{nodes}{Nodes evaluated during the search.}
#'   \item{variation}{Principal variation (sequence of columns).}
#'   \item{verdict}{Textual interpretation of the score.}
#' }
#'   The object has a \code{print} method that shows a readable summary.
#'
#' @examples
#' # Enter a position and analyze it (turn inferred automatically)
#' board <- create_position(c(4, 4, 3, 5, 3))
#' analysis <- analyze_position(board, depth = 5)
#' analysis
#' analysis$move
#'
#' # Give the turn explicitly
#' analyze_position(board, turn = 1, depth = 4)
#'
#' @seealso \code{\link{minimax}}, \code{\link{create_position}},
#'   \code{\link{visualize_variation}}
#'
#' @export
analyze_position <- function(board, turn = NULL, depth = 7,
                             show = FALSE, delay = 1) {
  if (!is.matrix(board) || !identical(dim(board), c(6L, 7L)) ||
      anyNA(board) || any(!board %in% 0:2)) {
    stop("`board` must be a 6x7 matrix with values 0, 1 or 2.",
         call. = FALSE)
  }
  if (length(depth) != 1L || is.na(depth) ||
      !is.numeric(depth) || !is.finite(depth) ||
      depth != as.integer(depth) || depth < 0) {
    stop("`depth` must be an integer greater than or equal to 0.", call. = FALSE)
  }
  if (length(show) != 1L || is.na(show) || !is.logical(show)) {
    stop("`show` must be TRUE or FALSE.", call. = FALSE)
  }

  board <- matrix(as.integer(board), nrow = 6L, ncol = 7L)

  if (is.null(turn)) {
    turn <- .player_to_move(board)
    if (is.na(turn)) {
      stop("Cannot infer the turn from this position; ",
           "specify `turn` (1 = human, 2 = AI).", call. = FALSE)
    }
  }
  if (length(turn) != 1L || is.na(turn) || !turn %in% c(1, 2)) {
    stop("`turn` must be 1 (human) or 2 (AI).", call. = FALSE)
  }

  turn  <- as.integer(turn)
  depth <- as.integer(depth)
  maximizingAI <- (turn == 2L)

  outcome <- game_over(board)
  res <- minimax(board, depth, maximizingAI)

  analysis <- list(
    turn           = turn,
    depth          = depth,
    move           = res$move,
    score          = res$score,
    evaluation     = evaluate_position(board),
    nodes          = res$nodes,
    normal_nodes   = res$normal_nodes,
    tactical_nodes = res$tactical_nodes,
    variation      = res$variation,
    finished       = isTRUE(outcome$finished),
    result         = outcome$result,
    verdict        = .analysis_verdict(res$score, outcome)
  )
  class(analysis) <- "conn4R_analysis"

  if (isTRUE(show) && length(res$variation) > 0L) {
    visualize_variation(board, turn, res$variation, delay = delay)
  }

  analysis
}

#' @description \code{print.conn4R_analysis} shows a readable summary of the
#'   analysis of a position.
#' @param x Object of class \code{"conn4R_analysis"} returned by
#'   \code{analyze_position}.
#' @param ... Additional arguments (ignored).
#' @rdname analyze_position
#' @export
print.conn4R_analysis <- function(x, ...) {
  who  <- if (x$turn == 1L) "human (player 1)" else "AI (player 2)"
  rule <- strrep("-", 57L)

  cat("Position analysis (C++ minimax engine)\n")
  cat(rule, "\n", sep = "")
  cat(sprintf("To move:             %s\n", who))
  cat(sprintf("Depth:               %d\n", x$depth))
  if (isTRUE(x$finished)) {
    cat("Status:              the game is already over\n")
  }
  cat(sprintf("Recommended move:    %s\n",
              if (is.na(x$move)) "none" else sprintf("column %d", x$move)))
  cat(sprintf("Minimax score:       %.0f\n", as.double(x$score)))
  cat(sprintf("Static evaluation:   %d\n", as.integer(x$evaluation)))
  cat(sprintf("Verdict:             %s\n", x$verdict))
  cat(sprintf("Nodes evaluated:     %.0f\n", as.double(x$nodes)))
  cat(sprintf("Principal variation: %s\n",
              if (length(x$variation) == 0L) "(no variation)"
              else paste(x$variation, collapse = " -> ")))
  cat(rule, "\n", sep = "")

  invisible(x)
}
