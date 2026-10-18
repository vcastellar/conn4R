// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>
#include <array>
#include <cmath>
#include <climits>

using namespace Rcpp;

// ============================================================
// Constants
// ============================================================

static const int SCORE1 = 1;
static const int SCORE2 = 10;
static const int SCORE3 = 1000;
static const int SCORE4 = 100000;

static const int BONUS_PARITY = 300;
static const int BONUS_STACKED = 8000;
static const int BONUS_CONNECTION = 3;
static const int MATE_SCORE = 1000000000;
static const double WIN_SCORE  = 1e9;
static const double BLOCK_SCORE = 1e8;

// Positional bitboard (row-major, as in R: row 1 = top, row 6 = bottom)
static const int BITBOARD[6][7] = {
  {0, 1, 2, 3, 2, 1, 0},
  {1, 2, 3, 4, 3, 2, 1},
  {2, 3, 4, 5, 4, 3, 2},
  {3, 4, 5, 6, 5, 4, 3},
  {4, 5, 6, 7, 6, 5, 4},
  {1, 2, 3, 4, 3, 2, 1}
};

// ============================================================
// Board representation as a flat array [row*7+col], 0-based
// ============================================================

typedef std::array<int, 42> Board;

inline Board from_r(const IntegerMatrix& m) {
  Board b;
  for (int r = 0; r < 6; r++)
    for (int c = 0; c < 7; c++)
      b[r * 7 + c] = m(r, c);
  return b;
}

inline IntegerMatrix to_r(const Board& b) {
  IntegerMatrix m(6, 7);
  for (int r = 0; r < 6; r++)
    for (int c = 0; c < 7; c++)
      m(r, c) = b[r * 7 + c];
  return m;
}

// ============================================================
// Precomputed lines: 69 lines x 4 cells (0-based flat indices)
// ============================================================

struct Lines {
  int idx[69][4];
  int row[69][4]; // 0-based row of each cell
  int col[69][4]; // 0-based column of each cell
  int n;

  Lines() {
    n = 0;
    auto add = [&](int r0, int c0, int dr, int dc) {
      for (int i = 0; i < 4; i++) {
        row[n][i] = r0 + dr * i;
        col[n][i] = c0 + dc * i;
        idx[n][i] = (r0 + dr * i) * 7 + (c0 + dc * i);
      }
      n++;
    };
    // Horizontal
    for (int r = 0; r < 6; r++)
      for (int c = 0; c <= 3; c++) add(r, c, 0, 1);
    // Vertical
    for (int c = 0; c < 7; c++)
      for (int r = 0; r <= 2; r++) add(r, c, 1, 0);
    // Diagonal ↘
    for (int r = 0; r <= 2; r++)
      for (int c = 0; c <= 3; c++) add(r, c, 1, 1);
    // Diagonal ↙
    for (int r = 0; r <= 2; r++)
      for (int c = 3; c < 7; c++) add(r, c, 1, -1);
  }
};

static const Lines LINES;

// ============================================================
// Basic board functions
// ============================================================

inline bool col_available(const Board& b, int c) {
  return b[c] == 0; // row 0 = top
}

inline Board drop_piece(Board b, int col, int player) {
  for (int r = 5; r >= 0; r--) {
    if (b[r * 7 + col] == 0) {
      b[r * 7 + col] = player;
      return b;
    }
  }
  return b;
}

std::vector<int> available_cols(const Board& b) {
  std::vector<int> cols;
  for (int c = 0; c < 7; c++)
    if (col_available(b, c)) cols.push_back(c);
  return cols;
}

// ============================================================
// Static evaluation
// ============================================================

// Evaluates a player's open lines (lines without opponent pieces)
static int evaluate_turn(const Board& b, int turn) {
  int opponent = (turn == 1) ? 2 : 1;
  int total = 0;
  for (int l = 0; l < LINES.n; l++) {
    int n = 0, empty = 0, blk = 0;
    for (int i = 0; i < 4; i++) {
      int val = b[LINES.idx[l][i]];
      if (val == turn) n++;
      else if (val == 0) empty++;
      else blk++;
    }
    if (blk > 0) continue; // blocked line
    if      (n == 4)                 total += SCORE4;
    else if (n == 3 && empty == 1)   total += SCORE3;
    else if (n == 2 && empty == 2)   total += SCORE2;
    else if (n == 1 && empty == 3)   total += SCORE1;
  }
  return total;
}

// Detects whether the game is over and who won
// Returns: 0 = ongoing, 1 = player 1 wins, 2 = player 2 wins, 3 = draw
static int game_status(const Board& b) {
  if (evaluate_turn(b, 1) >= 50000) return 1;
  if (evaluate_turn(b, 2) >= 50000) return 2;
  if (available_cols(b).empty())    return 3;
  return 0;
}

// Terminal states must always dominate the heuristic evaluation.
// The number of empty cells is used to prefer quick wins and delay losses;
// since it depends only on the board, the score is still safe for the TT.
static int terminal_score(const Board& b, int result) {
  if (result == 3) return 0;
  int empty_cells = 0;
  for (int i = 0; i < 42; i++)
    if (b[i] == 0) empty_cells++;
  return (result == 2) ? MATE_SCORE + empty_cells : -MATE_SCORE - empty_cells;
}

struct ThreatDetail {
  int count, even, odd, stacked;
};

static ThreatDetail count_threats(const Board& b, int turn) {
  ThreatDetail d = {0, 0, 0, 0};
  int threat_cols[42];
  int nc = 0;

  for (int l = 0; l < LINES.n; l++) {
    int n = 0, empty = 0, empty_pos = -1;
    for (int i = 0; i < 4; i++) {
      int val = b[LINES.idx[l][i]];
      if (val == turn) n++;
      else if (val == 0) { empty++; empty_pos = i; }
    }
    if (n != 3 || empty != 1) continue;

    int mate_row = LINES.row[l][empty_pos]; // 0-based
    int mate_col = LINES.col[l][empty_pos]; // 0-based
    // Accessible cell: lowest row (r==5) or the cell below is occupied
    bool accessible = (mate_row == 5) || (b[(mate_row + 1) * 7 + mate_col] != 0);
    if (!accessible) continue;

    d.count++;
    threat_cols[nc++] = mate_col;
    // row_from_bottom = 5 - mate_row (0-based from the bottom, 0..5)
    // but in R it was 7 - mate_row (1-based); now: (6 - mate_row) 1-based = 5-mate_row+1
    int row_from_bottom = 6 - mate_row; // 1..6
    if (row_from_bottom % 2 == 0) d.even++;
    else                          d.odd++;
  }

  // Stacked threats: 2+ threats in the same column
  if (nc > 1) {
    int freq[7] = {0};
    for (int i = 0; i < nc; i++) freq[threat_cols[i]]++;
    for (int c = 0; c < 7; c++) if (freq[c] >= 2) d.stacked++;
  }
  return d;
}

static int connectivity_bonus(const Board& b, int turn) {
  static const int DR[] = {0, 1, 1,  1};
  static const int DC[] = {1, 0, 1, -1};
  int total = 0;
  for (int r = 0; r < 6; r++) {
    for (int c = 0; c < 7; c++) {
      if (b[r * 7 + c] != turn) continue;
      for (int d = 0; d < 4; d++) {
        int nr = r + DR[d], nc = c + DC[d];
        if (nr >= 0 && nr < 6 && nc >= 0 && nc < 7 && b[nr * 7 + nc] == turn)
          total += BONUS_CONNECTION;
      }
    }
  }
  return total;
}

static int evaluate_position_impl(const Board& b) {
  // Positional bitboard
  int bitScore = 0;
  for (int r = 0; r < 6; r++)
    for (int c = 0; c < 7; c++) {
      int v = b[r * 7 + c];
      if      (v == 2) bitScore += BITBOARD[r][c];
      else if (v == 1) bitScore -= BITBOARD[r][c];
    }

  int eval_diff = evaluate_turn(b, 2) - evaluate_turn(b, 1);

  ThreatDetail thr_ai  = count_threats(b, 2);
  ThreatDetail thr_hum = count_threats(b, 1);

  int double_bonus   = (thr_ai.count  >= 2) ? SCORE3 * 10 : 0;
  int double_penalty = (thr_hum.count >= 2) ? SCORE3 * 10 : 0;

  int parity_bonus = (thr_ai.even  - thr_ai.odd)  * BONUS_PARITY
                   - (thr_hum.even - thr_hum.odd) * BONUS_PARITY;

  int stacked_bonus   = thr_ai.stacked  * BONUS_STACKED;
  int stacked_penalty = thr_hum.stacked * BONUS_STACKED;

  int connectivity = connectivity_bonus(b, 2) - connectivity_bonus(b, 1);

  return eval_diff + bitScore
       + double_bonus - double_penalty
       + parity_bonus
       + stacked_bonus - stacked_penalty
       + connectivity;
}

// ============================================================
// Move ordering
// ============================================================

struct MoveScore {
  int col;
  double score;
};

static std::vector<MoveScore> order_moves_impl(const Board& b, int turn) {
  std::vector<int> cols = available_cols(b);
  int opponent = (turn == 1) ? 2 : 1;
  std::vector<MoveScore> ms;

  for (int c : cols) {
    double score;
    Board bp = drop_piece(b, c, turn);
    int go_p = game_status(bp);
    if (go_p == turn) {
      score = WIN_SCORE;
    } else {
      Board bo = drop_piece(b, c, opponent);
      int go_o = game_status(bo);
      if (go_o == opponent) {
        score = BLOCK_SCORE;
      } else {
        score = std::abs((double)evaluate_position_impl(bp)) + std::abs((double)evaluate_position_impl(bo));
      }
    }
    ms.push_back({c, score});
  }

  std::stable_sort(ms.begin(), ms.end(), [](const MoveScore& a, const MoveScore& b) {
    return a.score > b.score;
  });
  return ms;
}

// ============================================================
// Transposition table
// ============================================================

static const int TT_EXACT = 0;
static const int TT_LOWER = 1;
static const int TT_UPPER = 2;

struct TTEntry {
  int depth;
  int score;
  int flag;
  int move; // 0-based column, -1 if unknown
};

typedef std::unordered_map<std::string, TTEntry> TT;

static std::string tt_key(const Board& b) {
  std::string k(42, '0');
  for (int i = 0; i < 42; i++) k[i] = '0' + b[i];
  return k;
}

static const TTEntry* tt_lookup(const TT& tt, const std::string& key,
                                 int depth, int alpha, int beta) {
  auto it = tt.find(key);
  if (it == tt.end()) return nullptr;
  const TTEntry& e = it->second;
  if (e.depth < depth) return nullptr;
  if (e.flag == TT_EXACT) return &e;
  if (e.flag == TT_LOWER && e.score >= beta)  return &e;
  if (e.flag == TT_UPPER && e.score <= alpha) return &e;
  return nullptr;
}

static void tt_store(TT& tt, const std::string& key,
                     int depth, int score, int alpha_orig, int beta_orig, int move) {
  int flag;
  if      (score >= beta_orig)  flag = TT_LOWER;
  else if (score <= alpha_orig) flag = TT_UPPER;
  else                          flag = TT_EXACT;

  auto it = tt.find(key);
  if (it == tt.end() || it->second.depth <= depth) {
    tt[key] = {depth, score, flag, move};
  }
}

// ============================================================
// Minimax with alpha-beta and transposition table
// ============================================================

struct MMResult {
  int score;
  int move; // 0-based column, -1 if there is none
};

struct SearchStats {
  long long normal;
  long long tactical;
};

// Returns the moves that win immediately for a player. Besides being
// useful for move ordering, this information makes it possible to avoid
// statically evaluating a "noisy" position right before a mate (horizon effect).
static std::vector<int> winning_moves(const Board& b, int turn) {
  std::vector<int> winning;
  for (int c : available_cols(b)) {
    Board child = drop_piece(b, c, turn);
    if (game_status(child) == turn) winning.push_back(c);
  }
  return winning;
}

// Connect 4-specific quiescence search. At the horizon only forced
// sequences are extended: winning now or answering an immediate win
// threat. This way an evaluation at depth N never overlooks that the
// opponent mates at N+1. The recursion always adds a piece and is therefore
// bounded by the number of empty cells left on the board.
static MMResult search_tactics(const Board& b, bool maximizingAI,
                               SearchStats& stats) {
  int turn = maximizingAI ? 2 : 1;
  int opponent = maximizingAI ? 1 : 2;

  std::vector<int> own_wins = winning_moves(b, turn);
  if (!own_wins.empty()) {
    Board win = drop_piece(b, own_wins[0], turn);
    stats.tactical++;
    return {terminal_score(win, turn), own_wins[0]};
  }

  std::vector<int> rival_wins = winning_moves(b, opponent);
  if (rival_wins.empty()) return {evaluate_position_impl(b), -1};

  // Two different winning cells cannot be blocked with a single piece.
  // The defender's useless move before the mate is also played out so
  // that terminal_score correctly keeps the distance to the defeat.
  if (rival_wins.size() > 1) {
    Board defense = drop_piece(b, rival_wins[0], turn);
    stats.tactical++;
    std::vector<int> remaining = winning_moves(defense, opponent);
    Board defeat = drop_piece(defense, remaining[0], opponent);
    stats.tactical++;
    return {terminal_score(defeat, opponent), -1};
  }

  // The only non-losing move is to occupy the threatened cell. After doing
  // so the position is stabilized again, because the piece may enable a
  // new threat in the cell immediately above.
  int block = rival_wins[0];
  Board child = drop_piece(b, block, turn);
  stats.tactical++;
  MMResult res = search_tactics(child, !maximizingAI, stats);
  return {res.score, block};
}

static MMResult minimax_cpp(const Board& b, int depth, bool maximizingAI,
                            int alpha, int beta, TT& tt, SearchStats& stats) {
  stats.normal++;

  // A deep search can keep the main thread busy for several seconds.
  // Yielding it periodically lets R/RStudio process the pending repaint
  // of the graphics device (and makes minimax interruptible) without
  // paying the cost of checking for interrupts at every node.
  if ((stats.normal & 4095LL) == 0) Rcpp::checkUserInterrupt();

  int go = game_status(b);
  if (go != 0) return {terminal_score(b, go), -1};
  if (depth == 0) return search_tactics(b, maximizingAI, stats);

  std::string key = tt_key(b);
  const TTEntry* hit = tt_lookup(tt, key, depth, alpha, beta);
  if (hit != nullptr) {
    return {hit->score, hit->move};
  }

  int alpha_orig = alpha;
  int beta_orig  = beta;

  int turn = maximizingAI ? 2 : 1;
  std::vector<MoveScore> moves = order_moves_impl(b, turn);

  // If the TT has a best move, bring it to the front
  auto it_tt = tt.find(key);
  if (it_tt != tt.end() && it_tt->second.move >= 0) {
    int best_col = it_tt->second.move;
    auto it_m = std::find_if(moves.begin(), moves.end(),
                             [best_col](const MoveScore& m){ return m.col == best_col; });
    if (it_m != moves.end() && it_m != moves.begin()) {
      MoveScore tmp = *it_m;
      moves.erase(it_m);
      moves.insert(moves.begin(), tmp);
    }
  }

  int best_score = maximizingAI ? INT_MIN : INT_MAX;
  int best_col   = moves.empty() ? -1 : moves[0].col;

  for (const MoveScore& ms : moves) {
    Board child = drop_piece(b, ms.col, turn);
    MMResult res = minimax_cpp(child, depth - 1, !maximizingAI, alpha, beta, tt, stats);

    if (maximizingAI) {
      if (res.score > best_score) {
        best_score = res.score;
        best_col   = ms.col;
      }
      alpha = std::max(alpha, best_score);
    } else {
      if (res.score < best_score) {
        best_score = res.score;
        best_col   = ms.col;
      }
      beta = std::min(beta, best_score);
    }

    if (beta <= alpha) break;
  }

  tt_store(tt, key, depth, best_score, alpha_orig, beta_orig, best_col);
  return {best_score, best_col};
}

// ============================================================
// Principal variation (PV) extraction from the TT
// ============================================================

static IntegerVector extract_pv(Board b, const TT& tt, int max_depth) {
  std::vector<int> pv;
  for (int d = 0; d < max_depth; d++) {
    if (game_status(b) != 0) break;
    std::string key = tt_key(b);
    auto it = tt.find(key);
    if (it == tt.end() || it->second.move < 0) break;
    int col = it->second.move;
    pv.push_back(col + 1); // 1-indexed
    // Infer the turn: the player with fewer pieces moves
    int p1 = 0, p2 = 0;
    for (int i = 0; i < 42; i++) {
      if (b[i] == 1) p1++;
      else if (b[i] == 2) p2++;
    }
    int player = (p1 <= p2) ? 1 : 2;
    b = drop_piece(b, col, player);
  }
  return wrap(pv);
}

// ============================================================
// Functions exported to R
// ============================================================

//' Minimax engine with alpha-beta pruning and transposition table (C++)
//'
//' @description C++ implementation of the minimax algorithm with alpha-beta
//'   pruning and an internal transposition table. At the horizon it applies a
//'   quiescence search that extends immediate wins and blocks until a
//'   tactically stable position is reached. It also returns the number of
//'   evaluated nodes and the principal
//'   variation extracted from the transposition table.
//'
//' @param board IntegerMatrix 6 x 7. Board state: 0 empty, 1 human, 2 AI.
//' @param depth int. Maximum depth of the search tree.
//' @param maximizingAI bool. \code{TRUE} if it is the AI's turn
//'   (MAX node); \code{FALSE} if it is the human's turn (MIN node).
//'
//' @return List with six elements:
//' \describe{
//'   \item{score}{int. Minimax score of the root position.}
//'   \item{move}{int. Best column (1-7) for the player to move, or
//'     \code{NA} if there are no moves available.}
//'   \item{nodes}{double. Total number of nodes evaluated during the search.}
//'   \item{normal_nodes}{double. Nodes visited by minimax up to the horizon.}
//'   \item{tactical_nodes}{double. Additional nodes visited by the quiescence
//'     search beyond the horizon.}
//'   \item{variation}{IntegerVector. Sequence of columns (1-7) of the principal
//'     variation extracted from the transposition table.}
//' }
//'
//' @examples
//' board <- create_random_position(10)
//' system.time(res <- minimax(board, depth = 7L, maximizingAI = TRUE))
//' res$score
//' res$move
//' res$nodes
//' res$variation
//'
//' @seealso \code{\link{evaluate_position}}, \code{\link{start_game}}
// [[Rcpp::export]]
List minimax(IntegerMatrix board, int depth, bool maximizingAI) {
  Board b = from_r(board);
  TT tt;
  SearchStats stats = {0, 0};
  MMResult res = minimax_cpp(b, depth, maximizingAI,
                             INT_MIN, INT_MAX, tt, stats);
  IntegerVector pv = extract_pv(b, tt, depth);
  // Return a 1-indexed move (like the rest of the package)
  return List::create(
    Named("score")          = res.score,
    Named("move")           = (res.move >= 0) ? (res.move + 1) : NA_INTEGER,
    Named("nodes")          = (double)(stats.normal + stats.tactical),
    Named("normal_nodes")   = (double)stats.normal,
    Named("tactical_nodes") = (double)stats.tactical,
    Named("variation")      = pv
  );
}

//' Static evaluation of a position (C++)
//'
//' @description Evaluates the
//'   board by combining six components: open lines, center control
//'   (positional bitboard), double threats, threat parity, stacked
//'   threats and piece connectivity. The result is always expressed from
//'   the AI's perspective (player 2): positive = AI advantage.
//'
//' @param board IntegerMatrix 6 x 7. Board state: 0 empty, 1 human, 2 AI.
//'
//' @return int. Static score of the position.
//'
//' @examples
//' board <- create_random_position(12)
//' evaluate_position(board)
//'
//' @seealso \code{\link{minimax}}
// [[Rcpp::export]]
int evaluate_position(IntegerMatrix board) {
  return evaluate_position_impl(from_r(board));
}

//' Detect the end of the game (C++)
//'
//' @description Checks whether the board is a terminal state (win or
//'   draw).
//'
//' @param board IntegerMatrix 6 x 7. Board state.
//'
//' @return List with two elements:
//' \describe{
//'   \item{finished}{logical. \code{TRUE} if the game is over.}
//'   \item{result}{int or \code{NA}. If \code{finished}: \code{1} the human
//'     wins, \code{2} the AI wins, \code{0} draw. If the game is not over,
//'     \code{NA}.}
//' }
//'
//' @examples
//' board <- create_random_position(10)
//' game_over(board)
//'
// [[Rcpp::export]]
List game_over(IntegerMatrix board) {
  Board b = from_r(board);
  int go = game_status(b);
  bool finished = (go != 0);
  int result = (go == 3) ? 0 : go; // 0 = draw, 1 = p1, 2 = p2; if not finished, go = 0
  return List::create(
    Named("finished") = finished,
    Named("result")   = finished ? result : NA_INTEGER
  );
}

//' Place a piece on the board (C++)
//'
//' @description Drops a
//'   piece in the given column, respecting gravity (the piece falls to the
//'   lowest free row). If the column is full, the board is returned
//'   unchanged.
//'
//' @param board IntegerMatrix 6 x 7. Board state.
//' @param column int. Column where the piece is dropped (1-7, 1-indexed).
//' @param player int. Player identifier: \code{1} (human) or
//'   \code{2} (AI).
//'
//' @return IntegerMatrix 6 x 7 with the piece inserted.
//'
//' @examples
//' board <- reset_board()
//' board <- make_move(board, column = 4L, player = 1L)
//' board <- make_move(board, column = 4L, player = 2L)
//' visualize_board(board)
//'
// [[Rcpp::export]]
IntegerMatrix make_move(IntegerMatrix board, int column, int player) {
  Board b = from_r(board);
  Board nb = drop_piece(b, column - 1, player); // 1-indexed to 0-indexed
  return to_r(nb);
}

//' Available columns (C++)
//'
//' @description Returns the
//'   indices (1-7) of the columns that are not completely full.
//'
//' @param board IntegerMatrix 6 x 7. Board state.
//'
//' @return IntegerVector with the available column indices (1-indexed).
//'
//' @examples
//' board <- create_random_position(20)
//' available_moves(board)
//'
//' @seealso \code{\link{order_moves}}
// [[Rcpp::export]]
IntegerVector available_moves(IntegerMatrix board) {
  Board b = from_r(board);
  std::vector<int> cols = available_cols(b);
  IntegerVector out(cols.size());
  for (int i = 0; i < (int)cols.size(); i++) out[i] = cols[i] + 1; // 1-indexed
  return out;
}

//' Order moves by heuristic (C++)
//'
//' @description Returns the
//'   available columns ordered from best to worst candidate in order to
//'   maximize the effectiveness of alpha-beta pruning:
//'   \enumerate{
//'     \item Immediate wins for the player to move (score = 1e9).
//'     \item Blocks of an immediate win by the opponent (1e8).
//'     \item The rest, ordered by combined static evaluation (|own_eval|
//'       + |opponent_eval| after the move).
//'   }
//'
//' @param board IntegerMatrix 6 x 7. Board state.
//' @param turn int. Player to move: \code{1} (human) or \code{2} (AI).
//'
//' @return Data frame with columns:
//' \describe{
//'   \item{moves}{int. Column index (1-7).}
//'   \item{score}{double. Heuristic ordering score (higher = better).}
//' }
//'
//' @examples
//' board <- create_random_position(11)
//' order_moves(board, turn = 2L)
//'
//' @seealso \code{\link{minimax}}
// [[Rcpp::export]]
DataFrame order_moves(IntegerMatrix board, int turn) {
  Board b = from_r(board);
  std::vector<MoveScore> ms = order_moves_impl(b, turn);
  IntegerVector moves(ms.size());
  NumericVector scores(ms.size());
  for (int i = 0; i < (int)ms.size(); i++) {
    moves[i]  = ms[i].col + 1;
    scores[i] = ms[i].score;
  }
  return DataFrame::create(
    Named("moves") = moves,
    Named("score") = scores
  );
}
