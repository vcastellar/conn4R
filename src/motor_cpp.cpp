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
// Constantes
// ============================================================

static const int PUNT1 = 1;
static const int PUNT2 = 10;
static const int PUNT3 = 1000;
static const int PUNT4 = 100000;

static const int BONUS_PARIDAD = 300;
static const int BONUS_APILADA = 8000;
static const int BONUS_CONEXION = 3;
static const int MATE_SCORE = 1000000000;
static const double WIN_SCORE  = 1e9;
static const double BLOCK_SCORE = 1e8;

// Bitboard posicional (fila-mayor, igual que R: fila 1 = arriba, fila 6 = abajo)
static const int BITBOARD[6][7] = {
  {0, 1, 2, 3, 2, 1, 0},
  {1, 2, 3, 4, 3, 2, 1},
  {2, 3, 4, 5, 4, 3, 2},
  {3, 4, 5, 6, 5, 4, 3},
  {4, 5, 6, 7, 6, 5, 4},
  {1, 2, 3, 4, 3, 2, 1}
};

// ============================================================
// Representación del tablero como array plano [fila*7+col], 0-based
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
// Líneas precalculadas: 69 líneas x 4 celdas (índices planos 0-based)
// ============================================================

struct Lines {
  int idx[69][4];
  int row[69][4]; // fila 0-based de cada celda
  int col[69][4]; // col 0-based de cada celda
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
    // Horizontales
    for (int r = 0; r < 6; r++)
      for (int c = 0; c <= 3; c++) add(r, c, 0, 1);
    // Verticales
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
// Funciones básicas del tablero
// ============================================================

inline bool col_available(const Board& b, int c) {
  return b[c] == 0; // fila 0 = arriba
}

inline Board make_move(Board b, int col, int player) {
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
// Evaluación estática
// ============================================================

// Evalúa líneas abiertas de un jugador (sin piezas del oponente)
static int evaluar_turno(const Board& b, int turno) {
  int oponente = (turno == 1) ? 2 : 1;
  int total = 0;
  for (int l = 0; l < LINES.n; l++) {
    int n = 0, v = 0, blk = 0;
    for (int i = 0; i < 4; i++) {
      int val = b[LINES.idx[l][i]];
      if (val == turno) n++;
      else if (val == 0) v++;
      else blk++;
    }
    if (blk > 0) continue; // línea bloqueada
    if      (n == 4)              total += PUNT4;
    else if (n == 3 && v == 1)    total += PUNT3;
    else if (n == 2 && v == 2)    total += PUNT2;
    else if (n == 1 && v == 3)    total += PUNT1;
  }
  return total;
}

// Detecta si el juego terminó y quién ganó
// Devuelve: 0=continúa, 1=gana jugador1, 2=gana jugador2, 3=empate
static int game_over(const Board& b) {
  if (evaluar_turno(b, 1) >= 50000) return 1;
  if (evaluar_turno(b, 2) >= 50000) return 2;
  if (available_cols(b).empty())    return 3;
  return 0;
}

// Los estados terminales deben dominar siempre a la evaluación heurística.
// Se usa el número de huecos para preferir victorias rápidas y retrasar derrotas;
// al depender solo del tablero, la puntuación sigue siendo segura para la TT.
static int terminal_score(const Board& b, int resultado) {
  if (resultado == 3) return 0;
  int huecos = 0;
  for (int i = 0; i < 42; i++)
    if (b[i] == 0) huecos++;
  return (resultado == 2) ? MATE_SCORE + huecos : -MATE_SCORE - huecos;
}

struct ThreatDetail {
  int count, pares, impares, apiladas;
};

static ThreatDetail contar_amenazas(const Board& b, int turno) {
  ThreatDetail d = {0, 0, 0, 0};
  int cols_amenaza[42];
  int nc = 0;

  for (int l = 0; l < LINES.n; l++) {
    int n = 0, v = 0, empty_pos = -1;
    for (int i = 0; i < 4; i++) {
      int val = b[LINES.idx[l][i]];
      if (val == turno) n++;
      else if (val == 0) { v++; empty_pos = i; }
    }
    if (n != 3 || v != 1) continue;

    int fila_mat = LINES.row[l][empty_pos]; // 0-based
    int col_mat  = LINES.col[l][empty_pos]; // 0-based
    // Celda accesible: fila más baja (r==5) o celda de abajo ocupada
    bool accesible = (fila_mat == 5) || (b[(fila_mat + 1) * 7 + col_mat] != 0);
    if (!accesible) continue;

    d.count++;
    cols_amenaza[nc++] = col_mat;
    // fila_desde_abajo = 5 - fila_mat (0-based desde abajo, 0..5)
    // pero en R era 7 - fila_mat (1-based), ahora: (6 - fila_mat) en 1-based = 5-fila_mat+1
    int fila_desde_abajo = 6 - fila_mat; // 1..6
    if (fila_desde_abajo % 2 == 0) d.pares++;
    else                            d.impares++;
  }

  // Amenazas apiladas: 2+ amenazas en la misma columna
  if (nc > 1) {
    int freq[7] = {0};
    for (int i = 0; i < nc; i++) freq[cols_amenaza[i]]++;
    for (int c = 0; c < 7; c++) if (freq[c] >= 2) d.apiladas++;
  }
  return d;
}

static int bonus_conectividad(const Board& b, int turno) {
  static const int DR[] = {0, 1, 1,  1};
  static const int DC[] = {1, 0, 1, -1};
  int total = 0;
  for (int r = 0; r < 6; r++) {
    for (int c = 0; c < 7; c++) {
      if (b[r * 7 + c] != turno) continue;
      for (int d = 0; d < 4; d++) {
        int nr = r + DR[d], nc = c + DC[d];
        if (nr >= 0 && nr < 6 && nc >= 0 && nc < 7 && b[nr * 7 + nc] == turno)
          total += BONUS_CONEXION;
      }
    }
  }
  return total;
}

static int evaluar_posicion_impl(const Board& b) {
  // Bitboard posicional
  int puntBit = 0;
  for (int r = 0; r < 6; r++)
    for (int c = 0; c < 7; c++) {
      int v = b[r * 7 + c];
      if      (v == 2) puntBit += BITBOARD[r][c];
      else if (v == 1) puntBit -= BITBOARD[r][c];
    }

  int eval_diff = evaluar_turno(b, 2) - evaluar_turno(b, 1);

  ThreatDetail am_ia  = contar_amenazas(b, 2);
  ThreatDetail am_hum = contar_amenazas(b, 1);

  int bonus_doble   = (am_ia.count  >= 2) ? PUNT3 * 10 : 0;
  int penalty_doble = (am_hum.count >= 2) ? PUNT3 * 10 : 0;

  int bonus_paridad = (am_ia.pares  - am_ia.impares)  * BONUS_PARIDAD
                    - (am_hum.pares - am_hum.impares)  * BONUS_PARIDAD;

  int bonus_apilada   = am_ia.apiladas  * BONUS_APILADA;
  int penalty_apilada = am_hum.apiladas * BONUS_APILADA;

  int conex = bonus_conectividad(b, 2) - bonus_conectividad(b, 1);

  return eval_diff + puntBit
       + bonus_doble - penalty_doble
       + bonus_paridad
       + bonus_apilada - penalty_apilada
       + conex;
}

// ============================================================
// Ordenación de jugadas
// ============================================================

struct MoveScore {
  int col;
  double score;
};

static std::vector<MoveScore> ordenar_jugadas_impl(const Board& b, int turno) {
  std::vector<int> cols = available_cols(b);
  int oponente = (turno == 1) ? 2 : 1;
  std::vector<MoveScore> ms;

  for (int c : cols) {
    double score;
    Board bj = make_move(b, c, turno);
    int go_j = game_over(bj);
    if (go_j == turno) {
      score = WIN_SCORE;
    } else {
      Board bo = make_move(b, c, oponente);
      int go_o = game_over(bo);
      if (go_o == oponente) {
        score = BLOCK_SCORE;
      } else {
        score = std::abs((double)evaluar_posicion_impl(bj)) + std::abs((double)evaluar_posicion_impl(bo));
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
// Tabla de transposición
// ============================================================

static const int TT_EXACT = 0;
static const int TT_LOWER = 1;
static const int TT_UPPER = 2;

struct TTEntry {
  int prof;
  int punt;
  int flag;
  int jugada; // col 0-based, -1 si desconocida
};

typedef std::unordered_map<std::string, TTEntry> TT;

static std::string tt_key(const Board& b) {
  std::string k(42, '0');
  for (int i = 0; i < 42; i++) k[i] = '0' + b[i];
  return k;
}

static const TTEntry* tt_lookup(const TT& tt, const std::string& key,
                                 int prof, int alpha, int beta) {
  auto it = tt.find(key);
  if (it == tt.end()) return nullptr;
  const TTEntry& e = it->second;
  if (e.prof < prof) return nullptr;
  if (e.flag == TT_EXACT) return &e;
  if (e.flag == TT_LOWER && e.punt >= beta)  return &e;
  if (e.flag == TT_UPPER && e.punt <= alpha) return &e;
  return nullptr;
}

static void tt_store(TT& tt, const std::string& key,
                     int prof, int punt, int alpha_orig, int beta_orig, int jugada) {
  int flag;
  if      (punt >= beta_orig)  flag = TT_LOWER;
  else if (punt <= alpha_orig) flag = TT_UPPER;
  else                         flag = TT_EXACT;

  auto it = tt.find(key);
  if (it == tt.end() || it->second.prof <= prof) {
    tt[key] = {prof, punt, flag, jugada};
  }
}

// ============================================================
// Minimax con alpha-beta y tabla de transposición
// ============================================================

struct MMResult {
  int puntuacion;
  int jugada; // col 0-based, -1 si no hay
};

static MMResult minimax_cpp(const Board& b, int prof, bool maximizandoIA,
                             int alpha, int beta, TT& tt, long long& nodes) {
  nodes++;

  int go = game_over(b);
  if (go != 0) return {terminal_score(b, go), -1};
  if (prof == 0) return {evaluar_posicion_impl(b), -1};

  std::string key = tt_key(b);
  const TTEntry* hit = tt_lookup(tt, key, prof, alpha, beta);
  if (hit != nullptr) {
    return {hit->punt, hit->jugada};
  }

  int alpha_orig = alpha;
  int beta_orig  = beta;

  int turno = maximizandoIA ? 2 : 1;
  std::vector<MoveScore> moves = ordenar_jugadas_impl(b, turno);

  // Si la TT tiene mejor jugada, moverla al frente
  auto it_tt = tt.find(key);
  if (it_tt != tt.end() && it_tt->second.jugada >= 0) {
    int best_col = it_tt->second.jugada;
    auto it_m = std::find_if(moves.begin(), moves.end(),
                             [best_col](const MoveScore& m){ return m.col == best_col; });
    if (it_m != moves.end() && it_m != moves.begin()) {
      MoveScore tmp = *it_m;
      moves.erase(it_m);
      moves.insert(moves.begin(), tmp);
    }
  }

  int mejor_punt = maximizandoIA ? INT_MIN : INT_MAX;
  int mejor_col  = moves.empty() ? -1 : moves[0].col;

  for (const MoveScore& ms : moves) {
    Board nuevo = make_move(b, ms.col, turno);
    MMResult res = minimax_cpp(nuevo, prof - 1, !maximizandoIA, alpha, beta, tt, nodes);

    if (maximizandoIA) {
      if (res.puntuacion > mejor_punt) {
        mejor_punt = res.puntuacion;
        mejor_col  = ms.col;
      }
      alpha = std::max(alpha, mejor_punt);
    } else {
      if (res.puntuacion < mejor_punt) {
        mejor_punt = res.puntuacion;
        mejor_col  = ms.col;
      }
      beta = std::min(beta, mejor_punt);
    }

    if (beta <= alpha) break;
  }

  tt_store(tt, key, prof, mejor_punt, alpha_orig, beta_orig, mejor_col);
  return {mejor_punt, mejor_col};
}

// ============================================================
// Extracción de variante principal (PV) desde la TT
// ============================================================

static IntegerVector extract_pv(Board b, const TT& tt, int max_depth) {
  std::vector<int> pv;
  for (int d = 0; d < max_depth; d++) {
    if (game_over(b) != 0) break;
    std::string key = tt_key(b);
    auto it = tt.find(key);
    if (it == tt.end() || it->second.jugada < 0) break;
    int col = it->second.jugada;
    pv.push_back(col + 1); // 1-indexed
    // Inferir turno: jugador con menos piezas mueve
    int p1 = 0, p2 = 0;
    for (int i = 0; i < 42; i++) {
      if (b[i] == 1) p1++;
      else if (b[i] == 2) p2++;
    }
    int player = (p1 <= p2) ? 1 : 2;
    b = make_move(b, col, player);
  }
  return wrap(pv);
}

// ============================================================
// Funciones exportadas a R
// ============================================================

//' Motor minimax con alpha-beta y tabla de transposición (C++)
//'
//' @description Implementación C++ del algoritmo minimax con poda alpha-beta y
//'   tabla de transposición interna. Devuelve además el conteo de nodos
//'   evaluados y la variante
//'   principal extraída de la tabla de transposición.
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero: 0 vacío, 1 humano, 2 IA.
//' @param profundidad int. Profundidad máxima del árbol de búsqueda.
//' @param maximizandoIA bool. \code{TRUE} si el turno actual es de la IA
//'   (nodo MAX); \code{FALSE} si es del humano (nodo MIN).
//'
//' @return Lista con cuatro elementos:
//' \describe{
//'   \item{puntuacion}{int. Puntuación minimax de la posición raíz.}
//'   \item{jugada}{int. Columna óptima (1-7) para el jugador en turno, o
//'     \code{NA} si no hay jugadas disponibles.}
//'   \item{nodos}{double. Número total de nodos evaluados durante la búsqueda.}
//'   \item{variante}{IntegerVector. Secuencia de columnas (1-7) de la variante
//'     principal extraída de la tabla de transposición.}
//' }
//'
//' @examples
//' tablero <- crear_posicion_aleatoria(10)
//' system.time(res <- minimax(tablero, profundidad = 7L, maximizandoIA = TRUE))
//' res$puntuacion
//' res$jugada
//' res$nodos
//' res$variante
//'
//' @seealso \code{\link{evaluar_posicion}}, \code{\link{iniciar_partida}}
// [[Rcpp::export]]
List minimax(IntegerMatrix tablero, int profundidad, bool maximizandoIA) {
  Board b = from_r(tablero);
  TT tt;
  long long nodes = 0;
  MMResult res = minimax_cpp(b, profundidad, maximizandoIA, INT_MIN, INT_MAX, tt, nodes);
  IntegerVector pv = extract_pv(b, tt, profundidad);
  // Devolver jugada 1-indexada (como el resto del paquete)
  return List::create(
    Named("puntuacion") = res.puntuacion,
    Named("jugada")     = (res.jugada >= 0) ? (res.jugada + 1) : NA_INTEGER,
    Named("nodos")      = (double)nodes,
    Named("variante")   = pv
  );
}

//' Evaluación estática de una posición (C++)
//'
//' @description Evalúa el
//'   tablero combinando seis componentes: líneas abiertas, control de centro
//'   (bitboard posicional), amenazas dobles, paridad de amenazas, amenazas
//'   apiladas y conectividad de piezas. El resultado se expresa siempre desde
//'   la perspectiva de la IA (jugador 2): positivo = ventaja IA.
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero: 0 vacío, 1 humano, 2 IA.
//'
//' @return int. Puntuación estática de la posición.
//'
//' @examples
//' tablero <- crear_posicion_aleatoria(12)
//' evaluar_posicion(tablero)
//'
//' @seealso \code{\link{minimax}}
// [[Rcpp::export]]
int evaluar_posicion(IntegerMatrix tablero) {
  return evaluar_posicion_impl(from_r(tablero));
}

//' Detectar fin de partida (C++)
//'
//' @description Comprueba si el tablero es un estado terminal (victoria o
//'   empate).
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero.
//'
//' @return Lista con dos elementos:
//' \describe{
//'   \item{finalizado}{logical. \code{TRUE} si la partida ha concluido.}
//'   \item{resultado}{int o \code{NA}. Si \code{finalizado}: \code{1} gana
//'     el humano, \code{2} gana la IA, \code{0} empate. Si no ha terminado,
//'     \code{NA}.}
//' }
//'
//' @examples
//' tablero <- crear_posicion_aleatoria(10)
//' juego_terminado(tablero)
//'
// [[Rcpp::export]]
List juego_terminado(IntegerMatrix tablero) {
  Board b = from_r(tablero);
  int go = game_over(b);
  bool finalizado = (go != 0);
  int resultado = (go == 3) ? 0 : go; // 0=empate, 1=j1, 2=j2, si no finalizado go=0
  return List::create(
    Named("finalizado") = finalizado,
    Named("resultado")  = finalizado ? resultado : NA_INTEGER
  );
}

//' Colocar una ficha en el tablero (C++)
//'
//' @description Deposita una
//'   ficha en la columna indicada, respetando la gravedad (la pieza cae a la
//'   fila libre más baja). Si la columna está llena devuelve el tablero sin
//'   modificar.
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero.
//' @param columna int. Columna donde se deposita la ficha (1-7, 1-indexado).
//' @param jugador int. Identificador del jugador: \code{1} (humano) o
//'   \code{2} (IA).
//'
//' @return IntegerMatrix 6×7 con la ficha insertada.
//'
//' @examples
//' tablero <- reiniciar_tablero()
//' tablero <- realizar_jugada(tablero, columna = 4L, jugador = 1L)
//' tablero <- realizar_jugada(tablero, columna = 4L, jugador = 2L)
//' visualizar_tablero(tablero)
//'
// [[Rcpp::export]]
IntegerMatrix realizar_jugada(IntegerMatrix tablero, int columna, int jugador) {
  Board b = from_r(tablero);
  Board nb = make_move(b, columna - 1, jugador); // 1-indexed a 0-indexed
  return to_r(nb);
}

//' Columnas disponibles (C++)
//'
//' @description Devuelve los
//'   índices (1-7) de las columnas que no están completamente llenas.
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero.
//'
//' @return IntegerVector con los índices de columna disponibles (1-indexados).
//'
//' @examples
//' tablero <- crear_posicion_aleatoria(20)
//' jugadas_disponibles(tablero)
//'
//' @seealso \code{\link{ordenar_jugadas}}
// [[Rcpp::export]]
IntegerVector jugadas_disponibles(IntegerMatrix tablero) {
  Board b = from_r(tablero);
  std::vector<int> cols = available_cols(b);
  IntegerVector out(cols.size());
  for (int i = 0; i < (int)cols.size(); i++) out[i] = cols[i] + 1; // 1-indexed
  return out;
}

//' Ordenar jugadas por heurística (C++)
//'
//' @description Devuelve las
//'   columnas disponibles ordenadas de mejor a peor candidata para maximizar
//'   la eficacia de la poda alpha-beta:
//'   \enumerate{
//'     \item Victorias inmediatas del jugador en turno (puntuación = 1e9).
//'     \item Bloqueos de victoria inmediata del oponente (1e8).
//'     \item Resto ordenado por evaluación estática combinada (|eval_propia|
//'       + |eval_oponente| tras el movimiento).
//'   }
//'
//' @param tablero IntegerMatrix 6×7. Estado del tablero.
//' @param turno int. Jugador en turno: \code{1} (humano) o \code{2} (IA).
//'
//' @return Data frame con columnas:
//' \describe{
//'   \item{jugadas}{int. Índice de columna (1-7).}
//'   \item{puntuacion}{double. Puntuación heurística de ordenación (mayor = mejor).}
//' }
//'
//' @examples
//' tablero <- crear_posicion_aleatoria(11)
//' ordenar_jugadas(tablero, turno = 2L)
//'
//' @seealso \code{\link{minimax}}
// [[Rcpp::export]]
DataFrame ordenar_jugadas(IntegerMatrix tablero, int turno) {
  Board b = from_r(tablero);
  std::vector<MoveScore> ms = ordenar_jugadas_impl(b, turno);
  IntegerVector jugadas(ms.size());
  NumericVector puntuaciones(ms.size());
  for (int i = 0; i < (int)ms.size(); i++) {
    jugadas[i]     = ms[i].col + 1;
    puntuaciones[i] = ms[i].score;
  }
  return DataFrame::create(
    Named("jugadas")    = jugadas,
    Named("puntuacion") = puntuaciones
  );
}
