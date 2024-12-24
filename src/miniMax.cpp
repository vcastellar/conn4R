#include <Rcpp.h>
#include "evaluacion.h"
using namespace Rcpp;


// Implementación de Minimax
// [[Rcpp::export]]
List miniMaxCpp(NumericMatrix tablero, int profundidad, bool maximizandoIA,
                double alpha = NA_REAL, double beta = NA_REAL) {

  if (alpha == NA_REAL) {
    alpha = -std::numeric_limits<double>::infinity();  // Infinito negativo
  }
  if (beta == NA_REAL) {
    beta = std::numeric_limits<double>::infinity();  // Infinito positivo
  }

  // Verificar condición de parada
  if (profundidad == 0 || as<List>(juegoTerminado(tablero))["finalizado"]) {
    return List::create(Named("puntuacion") = evaluarPosicion(tablero),
                        Named("jugada") = R_NilValue);
  }

  if (maximizandoIA) {
    double mejor_puntuacion = -INFINITY;
    int mejor_jugada = NA_INTEGER;

    for (int columna : jugadasDisponibles(tablero)) {
      NumericMatrix nuevo_tablero = realizarJugada(tablero, columna, 2);
      double punt = evaluarPosicion(nuevo_tablero);

      List res = miniMaxCpp(nuevo_tablero, profundidad - 1, false, alpha, beta);

      if (as<double>(res["puntuacion"]) > mejor_puntuacion) {
        mejor_puntuacion = res["puntuacion"];
        mejor_jugada = columna;
      }

      alpha = std::max(alpha, mejor_puntuacion);
      if (beta <= alpha) {
        break;  // Poda alpha-beta
      }
    }

    return List::create(Named("puntuacion") = mejor_puntuacion,
                        Named("jugada") = mejor_jugada);

  } else {
    double mejor_puntuacion = INFINITY;
    int mejor_jugada = NA_INTEGER;

    for (int columna : jugadasDisponibles(tablero)) {
      NumericMatrix nuevo_tablero = realizarJugada(tablero, columna, 1);
      double punt = evaluarPosicion(nuevo_tablero);

      List res = miniMaxCpp(nuevo_tablero, profundidad - 1, true, alpha, beta);

      if (as<double>(res["puntuacion"]) < mejor_puntuacion) {
        mejor_puntuacion = res["puntuacion"];
        mejor_jugada = columna;
      }

      beta = std::min(beta, mejor_puntuacion);
      if (beta <= alpha) {
        break;  // Poda alpha-beta
      }
    }

    return List::create(Named("puntuacion") = mejor_puntuacion,
                        Named("jugada") = mejor_jugada);
  }
}
