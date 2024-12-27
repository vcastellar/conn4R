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



// [[Rcpp::export]]
List minimaxOpt_cpp(NumericMatrix tablero, int profundidad, bool maximizandoIA, 
                    double alpha = NA_REAL, double beta = NA_REAL) {
  
  
  if (alpha == NA_REAL) {
    alpha = -std::numeric_limits<double>::infinity();  // Infinito negativo
  }
  if (beta == NA_REAL) {
    beta = std::numeric_limits<double>::infinity();  // Infinito positivo
  }
  
  
  // Condición de parada (sin realizar evaluación adicional si ya es final)
  if (profundidad == 0 || juegoTerminado(tablero)["finalizado"]) {
    return List::create(
      _["puntuacion"] = evaluarPosicion(tablero),
      _["jugada"] = NA_REAL
    );
  }
  
  // Variables de control
  double mejor_puntuacion = maximizandoIA ? -INFINITY : INFINITY;
  int mejor_jugada = NA_INTEGER;
  
  // Obtener jugadas disponibles
  IntegerVector jugadas = jugadasDisponibles(tablero); 
  
  // Bucle para recorrer las jugadas disponibles
  for (int i = 0; i < jugadas.size(); i++) {
    int columna = jugadas[i];
    
    // Realizar la jugada
    NumericMatrix nuevo_tablero = realizarJugada(tablero, columna, maximizandoIA ? 2 : 1);
    double punt = evaluarPosicion(nuevo_tablero);
    
    // Recursión Minimax
    List res = minimaxOpt_cpp(nuevo_tablero, profundidad - 1, !maximizandoIA, alpha, beta);
    double puntuacion = res["puntuacion"];
    
    // Actualización de puntuación y poda
    if (maximizandoIA) {
      if (puntuacion > mejor_puntuacion) {
        mejor_puntuacion = puntuacion;
        mejor_jugada = columna;
      }
      alpha = std::max(alpha, mejor_puntuacion);
    } else {
      if (puntuacion < mejor_puntuacion) {
        mejor_puntuacion = puntuacion;
        mejor_jugada = columna;
      }
      beta = std::min(beta, mejor_puntuacion);
    }
    
    // Poda Alpha-Beta
    if (beta <= alpha) {
      break;
    }
  }
  
  return List::create(
    _["puntuacion"] = mejor_puntuacion,
    _["jugada"] = mejor_jugada
  );
}


// Implementación de Minimax
// [[Rcpp::export]]
List miniMaxCppOpt(NumericMatrix tablero, int profundidad, bool maximizandoIA, 
                   double alpha = NA_REAL, double beta = NA_REAL) {
  
  if (alpha == NA_REAL) {
    alpha = -std::numeric_limits<double>::infinity();  // Infinito negativo
  }
  if (beta == NA_REAL) {
    beta = std::numeric_limits<double>::infinity();  // Infinito positivo
  }
  
  // Verificar condición de parada
  List juegoInfo = juegoTerminado(tablero);
  bool finalizado = as<bool>(juegoInfo["finalizado"]);
  
  if (profundidad == 0 || finalizado) {
    return List::create(Named("puntuacion") = evaluarPosicion(tablero),
                        Named("jugada")     = R_NilValue);
  }
  
  if (maximizandoIA) {
    double mejor_puntuacion = -INFINITY;
    int mejor_jugada = NA_INTEGER;
    
    // Obtener jugadas disponibles
    IntegerVector jugadas = jugadasDisponibles(tablero);
    
    for (int columna : jugadas) {
      NumericMatrix nuevo_tablero = realizarJugada(tablero, columna, 2);
      double punt = evaluarPosicion(nuevo_tablero);
      
      List res = miniMaxCppOpt(nuevo_tablero, profundidad - 1, false, alpha, beta);
      double puntuacion = as<double>(res["puntuacion"]);
      
      if (puntuacion > mejor_puntuacion) {
        mejor_puntuacion = puntuacion;
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
    
    // Obtener jugadas disponibles
    IntegerVector jugadas = jugadasDisponibles(tablero);
    
    for (int columna : jugadas) {
      NumericMatrix nuevo_tablero = realizarJugada(tablero, columna, 1);
      double punt = evaluarPosicion(nuevo_tablero);
      
      List res = miniMaxCppOpt(nuevo_tablero, profundidad - 1, true, alpha, beta);
      double puntuacion = as<double>(res["puntuacion"]);
      
      if (puntuacion < mejor_puntuacion) {
        mejor_puntuacion = puntuacion;
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

