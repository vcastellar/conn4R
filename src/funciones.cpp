// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include "evaluar.h"
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector jugadas_disponibles_cpp(IntegerMatrix tablero) {
  IntegerVector jugadas_candidatas;
  int ncol = tablero.ncol();
  
  for (int col = 0; col < ncol; col++) {
    if (tablero(0, col) == 0) {  // fila 1 en R es índice 0 en C++
      jugadas_candidatas.push_back(col + 1); // columnas 1-indexadas como en R
    }
  }
  
  return jugadas_candidatas;
}


// [[Rcpp::export]]
IntegerMatrix realizar_jugada_cpp(IntegerMatrix tablero, int columna, int jugador) {
  int col = columna - 1; // Ajustar índice a 0-based
  
  for (int fila = tablero.nrow() - 1; fila >= 0; fila--) {
    if (tablero(fila, col) == 0) {
      tablero(fila, col) = jugador;
      return tablero;
    }
  }
  
  // Si la columna está llena, devolver el tablero sin cambios
  return tablero;
}


// [[Rcpp::export]]
int suma(int x, int y) {
  return x + y;
}

// [[Rcpp::export]]
DataFrame ordenar_jugadas_cpp(IntegerMatrix tablero, int turno) {
  
  // Obtener jugadas candidatas (deberías portar jugadas_disponibles aquí)
  std::vector<int> jugadas_candidatas;
  for (int col = 0; col < tablero.ncol(); col++) {
    // Ejemplo: si hay un cero en la columna, la jugada es válida
    for (int row = 0; row < tablero.nrow(); row++) {
      if (tablero(row, col) == 0) {
        jugadas_candidatas.push_back(col + 1); // columnas 1-indexed
        break;
      }
    }
  }
  
  
  int oponente = (turno == 1) ? 2 : 1;
  NumericVector puntuaciones(jugadas_candidatas.size());
  
  for (size_t i = 0; i < jugadas_candidatas.size(); i++) {
    int col = jugadas_candidatas[i] - 1; // índice 0-based en C++
    
    // ====== Jugada del jugador actual ======
    IntegerMatrix tablero_j = clone(tablero);
    realizar_jugada_cpp(tablero_j, col, turno);
    double eval_j = 0; // evaluar_posicion_cpp(tablero_j);
    
    // ====== Jugada del oponente ======
    IntegerMatrix tablero_o = clone(tablero);
    realizar_jugada_cpp(tablero_o, col, oponente);
    double eval_o = 0; // evaluar_posicion_cpp(tablero_o);
    
    puntuaciones[i] = std::abs(eval_j) + std::abs(eval_o);
  }
  
  // Retornar DataFrame ordenado en R (o podemos ordenar aquí en C++)
  return DataFrame::create(
    Named("jugadas") = jugadas_candidatas,
    Named("puntuacion") = puntuaciones
  );
}

