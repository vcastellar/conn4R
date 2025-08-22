#ifndef CONN4R_EVALUAR_POSICION_H
#define CONN4R_EVALUAR_POSICION_H

#include <Rcpp.h>

namespace conn4R {

  using namespace Rcpp;
  
  // Constantes de puntuación
  const int punt1 = 1;
  const int punt2 = 10;
  const int punt3 = 100;
  const int punt4 = 100000;
  
  // bitboards estático (6x7)
  static NumericMatrix bitboards(6, 7);
  inline void init_bitboards() {
    double vals[42] = {
      0.0, 0.5, 1.0, 1.5, 1.0, 0.5, 0.0,
      0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5,
      1.0, 1.5, 2.0, 2.5, 2.0, 1.5, 1.0,
      1.5, 2.0, 2.5, 3.0, 2.5, 2.0, 1.5,
      2.0, 2.5, 3.0, 3.5, 3.0, 2.5, 2.0,
      0.5, 1.0, 1.5, 2.0, 1.5, 1.0, 0.5
    };
    for (int i = 0; i < 42; i++) {
      bitboards[i] = vals[i];
    }
}

// Generar coordenadas de líneas posibles (horizontal, vertical, diagonales)
inline std::vector<NumericMatrix> generar_coordenadas_lineas_cpp() {
  std::vector<NumericMatrix> lineas;
  
  // Horizontales
  for (int fila = 1; fila <= 6; fila++) {
    for (int col = 1; col <= 4; col++) {
      NumericMatrix coords(4, 2);
      for (int i = 0; i < 4; i++) {
        coords(i, 0) = fila;
        coords(i, 1) = col + i;
      }
      lineas.push_back(coords);
    }
  }
  
  // Verticales
  for (int col = 1; col <= 7; col++) {
    for (int fila = 1; fila <= 3; fila++) {
      NumericMatrix coords(4, 2);
      for (int i = 0; i < 4; i++) {
        coords(i, 0) = fila + i;
        coords(i, 1) = col;
      }
      lineas.push_back(coords);
    }
  }
  
  // Diagonal ↘
  for (int fila = 1; fila <= 3; fila++) {
    for (int col = 1; col <= 4; col++) {
      NumericMatrix coords(4, 2);
      for (int i = 0; i < 4; i++) {
        coords(i, 0) = fila + i;
        coords(i, 1) = col + i;
      }
      lineas.push_back(coords);
    }
  }
  
  // Diagonal ↙
  for (int fila = 1; fila <= 3; fila++) {
    for (int col = 4; col <= 7; col++) {
      NumericMatrix coords(4, 2);
      for (int i = 0; i < 4; i++) {
        coords(i, 0) = fila + i;
        coords(i, 1) = col - i;
      }
      lineas.push_back(coords);
    }
  }
  
  return lineas;
}

// Evalúa una línea
inline int evaluar_linea_cpp(IntegerVector linea, int turno) {
  int n = 0;
  int v = 0;
  for(int i = 0; i < linea.size(); i++) {
    if (linea[i] == turno) n++;
    if (linea[i] == 0) v++;
  }
  
  if (n == 4) return punt4;
  else if (n == 3 && v == 1) return punt3;
  else if (n == 2 && v == 2) return punt2;
  else if (n == 1 && v == 3) return punt1;
  else return 0;
}

// Evalúa el turno
inline int evaluar_turno_cpp(IntegerMatrix tablero, int turno, std::vector<NumericMatrix> &lineas_posibles) {
  int puntuacion = 0;
  
  for (size_t i = 0; i < lineas_posibles.size(); i++) {
    NumericMatrix coords = lineas_posibles[i];
    IntegerVector linea(4);
    
    for (int j = 0; j < 4; j++) {
      int fila = (int)coords(j, 0) - 1; // 0-based index
      int col = (int)coords(j, 1) - 1;
      linea[j] = tablero(fila, col);
    }
    
    puntuacion += evaluar_linea_cpp(linea, turno);
  }
  return puntuacion;
}

} // namespace conn4R

#endif // CONN4R_EVALUAR_POSICION_H