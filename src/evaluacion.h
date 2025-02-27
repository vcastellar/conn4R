#ifndef EVALUACION_H
#define EVALUACION_H

#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericMatrix crearBitboards() {
  NumericMatrix bitboards(6, 7);
  
  for (int row = 0; row < 6; row++) {
    for (int col = 0; col < 7; col++) {
      if (col == 1 || col == 5) {
        bitboards(row, col) += 0.5;
      }
      if (col == 2 || col == 4) {
        bitboards(row, col) += 1;
      }
      if (col == 3) {
        bitboards(row, col) += 1.5;
      }
      bitboards(row, col) += 0.5 * row;
    }
  }
  
  return bitboards;
}

// [[Rcpp::export]]
double evaluarLinea(NumericVector linea, int turno) {
  double puntuacion = 0;
  
  // Calcular cuentaTurno y cuentaVacio manualmente
  int cuentaTurno = 0;
  int cuentaVacio = 0;
  
  for (int i = 0; i < linea.size(); i++) {
    if (linea[i] == turno) {
      cuentaTurno++;
    } else if (linea[i] == 0) {
      cuentaVacio++;
    }
  }
  
  // Evaluar las condiciones de puntuación
  if (cuentaTurno == 4) {
    puntuacion = 100000;
  } else if (cuentaTurno == 3 && cuentaVacio == 1) {
    puntuacion = 100;
  } else if (cuentaTurno == 2 && cuentaVacio == 2) {
    puntuacion = 10;
  } else if (cuentaTurno == 1 && cuentaVacio == 3) {
    puntuacion = 1;
  }
  
  // Aplicar el factor de turno
  puntuacion *= pow(-1, turno);
  return puntuacion;
}


// [[Rcpp::export]]
double evaluarTurno(NumericMatrix tablero, int turno) {
  double puntuacion = 0;
  
  // Evaluar líneas horizontales
  for (int fila = 0; fila < 6; fila++) {
    for (int columna = 0; columna <= 3; columna++) {
      NumericVector linea(4);
      for (int k = 0; k < 4; k++) {
        linea[k] = tablero(fila, columna + k);
      }
      puntuacion += evaluarLinea(linea, turno);
    }
  }
  
  // Evaluar líneas verticales
  for (int columna = 0; columna < 7; columna++) {
    for (int fila = 0; fila <= 2; fila++) {
      NumericVector linea(4);
      for (int k = 0; k < 4; k++) {
        linea[k] = tablero(fila + k, columna);
      }
      puntuacion += evaluarLinea(linea, turno);
    }
  }
  
  // Evaluar líneas diagonales (de izquierda a derecha)
  for (int fila = 0; fila <= 2; fila++) {
    for (int columna = 0; columna <= 3; columna++) {
      NumericVector linea = {
        tablero(fila, columna),
        tablero(fila + 1, columna + 1),
        tablero(fila + 2, columna + 2),
        tablero(fila + 3, columna + 3)
      };
      puntuacion += evaluarLinea(linea, turno);
    }
  }
  
  // Evaluar líneas diagonales (de derecha a izquierda)
  for (int fila = 0; fila <= 2; fila++) {
    for (int columna = 3; columna < 7; columna++) {
      NumericVector linea = {
        tablero(fila, columna),
        tablero(fila + 1, columna - 1),
        tablero(fila + 2, columna - 2),
        tablero(fila + 3, columna - 3)
      };
      puntuacion += evaluarLinea(linea, turno);
    }
  }
  
  return puntuacion;
}

// [[Rcpp::export]]
double evaluarPosicion(NumericMatrix tablero) {
  NumericMatrix bitboards = crearBitboards();
  
  // Convertir el resultado de las comparaciones a NumericMatrix
  NumericMatrix tableroJugador2(tablero.nrow(), tablero.ncol());
  NumericMatrix tableroJugador1(tablero.nrow(), tablero.ncol());
  
  for (int i = 0; i < tablero.nrow(); i++) {
    for (int j = 0; j < tablero.ncol(); j++) {
      tableroJugador2(i, j) = (tablero(i, j) == 2) ? 1.0 : 0.0;
      tableroJugador1(i, j) = (tablero(i, j) == 1) ? 1.0 : 0.0;
    }
  }
  
  // Multiplicación elemento a elemento y suma total
  double sumaJugador2 = 0.0, sumaJugador1 = 0.0;
  for (int i = 0; i < tablero.nrow(); i++) {
    for (int j = 0; j < tablero.ncol(); j++) {
      sumaJugador2 += bitboards(i, j) * tableroJugador2(i, j);
      sumaJugador1 += bitboards(i, j) * tableroJugador1(i, j);
    }
  }
  
  // Calcular puntaje final
  double puntBit = sumaJugador2 - sumaJugador1;
  
  return evaluarTurno(tablero, 1) + evaluarTurno(tablero, 2) + puntBit;
}


// [[Rcpp::export]]
inline IntegerVector jugadasDisponibles(NumericMatrix tablero) {
  // Encuentra las columnas que no están llenas
  std::vector<int> columnas_disponibles;
  for (int col = 0; col < tablero.ncol(); ++col) {
    if (tablero(0, col) == 0) {
      columnas_disponibles.push_back(col + 1); // Sumar 1 para ajustar a la indexación de R
    }
  }
  return wrap(columnas_disponibles);
}


// [[Rcpp::export]]
inline List juegoTerminado(NumericMatrix tablero) {
  // Verificar si hay 4 en línea para el jugador 1 o 2
  for (int jugador = 1; jugador <= 2; ++jugador) {
    if (std::abs(evaluarTurno(tablero, jugador)) >= 100000) {
      if (jugador == 1) {
        return List::create(
          Named("finalizado") = true,
          Named("resultado") = "WIN HUMAN"
        );
      } else {
        return List::create(
          Named("finalizado") = true,
          Named("resultado") = "WIN IA"
        );
      }
    }
  }
  
  // Verificar si hay jugadas disponibles
  if (jugadasDisponibles(tablero).size() == 0) {
    return List::create(
      Named("finalizado") = true,
      Named("resultado") = "DRAW"
    );
  }
  
  // Si no se cumple ninguna de las condiciones anteriores, el juego no ha terminado
  return List::create(
    Named("finalizado") = false,
    Named("resultado") = R_NilValue
  );
}



// [[Rcpp::export]]
NumericMatrix realizarJugada(NumericMatrix tablero, int columna, int jugador) {
  NumericMatrix tableroAux = clone(tablero);
  for (int fila = tableroAux.nrow() - 1; fila >= 0; --fila) {
    if (tableroAux(fila, columna - 1) == 0) { // Restar 1 para ajustar la indexación de C++
      tableroAux(fila, columna - 1) = jugador;
      return tableroAux;
    }
  }
  // Si la columna está llena, no se realiza ninguna jugada
  return tableroAux;
}

#endif // EVALUACION_H