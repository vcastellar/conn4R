// #include <Rcpp.h>
// using namespace Rcpp;
// 
// // Clase "arbol"
// class Arbol {
// public:
//   IntegerVector idNodo;
//   IntegerVector jugada;
//   LogicalVector turno;
//   IntegerVector profundidad;
//   NumericVector puntuacion;
//   
//   // Constructor
//   Arbol() {}
//   
//   // Método para actualizar el objeto
//   void actualizar(bool turno_val, int jugada_val, int profundidad_val, double puntuacion_val) {
//     turno.push_back(turno_val);
//     jugada.push_back(jugada_val);
//     profundidad.push_back(profundidad_val);
//     idNodo.push_back(idNodo.size() == 0 ? 0 : max(idNodo) + 1);
//     puntuacion.push_back(puntuacion_val);
//   }
//   
//   // Método para actualizar el último nodo
//   void actUltNodo(std::string slotName, double value) {
//     if (slotName == "idNodo") {
//       idNodo[idNodo.size() - 1] = static_cast<int>(value);
//     } else if (slotName == "jugada") {
//       jugada[jugada.size() - 1] = static_cast<int>(value);
//     } else if (slotName == "turno") {
//       turno[turno.size() - 1] = static_cast<bool>(value);
//     } else if (slotName == "profundidad") {
//       profundidad[profundidad.size() - 1] = static_cast<int>(value);
//     } else if (slotName == "puntuacion") {
//       puntuacion[puntuacion.size() - 1] = value;
//     } else {
//       stop("Invalid slot name");
//     }
//   }
// 
//   
//   // Método para mostrar el contenido del objeto
//   std::string mostrar() {
//     std::ostringstream oss;
//     oss << "idNodo: " << idNodo << "\n";
//     oss << "jugada: " << jugada << "\n";
//     oss << "turno: " << turno << "\n";
//     oss << "profundidad: " << profundidad << "\n";
//     oss << "puntuacion: " << puntuacion << "\n";
//     return oss.str();
//   }
//   
// 
// };
// 
// // Exponer la clase y sus métodos a R
// RCPP_MODULE(ArbolModule) {
//   class_<Arbol>("Arbol")
//   .constructor()
//   .method("actualizar", &Arbol::actualizar, "Actualizar el objeto")
//   .method("actUltNodo", &Arbol::actUltNodo, "Actualizar el último nodo")
//   .method("mostrar", &Arbol::mostrar, "Mostrar el contenido del objeto");
// }


