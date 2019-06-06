#include <iostream>

using namespace std;

struct MascotaSt;
typedef MascotaSt* Mascota;

Mascota crearM(string nombre, string especie);
string getNombre(Mascota m);
void cambiarNombre(string nombre, Mascota& m);
void destruir(Mascota& m);
