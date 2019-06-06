#include "Mascota.h"

struct MascotaSt {
    string nombre;
    string especie;
};

Mascota crearM(string nombre, string especie) {
    MascotaSt* m = new MascotaSt;
    m->nombre = nombre;
    m->especie = especie;
    return m;
}

string getNombre(Mascota m) {
    return m->nombre;
}

void cambiarNombre(string nombre, Mascota& m) {
    m->nombre = nombre;
}

void destruir(Mascota& m) {
    delete m;
}
