#include "Persona.h"

struct PersonaSt {
    string nombre;
    int edad;
    Mascota mascota;
};

Persona crearP(string nombre, int edad) {
    PersonaSt* p = new PersonaSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;
}

void crecerD(Persona& p) {
    p->edad++;
}

string nombre(Persona p) {
    return p->nombre;
}

int edad(Persona p) {
    return p->edad;
}

void asignarMascota(Mascota m, Persona& p) {
    p->mascota = m;
}

Mascota getMascota(Persona p) {
    return p->mascota;
}
