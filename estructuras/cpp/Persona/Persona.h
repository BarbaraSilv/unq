#include <iostream>
#include "Mascota.h"

using namespace std;

struct PersonaSt;

typedef PersonaSt* Persona;

Persona crearP(string nombre, int edad);

string nombre(Persona p);

int edad(Persona p);

void crecerD(Persona& p);

void asignarMascota(Mascota m, Persona& p);

Mascota getMascota(Persona p);
