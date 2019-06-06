#include <iostream>
#include "Persona.h"

using namespace std;

int main() {
    Persona p = crearP("Jorge", 32);
    Persona f = crearP("Flor", 32);

    Mascota m = crearM("Rocky", "perro");

    asignarMascota(m, p);
    asignarMascota(m, f);

    cout << "p {" << endl;
    cout << "  nombre: " << nombre(p) << endl;
    cout << "  edad: " << edad(p) << endl;
    cout << "  mascota: " << getNombre(getMascota(p)) << endl;
    cout << "}" << endl;

    cout << endl;
    cout << "f {" << endl;
    cout << "  nombre: " << nombre(f) << endl;
    cout << "  edad: " << edad(f) << endl;
    cout << "  mascota: " << getNombre(getMascota(f)) << endl;
    cout << "}" << endl;

    cout << endl;
    cout << "m {" << endl;
    cout << "  nombre: " << getNombre(m) << endl;
    cout << "}" << endl;

    cout << endl;
    cout << "---------------------------------------------" << endl;
    cout << "---------------------------------------------" << endl;
    cambiarNombre("Coco", m);
    cout << endl;

    cout << "p {" << endl;
    cout << "  nombre: " << nombre(p) << endl;
    cout << "  edad: " << edad(p) << endl;
    cout << "  mascota: " << getNombre(getMascota(p)) << endl;
    cout << "}" << endl;

    cout << endl;
    cout << "f {" << endl;
    cout << "  nombre: " << nombre(f) << endl;
    cout << "  edad: " << edad(f) << endl;
    cout << "  mascota: " << getNombre(getMascota(f)) << endl;
    cout << "}" << endl;

    cout << endl;
    cout << "m {" << endl;
    cout << "  nombre: " << getNombre(m) << endl;
    cout << "}" << endl;
}
