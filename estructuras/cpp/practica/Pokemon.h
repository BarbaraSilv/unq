#include "Pokemon.cpp"


Pokemon crearPokemon(string nombre, int vida);

string getNombre(Pokemon p);

int getVida(Pokemon p);

void cambiarNombre(Pokemon p, string nombre);

bool estaVivo(Pokemon p);

void restarVida(Pokemon p); //Le resta una unidad a la vida.

void lucharN(int n, Pokemon p, Pokemon r);

void destruir(Pokemon p); //libera memoria
