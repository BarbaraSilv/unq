struct PokemonSt {
    string nombre;
    int vida;
};

/// INVARIANTE DE REPRESENTACION
/// LA VIDA ES UN NUMERO POSITIVO

typedef PokemonSt* Pokemon;

Pokemon crearP(string nombre, int vida);
string getNombre(Pokemon p);
int getVida(Pokemon p);
bool estaVivo(Pokemon p);
void restarUnoAVida(Pokemon& p);
void lucharN(int n, Pokemon& p1, Pokemon& p2); ///O(1)
void destruirP(Pokemon& p);
