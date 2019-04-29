type Nombre = String
type Year = Int

data Paper = P Nombre Year deriving (Show, Eq)

data Grupo = Vacio
            | Becario Nombre [Paper] | Investigador Nombre [Paper] Grupo Grupo Grupo deriving (Show)


-----

--autores --autores de un Paper :: Paper -> Conicet -> [Nombre]

--elevarBecario :: Nombre -> Conicet -> Conicet

--sigloXXIconicet :: Conicet -> Conicet

type Conicet = [Grupo]


paper1 :: Paper 
paper1 = P "aaa" 2001

paper2 :: Paper
paper2 = P "bbb" 1999


grupo1 :: Grupo
grupo1 = Investigador "Marcos" [paper1] 
    (Investigador "Raul" [paper1] (Becario "Luis" [paper2]) (Vacio) (Vacio))
    (Investigador "Raula" [paper1] (Becario "Luisa" [paper2]) (Becario "Rodrigo" [paper1]) 
            (Investigador "Raulcracio" [paper1] (Becario "Jesus" [paper1]) (Becario "Jesusa" [paper2]) (Becario "Nazareno" [paper1])))
    (Vacio)

jefesDeBecario :: Nombre -> Grupo -> [Nombre]
-- Prec: no existen dos becarios con el mismo nombre, el becario a buscar EXISTE en el grupo
jefesDeBecario n (Becario name _) = []
jefesDeBecario n (Investigador name _ g1 g2 g3) = if becarioEstaEn g1 n 
                then [name] ++ jefesDeBecario n g1
                else jefesDeBecario' n g2 g3 name 

jefesDeBecario' :: Nombre -> Grupo -> Grupo -> Nombre -> [Nombre]
jefesDeBecario' n g2 g3 name = if becarioEstaEn g2 n 
                then [name] ++ jefesDeBecario n g2 
                else [name] ++ jefesDeBecario n g3 

becarioEstaEn :: Grupo -> Nombre -> Bool 
becarioEstaEn Vacio n = False
becarioEstaEn (Becario name _) n = n == name 
becarioEstaEn (Investigador _ _ g1 g2 g3) n = 
    becarioEstaEn g1 n || becarioEstaEn g2 n || becarioEstaEn g3 n