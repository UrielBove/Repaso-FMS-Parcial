module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"


cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

rimaAsonante :: String -> String -> Bool
rimaAsonante pal1 pal2 = ultimasDosVocales 2 pal1 == ultimasDosVocales 2 pal2 

esVocalYTieneTilde :: [Char] -> [Char]
esVocalYTieneTilde = filter esVocal 

dameTildes :: [Char] -> [Char]
dameTildes = filter tieneTilde

ultimasDosVocales :: Number -> [Char] -> [Char]
ultimasDosVocales n pal = take n (reverse ((esVocalYTieneTilde.dameTildes) pal))

rimaConsonante :: String -> String -> Bool
rimaConsonante pal1 pal2 = take 3 (reverse pal1) == take 3 (reverse pal2)

riman :: String -> String -> Bool
riman pal1 pal2 = pal1 /= pal2 && rimaAsonante pal1 pal2 || rimaConsonante pal1 pal2

--Conjugaciones

type Conjugacion = Verso -> Verso -> Bool

porMedioDeRimas :: Conjugacion
porMedioDeRimas = cumplen ultimaPalabra riman

ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words 

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words 

anadiplosis :: Conjugacion
anadiplosis v1 v2 = ultimaPalabra v1 == primeraPalabra v2

--Patrones

type Patron = Estrofa -> Bool
type Par = (Number,Number)

simple :: Patron
simple estrofa = porMedioDeRimas (head estrofa) (last estrofa)

simple1 :: Par -> Patron
simple1 (n1,n2) estrofa = porMedioDeRimas (versoAt n1 estrofa) (versoAt n2 estrofa)

versoAt :: Number -> Estrofa -> Verso
versoAt n estrofa =  estrofa !! (n - 1)

esEsdrujula :: Patron
esEsdrujula = all (tieneVocalAcenTercerPos . ultimaPalabra)

tieneVocalAcenTercerPos :: Palabra -> Bool
tieneVocalAcenTercerPos pal = tieneTilde (reverse (filter esVocal pal) !! 3)

--

--anafora :: Estrofa -> Bool
--anafora [] = False
--anafora [v1,v2] = primeraPalabra v1 == primeraPalabra v2 
--anafora (v1:v2:vs) = primeraPalabra v1 == primeraPalabra v2 && anafora vs
--
iguales :: [Palabra] -> Bool
iguales [] = False
iguales (palabra:palabras) = all(==palabra)palabras

anafora :: Patron
anafora = iguales . map primeraPalabra

cadena :: Conjugacion -> Patron
cadena _ [] = False
cadena _ [_] = True
cadena conjugacion [v1,v2] = conjugacion v1 v2
cadena conjugacion (v1:v2:vs) = conjugacion v1 v2 && cadena conjugacion (v2:vs)

combinaDos :: Patron -> Patron -> Estrofa -> Bool
combinaDos p1 p2 e = p1 e && p2 e


aabb :: Patron
aabb estrofa = simple1 (1,2) estrofa && simple1 (3,4) estrofa

abab :: Patron
abab estrofa = simple1 (1,3) estrofa && simple1 (2,4) estrofa