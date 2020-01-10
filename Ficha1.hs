module Ex6 where


data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan(y/x)
angulo (Polar r a) = a

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving(Eq, Show)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]
vertices (Retangulo p1 p2) = [p1, p2, Cartesiano (posx p2) (posy p1), Cartesiano (posx p1) (posy p2)]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
        let a = dist p1 p2
            b = dist p2 p3
            c = dist p3 p1
            s = (a + b + c) / 2 --semi-perimetro
        in sqrt (s * (s - a) * (s - b) * s - c) -- formula de Heron
area (Circulo p r) = pi * r ^ 2
area (Retangulo p1 p2) = abs((posx p2 - posx p1) * (posy p2 - posy p1))

perimetro :: Figura -> Double
perimetro (Triangulo p1 p2 p3) =
            let a = dist p1 p2
                b = dist p2 p3
                c = dist p3 p1
            in a + b + c
perimetro (Circulo p r) = 2 * pi * r ^ 2
perimetro (Retangulo p1 p2) =
            let base = posx p2 - posx p1
                altura = posy p2 - posy p1
            in abs(2 * altura + 2 * base)

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isDigit :: Char -> Bool
isDigit d = '0' <= d && d <= '9'

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

toUpper :: Char -> Char
toUpper c = chr (ord c - 32)
