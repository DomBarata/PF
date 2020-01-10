module Ficha3 where
  data Hora = H Int Int
            deriving Show

  type Etapa = (Hora, Hora)
  type Viagem = [Etapa]

  v :: Viagem
  v = [ (H 09 30, H 10 25)
      , (H 11 20, H 12 45)
      , (H 13 30, H 14 45)
      ]

  e :: Etapa
  e = (H 09 00, H 14 00)

  horaValida :: Hora -> Bool
  horaValida (H h m) = h >= 0
                    && h < 24
                    && m >= 0
                    && m < 60

  maiorHora :: Hora -> Hora -> Bool
  --so e verdade se a primeira for maior que a segunda
  maiorHora (H h1 m1) (H h2 m2) | h1 > h2 = True
                                | h1 < h2 = False
                                | otherwise = m1 > m2

  etapaValida :: Etapa -> Bool
  etapaValida (h1, h2) = horaValida h1
                      && horaValida h2
                      && maiorHora h2 h1

  viagemValida :: Viagem -> Bool
  viagemValida []        = True
  viagemValida [e]       = etapaValida e
  viagemValida (e1:e2:t) = etapaValida e1
                        && etapaValida e2
                        && maiorHora (fst e2) (snd e1)
                        && viagemValida (e2:t)

  partidaChegada :: Viagem -> (Hora, Hora)
  partidaChegada []        = undefined
  partidaChegada [v]       = v
  partidaChegada (h:t) = (fst h, y)
           where (x,y) = partidaChegada (t)

  toHoras :: Int -> Hora
  toHoras x = H (div x 60) (mod x 60)

  toMinutes :: Hora -> Int
  toMinutes (H h m) = h * 60 + m

  tempoTotal :: Viagem -> Hora
  tempoTotal [] = H 00 00
  tempoTotal v  = toHoras(tempoTotal' v)

  tempoTotal' :: Viagem -> Int
  tempoTotal' []    = 0
  tempoTotal' (h:t) = tempoEtapa h + tempoTotal' t

  tempoEtapa :: Etapa -> Int
  tempoEtapa (h1, h2) = (toMinutes h2) - (toMinutes h1)

  tempoEspera :: Viagem -> Hora
  tempoEspera [] = H 00 00
  tempoEspera v  = toHoras(tempoEspera' v)

  tempoEspera' :: Viagem -> Int
  tempoEspera' (h1:h2:t) = tempoEtapa ((snd h1), (fst h2)) + tempoEspera' (h2:t)
  tempoEspera' _         = 0

  tempoTotalDaViagem :: Viagem -> Hora
  tempoTotalDaViagem [] = H 00 00
  tempoTotalDaViagem v  = toHoras(tempoEtapa(partidaChegada v))

  --
  --
  --
  --

  data Ponto = Cartesiano Double Double
             | Polar Double Double
             deriving (Show, Eq)

  type Poligonal = [Ponto]

  data Figura = Circulo Ponto Double
              | Rectangulo Ponto Ponto
              | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

  p :: Poligonal
  p = [(Cartesiano 0 1)
      , Cartesiano 3 4
      , Cartesiano 5 2
      , Cartesiano 4 8
      , Cartesiano 0 1
      ]

  polar2cart :: Ponto -> Ponto
  polar2cart (Polar r a)        = Cartesiano (r * cos (a*pi/180)) (r * sin (a*pi/180))
  polar2cart c@(Cartesiano x y) = c

  distancia :: Ponto -> Ponto -> Double
  distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
  distancia v1 v2                                 = distancia (polar2cart v1) (polar2cart v2)

  comprimento :: Poligonal -> Double
  comprimento []        = 0
  comprimento [p]       = 0
  comprimento (p1:p2:t) = (distancia p1 p2) + comprimento (p2:t)

  fechada :: Poligonal -> Bool
  fechada p = head p == last p

  triangula :: Poligonal -> [Figura]
  triangula (p1:p2:p3:[]) = []
  triangula (p1:p2:p3:t)  = (Triangulo p1 p2 p3):triangula(p1:p3:t)


  --
  --
  --
  --


  data Contacto = Casa Integer|
                  Trab Integer|
                  Tlm Integer|
                  Email String
                  deriving Show

  type Nome = String

  type Agenda = [(Nome, [Contacto])]

  acrescEmail :: Nome -> String -> Agenda -> Agenda
  acrescEmail no em []         = [(no,[Email em])]
  acrescEmail no em ((n,c):ag) | no == n   = (n, (Email em):c):ag
                               | otherwise = (n,c):acrescEmail no em ag

  verEmails :: Nome -> Agenda -> Maybe [String]
  verEmails _ []                          = Nothing
  verEmails nome ((name, contacto):lista) | nome == name = Just (procura contacto)
                                          | otherwise    = verEmails nome lista

  procura :: [Contacto] -> [String]
  procura []            = []
  procura ((Email h):t) = h:procura t
  procura (_:l)         = procura l

  consTelefs :: [Contacto] -> [Integer]
  consTelefs [] = []
  consTelefs ((Casa h):t) = h:consTelefs t
  consTelefs ((Trab h):t) = h:consTelefs t
  consTelefs ((Tlm h):t)  = h:consTelefs t
  consTelefs (_:t)        = consTelefs t

  casa :: Nome -> Agenda -> Maybe Integer
  casa _ [] = Nothing
  casa nome ((name, contacto):lista) | nome == name = procuraNumCasa contacto
                                     | otherwise    = casa nome lista

  procuraNumCasa :: [Contacto] -> Integer
  procuraNumCasa []            = Nothing
  procuraNumCasa ((Casa h):t)  = Just h
  procuraNumCasa (_:l)         = procura l
