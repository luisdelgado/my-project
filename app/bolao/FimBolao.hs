type Nome = [Char]
type TimeCasa = [Char]
type TimeFora = [Char]
type Senha = [Char]
type SenhaAdmin = [Char]
type Usuario = [Char]
type Gols = Int

data Sala = Sala Nome TimeCasa TimeFora Senha SenhaAdmin deriving (Show)

data Palpite = Palpite Nome Senha Usuario Gols Gols deriving (Show)

criarSala :: [Sala] -> Sala -> [Sala]
criarSala x y = x ++ [y]

criarPalpite :: [Palpite] -> Palpite -> [Palpite]
criarPalpite x y = x ++ [y]

fimBolao :: [Sala] -> [Palpite] -> [Char] -> [Char] -> Int -> Int -> [([Char],[Char])]
fimBolao [] _ _ _ _ _ = [("Sala não encontrada!", "Tente novamente!")]
fimBolao _ [] _ _ _ _ = [("Fim do Bolão!", "Agradecemos a sua participação.")]
fimBolao ((Sala a b c d e):xs) ((Palpite f g h i j):ys) nomeSala senhaAdmin golsCasa golsFora =
 if a == nomeSala && e == senhaAdmin
 then if i == golsCasa && j == golsFora
      then (h, "Acertou!"): fimBolao ((Sala a b c d e):xs) ys nomeSala senhaAdmin golsCasa golsFora
      else (h, "Errou!"): fimBolao ((Sala a b c d e):xs) ys nomeSala senhaAdmin golsCasa golsFora
 else fimBolao xs ((Palpite f g h i j):ys) nomeSala senhaAdmin golsCasa golsFora