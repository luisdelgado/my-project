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
fimBolao (x:xs) (y:ys) nomeSala senhaAdmin golsCasa golsFora =
 if "teste" == "teste"
 then ("Sala certa", "ok"):fimBolao xs ys nomeSala senhaAdmin golsCasa golsFora
 else [("Erro", "Erro")]