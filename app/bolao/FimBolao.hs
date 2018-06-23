type Nome = [Char]
type TimeCasa = [Char]
type TimeFora = [Char]
type Senha = [Char]
type SenhaAdmin = [Char]
type Usuario = [Char]
type Gols = Int

data Sala = Sala Nome TimeCasa TimeFora Senha SenhaAdmin deriving (Show)

data Palpite = Palpite Nome Senha Usuario Gols Gols deriving (Show)
