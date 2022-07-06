-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module Types (Number (..), Numbers (..), Code (..), Guess (..), Feedback (..)) where

-- | Os números a partir dos quais o código é construído.
data Number = Um | Dois | Tres | Quatro | Cinco | Seis deriving (Eq, Show, Enum)

-- | Uma sequência de códigos de Numbers.
type Numbers = [Number]

-- | Um código composto por uma sequência de Numbers de código.
newtype Code = Code Numbers deriving (Eq, Show)

-- | Um palpite do código composto por uma sequência de Numbers de código.
newtype Guess = Guess Numbers deriving (Eq, Show)

-- | Um feedback para um palpite composto por uma certa quantidade de Numbers-chave.
newtype Feedback = Feedback String deriving (Eq, Show)
