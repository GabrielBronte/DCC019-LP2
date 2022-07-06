-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module Types (CodePeg (..), CodePegs (..), Code (..), Guess (..), Feedback (..)) where

-- | Os números a partir dos quais o código é construído.
data CodePeg = Um | Dois | Tres | Quatro | Cinco | Seis deriving (Eq, Show, Enum)

-- | Uma sequência de códigos de pinos.
type CodePegs = [CodePeg]

-- | Um código composto por uma sequência de pinos de código.
newtype Code = Code CodePegs deriving (Eq, Show)

-- | Um palpite do código composto por uma sequência de pinos de código.
newtype Guess = Guess CodePegs deriving (Eq, Show)

-- | Um feedback para um palpite composto por um certo número de pinos-chave.
newtype Feedback = Feedback String deriving (Eq, Show)
