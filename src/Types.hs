-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module Types (Number (..), Numbers (..), Code (..), Guess (..), Feedback (..)) where

data Number = Um | Dois | Tres | Quatro | Cinco | Seis deriving (Eq, Show, Enum)

type Numbers = [Number]

newtype Code = Code Numbers deriving (Eq, Show)

newtype Guess = Guess Numbers deriving (Eq, Show)

newtype Feedback = Feedback String deriving (Eq, Show)
