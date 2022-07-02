module CompActions
    ( getRandomCode
    , getFeedback
    , correctGuess
    ) where
import Types
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import Data.List (intersect, nub)

-- | Retorna um pino de código aleatório.
getRandomCodePeg :: IO CodePeg
getRandomCodePeg =
    -- Como allPegs deriva Enum, podemos obter todas as cores com a sintaxe de intervalo
    let allPegs = [Um ..]
    in do
        -- Obtém um inteiro aleatório no intervalo fornecido, envolto em IO herdado de randomRIO
        randomIndex <- randomRIO (0, length allPegs - 1)
        -- Retorna o peg no índice aleatório especificado
        return $ allPegs !! randomIndex  --

-- | Retorna um código aleatório do comprimento especificado.
getRandomCode :: Int -> IO Code
-- randomCodePeg retorna um IO CodePeg e um mapa normal nos daria um [IO CodePeg],
-- mas queremos um IO [CodePeg] que é alcançado com mapM. 
-- Também queremos envolver a lista em um construtor de código, então usamos fmap para colocá-la sob o IO.

getRandomCode len = Code <$> mapM (const getRandomCodePeg) [1..len]

-- | Compares a guess to the secret code and returns the feedback encoded with black and white key pegs.
getFeedback :: Code -> Guess -> Feedback
getFeedback (Code c) (Guess g) =
    -- The order of the key pegs doesn't matter so we first give the black pegs and then the white pegs.
    Feedback $ replicate numCorrectPlace Completo ++ replicate numCorrectColourButNotPlace Parcial
    where
        -- The number of code pegs that match in colour and position: straigtforward zipping with equality
        numCorrectPlace = length . filter id $ zipWith (==) c g
        -- The number of code pegs that match in colour: we remove the duplicate colours from the
        --     code and guess and return the length of their intersection.
        numCorrectColour = length $ intersect (nub c) (nub g)
        -- The number of pegs that match in colour but have the wrong position: in case of duplicates
        --     the there can be more place matches than colour matches so we truncate the subtraction to 0
        numCorrectColourButNotPlace = if diff < 0 then 0 else diff
            where diff = numCorrectColour - numCorrectPlace

-- | Whether the guess matches the code.
correctGuess :: Code -> Guess -> Bool
correctGuess (Code c) (Guess g) = c == g
