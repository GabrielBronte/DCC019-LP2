module CompActions (getRandomCodes, getFeedback, winnerCondition, removeCorrectPlaces) where
import Types
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import Data.List

-- | Retorna um código aleatório.
getRandomCode :: IO CodePeg
getRandomCode =
    let allCodes = [Um ..]
    in do
        randomIndex <- randomRIO (0, length allCodes - 1)
        return $ allCodes !! randomIndex

-- | Retorna um código aleatório do comprimento especificado.
getRandomCodes :: IO Code
getRandomCodes = Code <$> mapM (const getRandomCode) [1..4]

-- | Compara um palpite com o código secreto e retorna o feedback com quantidade de códigos completos e parciais.
getFeedback :: Code -> Guess -> Feedback
getFeedback (Code c) (Guess g) =
    Feedback $ show numCorrectPlace ++ " Completo, " ++ show numCorrectCodeButNotPlace ++ " Parcial "
    where
        numCorrectPlace = length . filter id $ zipWith (==) c g 
        codeGuessWithoutDuplicates = removeCorrectPlaces c g
        numCorrectCodeButNotPlace = length $ intersect (fst codeGuessWithoutDuplicates) (snd codeGuessWithoutDuplicates)

-- | Se o palpite corresponde ao código.
winnerCondition :: Code -> Guess -> Bool
winnerCondition (Code c) (Guess g) = c == g

-- | Compara o palpite com o código e remove os códigos completos
removeCorrectPlaces :: Eq a => [a] -> [a] -> ([a], [a])
removeCorrectPlaces [] [] = ( [], [] )
removeCorrectPlaces (hh:ht) (gh:gt)
    | hh == gh = (hl, gl)
    | otherwise = (hh:hl, gh:gl)
    where (hl, gl) = removeCorrectPlaces ht gt



