module CompActions (getRandomCode, getFeedback, correctGuess, removeFromList, guessesExact) where
import Types
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import Data.List

removeFromList :: Eq a => [a] -> [a] -> [a]
removeFromList _ [] = []
removeFromList (y:ys) (x:xs)
                  | y == x = removeFromList [y] xs
                  | otherwise = x : removeFromList [y] xs

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

-- | Compara um palpite com o código secreto e retorna o feedback codificado com pinos de chave completo e parcial.
getFeedback :: Code -> Guess -> Feedback
getFeedback (Code c) (Guess g) =
    -- A ordem dos pinos das teclas não importa, então primeiro damos os pinos completos e depois os pinos parciais.
    Feedback $ show numCorrectPlace ++ " Completo, " ++ show numCorrectColour ++ " Parcial "
    where
        -- O número de pinos de código que correspondem em cor e posição: compactação direta com igualdade
        numCorrectPlace = length . filter id $ zipWith (==) c g 

        x = guessesExact c g
        -- O número de pinos de código que correspondem em cores: removemos as cores duplicadas do código, adivinhamos e retornamos o comprimento de sua interseção.
        numCorrectColour = length $ intersect (fst x) (snd x)
        -- O número de pinos que combinam em cores, mas têm a posição errada: em caso de duplicatas, 
        -- pode haver mais correspondências de lugar do que correspondências de cores, então truncamos a subtração para 0
        numCorrectColourButNotPlace = if diff < 0 then 0 else diff
            where diff = numCorrectColour - numCorrectPlace

-- | Se o palpite corresponde ao código.
correctGuess :: Code -> Guess -> Bool
correctGuess (Code c) (Guess g) = c == g


guessesExact :: Eq a => [a] -> [a] -> ([a], [a])
guessesExact [] [] = ( [], [] )
guessesExact (hh:ht) (gh:gt)
    | hh == gh = ( hl
                    , gl
                    )
    | otherwise = ( hh:hl
                    , gh:gl
                    )
    where ( hl, gl) = guessesExact ht gt



