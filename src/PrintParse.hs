-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module PrintParse (parseCodeGuess, prompt, printFeedback, parseCodeLength) where
import Types
import Text.Read (readMaybe)
import System.IO

parseNumber :: String -> Maybe Number
parseNumber "1" = Just Um
parseNumber "2" = Just Dois
parseNumber "3" = Just Tres
parseNumber "4" = Just Quatro
parseNumber "5" = Just Cinco
parseNumber "6" = Just Seis
parseNumber _   = Nothing

-- | Analisa uma string que consiste em letras separadas por espaço que representam os códigos.
parseCodeGuess :: String -> Maybe Guess
parseCodeGuess guess
    | length pegStrings /= 4 = Nothing
    | otherwise = Guess <$> mapM parseNumber pegStrings
        where pegStrings = words guess

-- | Imprime uma mensagem de prompt e retorna a resposta do usuário.
prompt :: String -> IO String
prompt message = hSetBuffering stdout NoBuffering >> putStr message >> getLine

-- | Imprime o feedback.
printFeedback :: Feedback -> IO ()
printFeedback (Feedback f) = putStrLn $ "Feedback: " ++ f

-- | Analisa e valida a entrada do usuário para o comprimento do código.
parseCodeLength :: String -> Maybe Int
parseCodeLength len = case readMaybe len of
    Just n -> if n `elem` [2..10] then Just n else Nothing
    Nothing -> Nothing
