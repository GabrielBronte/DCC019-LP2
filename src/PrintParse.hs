module PrintParse (parseCodeGuess, prompt, printFeedback, parseCodeLength) where
import Types
import Text.Read (readMaybe)
import System.IO

parseCodePeg :: String -> Maybe CodePeg
parseCodePeg "1" = Just Um
parseCodePeg "2" = Just Dois
parseCodePeg "3" = Just Tres
parseCodePeg "4" = Just Quatro
parseCodePeg "5" = Just Cinco
parseCodePeg "6" = Just Seis
parseCodePeg _   = Nothing

-- | Analisa uma string que consiste em letras separadas por espaço que representam os códigos.
parseCodeGuess :: String -> Maybe Guess
parseCodeGuess guess
    | length pegStrings /= 4 = Nothing
    | otherwise = Guess <$> mapM parseCodePeg pegStrings
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
