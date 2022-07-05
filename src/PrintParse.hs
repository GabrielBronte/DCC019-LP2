-- | Functions to handle parsing input and printing output in the Mastermind game.
module PrintParse (parseCodeGuess, prompt, printFeedback, parseCodeLength) where
import Types
import Text.Read (readMaybe)
import System.IO

-- | Parses a letter representing a code peg.
--   The input might be incorrect so it returns a Maybe.
parseCodePeg :: String -> Maybe CodePeg
parseCodePeg "1" = Just Um
parseCodePeg "2" = Just Dois
parseCodePeg "3" = Just Tres
parseCodePeg "4" = Just Quatro
parseCodePeg "5" = Just Cinco
parseCodePeg "6" = Just Seis
parseCodePeg _   = Nothing

-- | Analisa uma string que consiste em letras separadas por espaço que representam os pegs de código.
parseCodeGuess :: String -> Int -> Maybe Guess
parseCodeGuess guess codeLength
    -- Se o comprimento do palpite não for o mesmo que o comprimento do código, não podemos ter uma correspondência
    | length pegStrings /= codeLength = Nothing
    -- Similar situation as with getRandomCode: mapping parseCodePeg returns [Maybe CodePeg], 
    --     but we want a Maybe [CodePeg] so we use mapM. If any of the elements is Nothing,
    --     the whole value will be Nothing, so parse errors propagate.
    --     As before, we also fmap the Guess data constructor to get a Maybe Guess.
    | otherwise = Guess <$> mapM parseCodePeg pegStrings
        -- A função words divide uma string com palavras separadas por espaço em uma lista de palavras
        where pegStrings = words guess

-- | Imprime uma mensagem de prompt e retorna a resposta do usuário.
prompt :: String -> IO String
-- Se o buffer estiver ativado, a impressão acontecerá somente depois que você pressionar Enter - precisamos desativá-lo
prompt message = hSetBuffering stdout NoBuffering >> putStr message >> getLine

-- | Imprime o feedback.
printFeedback :: Feedback -> IO ()
printFeedback (Feedback f) = putStrLn $ "Feedback: " ++ f

-- | Analisa e valida a entrada do usuário para o comprimento do código.
parseCodeLength :: String -> Maybe Int
parseCodeLength len = case readMaybe len of
    Just n -> if n `elem` [2..10] then Just n else Nothing
    Nothing -> Nothing
