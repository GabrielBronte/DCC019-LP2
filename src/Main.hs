module Main where
import Types
import CompActions
import PrintParse
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "\n\nBem vindo ao CriptoGame!"
    putStrLn $ "\nO computador gerou um código de 4 dígitos."
    putStrLn $ "Cada um destes dígitos é um valor entre 1 e 6."
    putStrLn $ "Adivinhe o código gerado no menor número de turnos possíveis.\n"
    code <- getRandomCodes
    putStrLn $ show code
    playGame 1 code

playGame :: Int -> Code -> IO ()
playGame counter code = do
    guessString <- prompt "? "
    case parseCodeGuess guessString 4 of
        Just guess -> do
            let feedback = getFeedback code guess
            printFeedback feedback
            if (winnerCondition code guess)
            then do
                putStrLn $ "Correto! Voce desvendou o código secreto em " ++ show counter ++ " movimentos! \n\n"
                exitSuccess
            else playGame (counter+1) code
        Nothing -> do
            putStrLn "Não foi possível reconhecer sua entrada, tente novamente."
            playGame (counter+1) code
