module Main where
import Types
import CompActions
import PrintParse
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    play

play :: IO ()
play = do
    putStrLn "\n\nBem vindo ao CriptoGame!"
    code <- getRandomCode 4
    putStrLn $ "\nO computador gerou um código de 4 dígitos."
    putStrLn $ "Cada um destes dígitos é um valor entre 1 e 6."
    putStrLn $ "Adivinhe o código gerado no menor número de turnos possíveis.\n"
    forever $ do
        guessString <- prompt "? "
        case parseCodeGuess guessString 4 of
            Just guess -> do
                let feedback = getFeedback code guess
                printFeedback feedback
                when (correctGuess code guess) $ do
                    putStrLn "Correto! Bem feito!"
                    exitSuccess
            Nothing -> do
                putStrLn "Não foi possível reconhecer sua entrada, tente novamente."
                return ()
