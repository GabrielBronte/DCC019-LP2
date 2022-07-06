-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module Main where
import Types
import CompActions
import PrintParse
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "\n\nBem vindo ao CriptoGame!"
    putStrLn $ "\nO computador gerou um código de 4 dígitos."
    putStrLn $ "Cada um destes dígitos é um valor entre 1 e 6."
    putStrLn $ "Adivinhe o código gerado no menor número de turnos possíveis.\n"
    code <- getRandomCodes
    playGame 1 code

playGame :: Int -> Code -> IO ()
playGame counter code = do
    guessString <- prompt "? "
    case parseCodeGuess guessString of
        Just guess -> do
            let feedback = getFeedback code guess
            printFeedback feedback
            if (winnerCondition code guess)
            then do
                putStrLn $ "Parabéns, você acertou após " ++ show counter ++ " tentativas. \n\n"
                exitSuccess
            else playGame (counter+1) code
        Nothing -> do
            putStrLn "Não foi possível reconhecer sua entrada, tente novamente."
            playGame (counter+1) code
