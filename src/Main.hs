module Main where

import Types
import CompActions
import PrintParse
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Bem vindo ao CriptoGame!"
    playBreak 4


playBreak :: Int -> IO ()
playBreak codeLength = do
    code <- getRandomCode codeLength
    putStrLn $ "\nO computador criou um código de tamanho " ++ show codeLength ++ "."
    putStrLn $ "Palpite " ++ show codeLength ++ " colours from Red (R), Orange (O), \nYellow (Y), Green (G), Blue (B), or Purple (P).\n"
    forever $ do
        guessString <- prompt "Palpite: "
        case parseCodeGuess guessString codeLength of
            Just guess -> do
                let feedback = getFeedback code guess
                printFeedback feedback
                when (correctGuess code guess) $ do
                    putStrLn "Correto! Bem feito!"
                    exitSuccess
            Nothing -> do
                putStrLn "Não foi possível reconhecer sua entrada, tente novamente."
                return ()
