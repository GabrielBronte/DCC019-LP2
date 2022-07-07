-- Gabriel Bronte Cardoso - 201835002
-- Daniel Machado Barbosa Delgado - 201835013

module CompActions (getRandomCodes, getFeedback, winnerCondition, calcGame, select_fst, select_snd, select_trd) where
import Types
import System.Random (randomRIO)
import Control.Applicative ((<$>))
import Data.List

getRandomCode :: IO Number
getRandomCode =
    let allCodes = [Um ..]
    in do
        randomIndex <- randomRIO (0, length allCodes - 1)
        return $ allCodes !! randomIndex

getRandomCodes :: IO Code
getRandomCodes = Code <$> mapM (const getRandomCode) [1..4]

getFeedback :: Code -> Guess -> Feedback
getFeedback (Code c) (Guess g) =
    Feedback $ show numCorrectPlace ++ " Completo, " ++ show numCorrectCodeButNotPlace ++ " Parcial "
    where
        feedback = (calcGame c g) 
        numCorrectPlace = select_fst (feedback) 
        numCorrectCodeButNotPlace = length $ intersect (select_snd (feedback) ) (select_trd (feedback) )

winnerCondition :: Code -> Guess -> Bool
winnerCondition (Code c) (Guess g) = c == g

calcGame :: Eq a => [a] -> [a] -> (Int, [a], [a])
calcGame [] [] = ( 0, [], [] )
calcGame (hh:ht) (gh:gt)
    | hh == gh = ( c+1, hl, gl )
    | otherwise = ( c, hh:hl, gh:gl )
    where ( c, hl, gl ) = calcGame ht gt

select_fst :: (Int, [a], [a]) -> Int
select_fst(x, _, _) = x

select_snd :: (Int, [a], [a]) -> [a]
select_snd(_, y, _) = y

select_trd :: (Int, [a], [a]) -> [a]
select_trd(_, _, z) = z


