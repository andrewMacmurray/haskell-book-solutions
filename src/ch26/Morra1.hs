module Ch26.Morra1 where

import System.Random (randomRIO)
import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- two players, human and computer
-- each one is either odd or even
-- on each turn both 'puts' down either 1 or 2
-- if the combined number is even the even player wins
-- if the combined number is odd the odd player wins

data Player =
    Human
  | Computer
  deriving (Eq, Show)

data Game =
  Game {
    human :: Int
  , computer :: Int
  } deriving Eq

instance Show Game where
  show (Game { human = x, computer = y }) =
    mconcat [ "-----Human: ", show x
            , "\n"
            , "-----Computer: ", show y
            ]


main :: IO ()
main = do
  putStrLn "The game of Morra!"
  results <- execStateT runGuesses $ Game 0 0
  putStrLn "Final Results:"
  putStrLn $ (show results)


runGuesses :: StateT Game IO ()
runGuesses = do
  game <- get
  io $ putStrLn "enter either 1 or 2"
  h <- io getLine
  cg <- io $ randomRIO (1, 2)
  let hg      = humanGuess h
      newGame = handleGuess hg cg game
      pw      = pointWinner hg cg
  io $ notifyPointWinner pw
  case newGame of
    Left err -> do
      io $ putStrLn err
      runGuesses

    Right g -> do
      put g
      io . putStrLn $ show g
      handleNextGuess g


handleNextGuess :: Game -> StateT Game IO ()
handleNextGuess game
  | human game >= 10    = io $ putStrLn "You win!!"
  | computer game >= 10 = io $ putStrLn "Computer wins :("
  | otherwise           = runGuesses


io :: IO a -> StateT Game IO a
io = liftIO


notifyPointWinner :: Either String Player -> IO ()
notifyPointWinner (Left err)       = putStrLn err
notifyPointWinner (Right Human)    = putStrLn "You won the point!"
notifyPointWinner (Right Computer) = putStrLn "Damn the computer won the point"


pointWinner :: Either String Int -> Int -> Either String Player
pointWinner (Left err) _ = Left err
pointWinner (Right x) y
  | even (x + y) = Right Human
  | otherwise    = Right Computer


handleGuess :: Either String Int -> Int -> Game -> Either String Game
handleGuess (Left err) _ _ = Left err
handleGuess (Right x) y game
  | even (x + y) = Right $ addPoint Human game
  | otherwise    = Right $ addPoint Computer game


addPoint :: Player -> Game -> Game
addPoint Human    g@(Game { human    = x }) = g { human    = x + 1 }
addPoint Computer g@(Game { computer = x }) = g { computer = x + 1 }


humanGuess :: String -> Either String Int
humanGuess "1" = Right 1
humanGuess "2" = Right 2
humanGuess _   = Left "invalid guess"
