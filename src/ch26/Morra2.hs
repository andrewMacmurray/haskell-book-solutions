module Ch26.Morra2 where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Console.ANSI
import Control.Applicative (liftA2)

data Player =
    Player1
  | Player2
  deriving (Eq, Show)

data Game =
  Game {
    p1 :: Int
  , p2 :: Int
  } deriving Eq

instance Show Game where
  show (Game { p1 = x, p2 = y }) =
    mconcat [ "-----Player 1: ", show x
            , "\n"
            , "-----Player 2: ", show y
            ]


main :: IO ()
main = do
  putStrLn "The game of Morra!"
  putStrLn "Player 1 is even, Player 2 is odd"
  results <- execStateT runGuesses $ Game 0 0
  setColor Magenta
  putStrLn "Final Results:"
  putStrLn $ (show results)


runGuesses :: StateT Game IO ()
runGuesses = do
  game <- get

  io $ setColor Blue >> putStrLn "Player 1 - enter either 1 or 2"
  p1Guess <- io getLine
  handleInvalidGuess p1Guess
  resetScreen

  io $ setColor Green >> putStrLn "Player 2 - enter either 1 or 2"
  p2Guess <- io getLine
  handleInvalidGuess p2Guess
  resetScreen

  let p1G     = parseGuess p1Guess
      p2G     = parseGuess p2Guess
      newGame = handleGuess p1G p2G game
      pw      = pointWinner p1G p2G
  io $ setColor Yellow
  io $ notifyPointWinner pw
  case newGame of
    Left err -> do
      io $ putStrLn err
      runGuesses

    Right g -> do
      put g
      io . putStrLn $ show g
      handleNextGuess g

setColor :: Color -> IO ()
setColor color = setSGR [ SetColor Foreground Vivid color ]

handleInvalidGuess :: String -> StateT Game IO ()
handleInvalidGuess rawGuess =
  case parseGuess rawGuess of
    Left err -> do
      resetScreen
      io $ putStrLn err
      runGuesses

    Right g -> return ()


resetScreen :: StateT Game IO ()
resetScreen = io $ do
  cursorUpLine 10
  clearScreen


handleNextGuess :: Game -> StateT Game IO ()
handleNextGuess game
  | p1 game >= 10 = io $ (setColor Magenta) >> (putStrLn "Player 1 wins!!")
  | p2 game >= 10 = io $ (setColor Magenta) >> (putStrLn "Player 2 wins!!")
  | otherwise     = runGuesses


io :: IO a -> StateT Game IO a
io = liftIO


notifyPointWinner :: Either String Player -> IO ()
notifyPointWinner (Left err)      = putStrLn err
notifyPointWinner (Right Player1) = putStrLn "Player 1 won the point!"
notifyPointWinner (Right Player2) = putStrLn "Player 2 won the point!"


pointWinner :: Either String Int -> Either String Int -> Either String Player
pointWinner (Left err) _ = Left err
pointWinner _ (Left err) = Left err
pointWinner (Right x) (Right y)
  | even (x + y) = Right Player1
  | otherwise    = Right Player2


handleGuess :: Either String Int -> Either String Int -> Game -> Either String Game
handleGuess (Left err) _ _ = Left err
handleGuess _ (Left err) _ = Left err
handleGuess (Right x) (Right y) game
  | even (x + y) = Right $ addPoint Player1 game
  | otherwise    = Right $ addPoint Player2 game


addPoint :: Player -> Game -> Game
addPoint Player1 = liftA2 Game (inc p1) p2
addPoint Player2 = liftA2 Game p1 (inc p2)

inc = fmap (+1)


parseGuess :: String -> Either String Int
parseGuess "1" = Right 1
parseGuess "2" = Right 2
parseGuess _   = Left "invalid guess"
