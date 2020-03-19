module Game where

import qualified Data.Vector as V
import Data.List
import Data.Maybe (fromJust) -- , isNothing, isJust, fromMaybe)

import System.Environment
import System.IO

clientRead = getLine
clientSendAndReceive = getLine

data Card = Card
  { suit :: String
  , rank :: String
  } deriving (Show)

data Trick = Trick
  { leader :: String
  , played :: [Card]
  } deriving (Show)

data Round = Round
  { number :: Int
  , tricks :: [Trick]
  , dealt :: [Card]
  , passed :: [Card]
  , received :: [Card]
  , held :: [Card]
  } deriving (Show)

data GameState = GameState
  { rounds :: [Round]
  } deriving (Show)

logit :: Show a => a -> IO ()
logit = print

currentTrick :: Round -> Trick
currentTrick = head . tricks

currentRound :: GameState -> Round
currentRound gs = head $ rounds gs

createRound :: GameState -> GameState
createRound gs =
  let newRound = Round{ number = length (rounds gs) + 1
                   , tricks = []
                   , dealt = []
                   , passed = []
                   , received = []
                   , held = [] }
  in gs{ rounds = newRound : rounds gs
     }

runTrick :: (GameState -> Card) -> GameState -> IO GameState
runTrick playCardFn gs = do
  logit "Starting trick"

  trick <- clientSendAndReceive "readyForTrick"

  let gsStartingTrick = gs{ rounds = (currentRound gs){ tricks = trick : tricks (currentRound gs) } : tail (rounds gs) }
  logit $ "Current trick:" ++ (show . currentTrick . currentRound) gsStartingTrick
  let cardToPlay = playCardFn gsStartingTrick
  logit $ "Playing card:" ++ (show cardToPlay)

  resultingTrick <- clientSendAndReceive "playCard" "card" cardToPlay
  logit $ "trick: result" ++ (show resultingTrick)
  let gsResultingTrick = gs{ rounds = (currentRound gs){ held = (delete cardToPlay (held (currentRound gs))), tricks = resultingTrick : tricks (currentRound gs) } : tail (rounds gs) }
  return gsResultingTrick

runRound :: (GameState -> [Card]) -> (GameState -> Card) -> GameState -> IO GameState
runRound passCardsFn playCardFn gs = do
  logit "Starting round"
  receivedCards <- clientSendAndReceive "readyForRound"
  let receivedCards' = V.toList receivedCards
      ags = gs{ rounds = (currentRound gs){ dealt = receivedCards', held = receivedCards' } : tail (rounds gs) }
  logit $ "You were dealt:" ++ (show . dealt . currentRound) ags
  newgs <- passCards passCardsFn ags
  playTrick playCardFn newgs

passCards :: (GameState -> [Card]) -> GameState -> IO GameState
passCards passCardsFn gs = do
  case number (currentRound gs) `rem` 4 of
    0 -> do
      logit "Not passing cards"
      return gs
    otherwise -> do
      logit "About to pass cards"
      let cardsToPass = passCardsFn gs
      received <- clientSendAndReceive "passCards" $ V.fromList cardsToPass
      let holding = (held (currentRound gs) \\ cardsToPass) ++ V.toList received
          ags = gs{ rounds = (currentRound gs){ held = holding, received = (V.toList received), passed = cardsToPass } : tail (rounds gs) }

      logit $ "Passing:" ++ show cardsToPass
      logit $ "Received cards:" ++ show received
      return ags

playTrick :: (GameState -> Card) -> GameState -> IO GameState
playTrick playCardFn gs = do
  loop 1 gs
  where
    loop :: Integer -> GameState -> IO GameState
    loop i gs | i <= 13 = do
                 newgs <- runTrick' gs
                 loop (i + 1) newgs
              | otherwise = return gs
    runTrick' = runTrick playCardFn

runGame :: (GameState -> [Card]) -> (GameState -> Card) -> IO ()
runGame passCardsFn playCardFn = do
  logit "Starting game"

  loop (GameState [])

  logit "done?"

  where
    loop :: GameState -> IO GameState
    loop gs = do
      newgs <- runRound passCardsFn playCardFn (createRound gs)
      roundResult <- clientSendAndReceive "finishedRound"
      logit $ "round result:" ++ show roundResult
      case roundResult of
        "nextRound" -> loop newgs
        "endGame" -> return newgs

play :: (GameState -> [Card]) -> (GameState -> Card) -> IO ()
play passCardsFn playCardFn = do
  hSetBuffering stdin NoBuffering
  logit "Starting"

  logit "playing"
  gameInfo <- clientRead
  logit $ "game info:" ++ show gameInfo

  runGame passCardsFn playCardFn

  logit "Game is over"
  gameResult <- clientSendAndReceive "finishedGame"
  logit $ "game result:" ++ show gameResult




--
-- finishGame handler ticket = do
--   print "Game result:"
--   gameResult <- get_game_result handler ticket
--   print gameResult
--   print "DONE"
--
