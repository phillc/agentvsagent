module Game where

import Network
import Hearts_Types
import Hearts_Client
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Protocol.Binary

import qualified Data.Vector as V
import Data.List
import Data.Maybe (fromJust) -- , isNothing, isJust, fromMaybe)

import System.Environment

suit :: Card -> Suit
suit = fromJust . f_Card_suit
rank :: Card -> Rank
rank = fromJust . f_Card_rank

played :: Trick -> [Card]
played = V.toList . fromJust . f_Trick_played

data Round = Round
  { number :: Int
  , tricks :: [Trick]
  , dealt :: [Card]
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
                   , held = [] }
  in gs{ rounds = newRound : rounds gs
     }

runTrick :: (Transport t, Transport a, Protocol a2, Protocol a1) => (GameState -> Card) -> (a2 t, a1 a) -> Ticket -> GameState -> IO GameState
runTrick playCardFn handler ticket gs = do
  logit "Starting trick"

  trick <- get_trick handler ticket

  let gsStartingTrick = gs{ rounds = (currentRound gs){ tricks = trick : tricks (currentRound gs) } : tail (rounds gs) }
  logit $ "Current trick:" ++ (show . currentTrick . currentRound) gsStartingTrick
  let cardToPlay = playCardFn gsStartingTrick
  logit $ "Playing card:" ++ (show cardToPlay)

  resultingTrick <- play_card handler ticket $ cardToPlay
  logit $ "trick: result" ++ (show resultingTrick)
  let gsResultingTrick = gs{ rounds = (currentRound gs){ held = (delete cardToPlay (held (currentRound gs))), tricks = resultingTrick : tricks (currentRound gs) } : tail (rounds gs) }
  return gsResultingTrick

runRound :: (Transport t, Transport a, Protocol a2, Protocol a1) => (GameState -> [Card]) -> (GameState -> Card) -> (a2 t, a1 a) -> Ticket -> GameState -> IO GameState
runRound passCardsFn playCardFn handler ticket gs = do
  logit "Starting round"
  receivedCards <- get_hand handler ticket
  let receivedCards' = V.toList receivedCards
      ags = gs{ rounds = (currentRound gs){ dealt = receivedCards', held = receivedCards' } : tail (rounds gs) }
  logit $ "You were dealt:" ++ (show . dealt . currentRound) ags
  newgs <- passCards passCardsFn handler ticket ags
  playTrick playCardFn handler ticket newgs

passCards :: (Transport t, Transport a, Protocol a2, Protocol a1) => (GameState -> [Card]) -> (a2 t, a1 a) -> Ticket -> GameState -> IO GameState
passCards passCardsFn handler ticket gs = do
  case number (currentRound gs) `rem` 4 of
    0 -> do
      logit "Not passing cards"
      return gs
    otherwise -> do
      logit "About to pass cards"
      let cardsToPass = passCardsFn gs
      received <- pass_cards handler ticket $ V.fromList cardsToPass
      let holding = (held (currentRound gs) \\ cardsToPass) ++ V.toList received
          ags = gs{ rounds = (currentRound gs){ held = holding } : tail (rounds gs) }

      logit $ "Passing:" ++ show cardsToPass
      logit $ "Received cards:" ++ show received
      return ags

playTrick :: (Transport t, Transport a, Protocol a2, Protocol a1) => (GameState -> Card) -> (a2 t, a1 a) -> Ticket -> GameState -> IO GameState
playTrick playCardFn handler ticket gs = do
  loop 1 gs
  where
    loop :: Integer -> GameState -> IO GameState
    loop i gs | i <= 13 = do
                 newgs <- runTrick' gs
                 loop (i + 1) newgs
              | otherwise = return gs
    runTrick' = runTrick playCardFn handler ticket

runGame :: (Transport t, Transport a, Protocol a2, Protocol a1) => (GameState -> [Card]) -> (GameState -> Card) -> (a2 t, a1 a) -> Ticket -> IO ()
runGame passCardsFn playCardFn handler ticket = do
  logit "Starting game"

  loop (GameState [])

  logit "done?"

  where
    loop :: GameState -> IO GameState
    loop gs = do
      newgs <- runRound passCardsFn playCardFn handler ticket (createRound gs)
      roundResult <- get_round_result handler ticket
      logit $ "round result:" ++ show roundResult
      case f_RoundResult_status roundResult of
        Just NEXT_ROUND -> loop newgs
        Just END_GAME -> return newgs

play :: (GameState -> [Card]) -> (GameState -> Card) -> IO ()
play passCardsFn playCardFn = do
  env <- getEnvironment
  logit "done"
  let host = "127.0.0.1"
      port = 4001
  logit "Starting"
  handle <- hOpen (host, PortNumber port)
  transport <- openFramedTransport handle
  let handler = (BinaryProtocol transport, BinaryProtocol transport)
  entryResponse <- enter_arena handler (EntryRequest Nothing)
  let ticket = f_EntryResponse_ticket entryResponse

  case ticket of
    Just ticket -> do
      logit "playing"
      gameInfo <- get_game_info handler ticket
      logit $ "game info:" ++ show gameInfo

      runGame passCardsFn playCardFn handler ticket

      logit "Game is over"
      gameResult <- get_game_result handler ticket
      logit $ "game result:" ++ show gameResult
    Nothing ->
      logit "no ticket"
  tClose handle




-- 
-- finishGame handler ticket = do
--   print "Game result:"
--   gameResult <- get_game_result handler ticket
--   print gameResult
--   print "DONE"
-- 
