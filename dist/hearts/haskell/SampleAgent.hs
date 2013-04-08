module Main where

import Network
import Hearts_Types
import Hearts_Client
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Protocol.Binary
import qualified Data.Vector as V

main :: IO ()
main = do
  print "Starting"
  handle <- hOpen ("127.0.0.1", PortNumber 4001)
  transport <- openFramedTransport handle
  let handler = (BinaryProtocol transport, BinaryProtocol transport)
  entryResponse <- enter_arena handler (EntryRequest Nothing)
  let ticket = f_EntryResponse_ticket entryResponse
  case ticket of
    Just ticket' -> play handler ticket'
    Nothing -> print "no ticket"
  print "DONE"

play :: (Transport t, Transport a, Protocol a2, Protocol a1) => (a2 t, a1 a) -> Ticket -> IO ()
play handler ticket = do
  print "playing"
  gameInfo <- get_game_info handler ticket
  print "game info:"
  print gameInfo

  playRound handler ticket 1
  return ()

playRound :: (Transport t, Transport a, Protocol a2, Protocol a1) => (a2 t, a1 a) -> Ticket -> Int -> IO ()
playRound handler ticket roundNumber = do
  print "new round"
  dealt <- get_hand handler ticket
  print "dealt:"
  print dealt

  -- round %4...
  -- cardsToPass = hand
  let cardsToPass = V.take 3 dealt
      remaining = V.drop 3 dealt

  received <- pass_cards handler ticket cardsToPass
  print "received"
  print received
  let hand = received V.++ remaining
  playTrick handler ticket hand

playTrick :: (Transport t, Transport a, Protocol a2, Protocol a1) => (a2 t, a1 a) -> Ticket -> V.Vector Card -> IO ()
playTrick handler ticket hand = do
  print "hand:"
  print hand
  trick <- get_trick handler ticket
  print "trick:"
  print trick
  play_card handler ticket $ V.head hand
  finishTrick handler ticket hand

finishTrick handler ticket hand
  | V.null $ V.tail hand = finishRound handler ticket
  | otherwise            = playTrick handler ticket $ V.tail hand

finishRound handler ticket = do
  result <- get_round_result handler ticket
  let status = f_RoundResult_status result
  print status

