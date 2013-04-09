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
  case roundNumber `rem` 4 of
    0 -> playTrick handler ticket dealt
    otherwise -> passCards handler ticket dealt

  result <- get_round_result handler ticket
  print "round result:"
  print result
  let status = f_RoundResult_status result
  case status of
    Just NEXT_ROUND -> playRound handler ticket (roundNumber + 1)
    Just END_GAME -> finishGame handler ticket

passCards handler ticket dealt = do
  let cardsToPass = V.take 3 dealt
      remaining = V.drop 3 dealt

  received <- pass_cards handler ticket cardsToPass
  print "received"
  print received
  playTrick handler ticket $ received V.++ remaining

playTrick :: (Transport t, Transport a, Protocol a2, Protocol a1) => (a2 t, a1 a) -> Ticket -> V.Vector Card -> IO ()
playTrick handler ticket hand = do
  print "hand:"
  print hand
  trick <- get_trick handler ticket
  print "trick:"
  print trick
  play_card handler ticket $ V.head hand
  if V.null $ V.tail hand
    then print "finished round"
    else playTrick handler ticket $ V.tail hand

finishGame handler ticket = do
  print "Game result:"
  gameResult <- get_game_result handler ticket
  print gameResult
  print "DONE"
