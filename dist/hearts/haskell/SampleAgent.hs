module SampleAgent where

import Network
import Hearts_Types
import Hearts_Client
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Protocol.Binary

main :: IO ()
main = do
  print "Starting"
  handle <- hOpen ("127.0.0.1", PortNumber 4001)
  transport <- openFramedTransport handle
  let handler = (BinaryProtocol transport, BinaryProtocol transport)
  entryResponse <- enter_arena handler
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

playRound :: (Transport t, Transport a, Protocol a2, Protocol a1) => (a2 t, a1 a) -> Ticket -> Int -> IO () 0
playRound handler ticket roundNumber = do
  hand <- get_hand handler ticket
  print "hand:"
  print hand

  -- round %4...
  cardsToPass = hand


playTrick = 2
