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

play handler ticket = do
  print "playing"
  -- print . get_game_info handler
  return ()
