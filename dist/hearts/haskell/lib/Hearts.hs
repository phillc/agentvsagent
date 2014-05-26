{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.9.1)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module Hearts where
import Prelude ( Bool(..), Enum, Double, String, Maybe(..),
                 Eq, Show, Ord,
                 return, length, IO, fromIntegral, fromEnum, toEnum,
                 (.), (&&), (||), (==), (++), ($), (-) )

import Control.Exception
import Data.ByteString.Lazy
import Data.Hashable
import Data.Int
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as TL
import Data.Typeable ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector

import Thrift
import Thrift.Types ()


import Hearts_Types
import qualified Hearts_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Enter_arena_args = Enter_arena_args{f_Enter_arena_args_request :: Maybe EntryRequest} deriving (Show,Eq,Typeable)
instance Hashable Enter_arena_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Enter_arena_args_request record  
write_Enter_arena_args oprot record = do
  writeStructBegin oprot "Enter_arena_args"
  case f_Enter_arena_args_request record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("request",T_STRUCT,1)
    write_EntryRequest oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Enter_arena_args_fields iprot record = do
  (_,_t54,_id55) <- readFieldBegin iprot
  if _t54 == T_STOP then return record else
    case _id55 of 
      1 -> if _t54 == T_STRUCT then do
        s <- (read_EntryRequest iprot)
        read_Enter_arena_args_fields iprot record{f_Enter_arena_args_request=Just s}
        else do
          skip iprot _t54
          read_Enter_arena_args_fields iprot record
      _ -> do
        skip iprot _t54
        readFieldEnd iprot
        read_Enter_arena_args_fields iprot record
read_Enter_arena_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Enter_arena_args_fields iprot (Enter_arena_args{f_Enter_arena_args_request=Nothing})
  readStructEnd iprot
  return record
data Enter_arena_result = Enter_arena_result{f_Enter_arena_result_success :: Maybe EntryResponse} deriving (Show,Eq,Typeable)
instance Hashable Enter_arena_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Enter_arena_result_success record  
write_Enter_arena_result oprot record = do
  writeStructBegin oprot "Enter_arena_result"
  case f_Enter_arena_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_EntryResponse oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Enter_arena_result_fields iprot record = do
  (_,_t59,_id60) <- readFieldBegin iprot
  if _t59 == T_STOP then return record else
    case _id60 of 
      0 -> if _t59 == T_STRUCT then do
        s <- (read_EntryResponse iprot)
        read_Enter_arena_result_fields iprot record{f_Enter_arena_result_success=Just s}
        else do
          skip iprot _t59
          read_Enter_arena_result_fields iprot record
      _ -> do
        skip iprot _t59
        readFieldEnd iprot
        read_Enter_arena_result_fields iprot record
read_Enter_arena_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Enter_arena_result_fields iprot (Enter_arena_result{f_Enter_arena_result_success=Nothing})
  readStructEnd iprot
  return record
data Get_game_info_args = Get_game_info_args{f_Get_game_info_args_ticket :: Maybe Ticket} deriving (Show,Eq,Typeable)
instance Hashable Get_game_info_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_game_info_args_ticket record  
write_Get_game_info_args oprot record = do
  writeStructBegin oprot "Get_game_info_args"
  case f_Get_game_info_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_game_info_args_fields iprot record = do
  (_,_t64,_id65) <- readFieldBegin iprot
  if _t64 == T_STOP then return record else
    case _id65 of 
      1 -> if _t64 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Get_game_info_args_fields iprot record{f_Get_game_info_args_ticket=Just s}
        else do
          skip iprot _t64
          read_Get_game_info_args_fields iprot record
      _ -> do
        skip iprot _t64
        readFieldEnd iprot
        read_Get_game_info_args_fields iprot record
read_Get_game_info_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_game_info_args_fields iprot (Get_game_info_args{f_Get_game_info_args_ticket=Nothing})
  readStructEnd iprot
  return record
data Get_game_info_result = Get_game_info_result{f_Get_game_info_result_success :: Maybe GameInfo,f_Get_game_info_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Get_game_info_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_game_info_result_success record   `hashWithSalt` f_Get_game_info_result_ex record  
write_Get_game_info_result oprot record = do
  writeStructBegin oprot "Get_game_info_result"
  case f_Get_game_info_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_GameInfo oprot _v
    writeFieldEnd oprot}
  case f_Get_game_info_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_game_info_result_fields iprot record = do
  (_,_t69,_id70) <- readFieldBegin iprot
  if _t69 == T_STOP then return record else
    case _id70 of 
      0 -> if _t69 == T_STRUCT then do
        s <- (read_GameInfo iprot)
        read_Get_game_info_result_fields iprot record{f_Get_game_info_result_success=Just s}
        else do
          skip iprot _t69
          read_Get_game_info_result_fields iprot record
      1 -> if _t69 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Get_game_info_result_fields iprot record{f_Get_game_info_result_ex=Just s}
        else do
          skip iprot _t69
          read_Get_game_info_result_fields iprot record
      _ -> do
        skip iprot _t69
        readFieldEnd iprot
        read_Get_game_info_result_fields iprot record
read_Get_game_info_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_game_info_result_fields iprot (Get_game_info_result{f_Get_game_info_result_success=Nothing,f_Get_game_info_result_ex=Nothing})
  readStructEnd iprot
  return record
data Get_hand_args = Get_hand_args{f_Get_hand_args_ticket :: Maybe Ticket} deriving (Show,Eq,Typeable)
instance Hashable Get_hand_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_hand_args_ticket record  
write_Get_hand_args oprot record = do
  writeStructBegin oprot "Get_hand_args"
  case f_Get_hand_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_hand_args_fields iprot record = do
  (_,_t74,_id75) <- readFieldBegin iprot
  if _t74 == T_STOP then return record else
    case _id75 of 
      1 -> if _t74 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Get_hand_args_fields iprot record{f_Get_hand_args_ticket=Just s}
        else do
          skip iprot _t74
          read_Get_hand_args_fields iprot record
      _ -> do
        skip iprot _t74
        readFieldEnd iprot
        read_Get_hand_args_fields iprot record
read_Get_hand_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_hand_args_fields iprot (Get_hand_args{f_Get_hand_args_ticket=Nothing})
  readStructEnd iprot
  return record
data Get_hand_result = Get_hand_result{f_Get_hand_result_success :: Maybe (Vector.Vector Card),f_Get_hand_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Get_hand_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_hand_result_success record   `hashWithSalt` f_Get_hand_result_ex record  
write_Get_hand_result oprot record = do
  writeStructBegin oprot "Get_hand_result"
  case f_Get_hand_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_LIST,0)
    (let f = Vector.mapM_ (\_viter78 -> write_Card oprot _viter78) in do {writeListBegin oprot (T_STRUCT,fromIntegral $ Vector.length _v); f _v;writeListEnd oprot})
    writeFieldEnd oprot}
  case f_Get_hand_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_hand_result_fields iprot record = do
  (_,_t80,_id81) <- readFieldBegin iprot
  if _t80 == T_STOP then return record else
    case _id81 of 
      0 -> if _t80 == T_LIST then do
        s <- (let f n = Vector.replicateM (fromIntegral n) ((read_Card iprot)) in do {(_etype85,_size82) <- readListBegin iprot; f _size82})
        read_Get_hand_result_fields iprot record{f_Get_hand_result_success=Just s}
        else do
          skip iprot _t80
          read_Get_hand_result_fields iprot record
      1 -> if _t80 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Get_hand_result_fields iprot record{f_Get_hand_result_ex=Just s}
        else do
          skip iprot _t80
          read_Get_hand_result_fields iprot record
      _ -> do
        skip iprot _t80
        readFieldEnd iprot
        read_Get_hand_result_fields iprot record
read_Get_hand_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_hand_result_fields iprot (Get_hand_result{f_Get_hand_result_success=Nothing,f_Get_hand_result_ex=Nothing})
  readStructEnd iprot
  return record
data Pass_cards_args = Pass_cards_args{f_Pass_cards_args_ticket :: Maybe Ticket,f_Pass_cards_args_cards :: Maybe (Vector.Vector Card)} deriving (Show,Eq,Typeable)
instance Hashable Pass_cards_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Pass_cards_args_ticket record   `hashWithSalt` f_Pass_cards_args_cards record  
write_Pass_cards_args oprot record = do
  writeStructBegin oprot "Pass_cards_args"
  case f_Pass_cards_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  case f_Pass_cards_args_cards record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("cards",T_LIST,2)
    (let f = Vector.mapM_ (\_viter89 -> write_Card oprot _viter89) in do {writeListBegin oprot (T_STRUCT,fromIntegral $ Vector.length _v); f _v;writeListEnd oprot})
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Pass_cards_args_fields iprot record = do
  (_,_t91,_id92) <- readFieldBegin iprot
  if _t91 == T_STOP then return record else
    case _id92 of 
      1 -> if _t91 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Pass_cards_args_fields iprot record{f_Pass_cards_args_ticket=Just s}
        else do
          skip iprot _t91
          read_Pass_cards_args_fields iprot record
      2 -> if _t91 == T_LIST then do
        s <- (let f n = Vector.replicateM (fromIntegral n) ((read_Card iprot)) in do {(_etype96,_size93) <- readListBegin iprot; f _size93})
        read_Pass_cards_args_fields iprot record{f_Pass_cards_args_cards=Just s}
        else do
          skip iprot _t91
          read_Pass_cards_args_fields iprot record
      _ -> do
        skip iprot _t91
        readFieldEnd iprot
        read_Pass_cards_args_fields iprot record
read_Pass_cards_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Pass_cards_args_fields iprot (Pass_cards_args{f_Pass_cards_args_ticket=Nothing,f_Pass_cards_args_cards=Nothing})
  readStructEnd iprot
  return record
data Pass_cards_result = Pass_cards_result{f_Pass_cards_result_success :: Maybe (Vector.Vector Card),f_Pass_cards_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Pass_cards_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Pass_cards_result_success record   `hashWithSalt` f_Pass_cards_result_ex record  
write_Pass_cards_result oprot record = do
  writeStructBegin oprot "Pass_cards_result"
  case f_Pass_cards_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_LIST,0)
    (let f = Vector.mapM_ (\_viter100 -> write_Card oprot _viter100) in do {writeListBegin oprot (T_STRUCT,fromIntegral $ Vector.length _v); f _v;writeListEnd oprot})
    writeFieldEnd oprot}
  case f_Pass_cards_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Pass_cards_result_fields iprot record = do
  (_,_t102,_id103) <- readFieldBegin iprot
  if _t102 == T_STOP then return record else
    case _id103 of 
      0 -> if _t102 == T_LIST then do
        s <- (let f n = Vector.replicateM (fromIntegral n) ((read_Card iprot)) in do {(_etype107,_size104) <- readListBegin iprot; f _size104})
        read_Pass_cards_result_fields iprot record{f_Pass_cards_result_success=Just s}
        else do
          skip iprot _t102
          read_Pass_cards_result_fields iprot record
      1 -> if _t102 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Pass_cards_result_fields iprot record{f_Pass_cards_result_ex=Just s}
        else do
          skip iprot _t102
          read_Pass_cards_result_fields iprot record
      _ -> do
        skip iprot _t102
        readFieldEnd iprot
        read_Pass_cards_result_fields iprot record
read_Pass_cards_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Pass_cards_result_fields iprot (Pass_cards_result{f_Pass_cards_result_success=Nothing,f_Pass_cards_result_ex=Nothing})
  readStructEnd iprot
  return record
data Get_trick_args = Get_trick_args{f_Get_trick_args_ticket :: Maybe Ticket} deriving (Show,Eq,Typeable)
instance Hashable Get_trick_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_trick_args_ticket record  
write_Get_trick_args oprot record = do
  writeStructBegin oprot "Get_trick_args"
  case f_Get_trick_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_trick_args_fields iprot record = do
  (_,_t112,_id113) <- readFieldBegin iprot
  if _t112 == T_STOP then return record else
    case _id113 of 
      1 -> if _t112 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Get_trick_args_fields iprot record{f_Get_trick_args_ticket=Just s}
        else do
          skip iprot _t112
          read_Get_trick_args_fields iprot record
      _ -> do
        skip iprot _t112
        readFieldEnd iprot
        read_Get_trick_args_fields iprot record
read_Get_trick_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_trick_args_fields iprot (Get_trick_args{f_Get_trick_args_ticket=Nothing})
  readStructEnd iprot
  return record
data Get_trick_result = Get_trick_result{f_Get_trick_result_success :: Maybe Trick,f_Get_trick_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Get_trick_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_trick_result_success record   `hashWithSalt` f_Get_trick_result_ex record  
write_Get_trick_result oprot record = do
  writeStructBegin oprot "Get_trick_result"
  case f_Get_trick_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_Trick oprot _v
    writeFieldEnd oprot}
  case f_Get_trick_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_trick_result_fields iprot record = do
  (_,_t117,_id118) <- readFieldBegin iprot
  if _t117 == T_STOP then return record else
    case _id118 of 
      0 -> if _t117 == T_STRUCT then do
        s <- (read_Trick iprot)
        read_Get_trick_result_fields iprot record{f_Get_trick_result_success=Just s}
        else do
          skip iprot _t117
          read_Get_trick_result_fields iprot record
      1 -> if _t117 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Get_trick_result_fields iprot record{f_Get_trick_result_ex=Just s}
        else do
          skip iprot _t117
          read_Get_trick_result_fields iprot record
      _ -> do
        skip iprot _t117
        readFieldEnd iprot
        read_Get_trick_result_fields iprot record
read_Get_trick_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_trick_result_fields iprot (Get_trick_result{f_Get_trick_result_success=Nothing,f_Get_trick_result_ex=Nothing})
  readStructEnd iprot
  return record
data Play_card_args = Play_card_args{f_Play_card_args_ticket :: Maybe Ticket,f_Play_card_args_card :: Maybe Card} deriving (Show,Eq,Typeable)
instance Hashable Play_card_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Play_card_args_ticket record   `hashWithSalt` f_Play_card_args_card record  
write_Play_card_args oprot record = do
  writeStructBegin oprot "Play_card_args"
  case f_Play_card_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  case f_Play_card_args_card record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("card",T_STRUCT,2)
    write_Card oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Play_card_args_fields iprot record = do
  (_,_t122,_id123) <- readFieldBegin iprot
  if _t122 == T_STOP then return record else
    case _id123 of 
      1 -> if _t122 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Play_card_args_fields iprot record{f_Play_card_args_ticket=Just s}
        else do
          skip iprot _t122
          read_Play_card_args_fields iprot record
      2 -> if _t122 == T_STRUCT then do
        s <- (read_Card iprot)
        read_Play_card_args_fields iprot record{f_Play_card_args_card=Just s}
        else do
          skip iprot _t122
          read_Play_card_args_fields iprot record
      _ -> do
        skip iprot _t122
        readFieldEnd iprot
        read_Play_card_args_fields iprot record
read_Play_card_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Play_card_args_fields iprot (Play_card_args{f_Play_card_args_ticket=Nothing,f_Play_card_args_card=Nothing})
  readStructEnd iprot
  return record
data Play_card_result = Play_card_result{f_Play_card_result_success :: Maybe Trick,f_Play_card_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Play_card_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Play_card_result_success record   `hashWithSalt` f_Play_card_result_ex record  
write_Play_card_result oprot record = do
  writeStructBegin oprot "Play_card_result"
  case f_Play_card_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_Trick oprot _v
    writeFieldEnd oprot}
  case f_Play_card_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Play_card_result_fields iprot record = do
  (_,_t127,_id128) <- readFieldBegin iprot
  if _t127 == T_STOP then return record else
    case _id128 of 
      0 -> if _t127 == T_STRUCT then do
        s <- (read_Trick iprot)
        read_Play_card_result_fields iprot record{f_Play_card_result_success=Just s}
        else do
          skip iprot _t127
          read_Play_card_result_fields iprot record
      1 -> if _t127 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Play_card_result_fields iprot record{f_Play_card_result_ex=Just s}
        else do
          skip iprot _t127
          read_Play_card_result_fields iprot record
      _ -> do
        skip iprot _t127
        readFieldEnd iprot
        read_Play_card_result_fields iprot record
read_Play_card_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Play_card_result_fields iprot (Play_card_result{f_Play_card_result_success=Nothing,f_Play_card_result_ex=Nothing})
  readStructEnd iprot
  return record
data Get_round_result_args = Get_round_result_args{f_Get_round_result_args_ticket :: Maybe Ticket} deriving (Show,Eq,Typeable)
instance Hashable Get_round_result_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_round_result_args_ticket record  
write_Get_round_result_args oprot record = do
  writeStructBegin oprot "Get_round_result_args"
  case f_Get_round_result_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_round_result_args_fields iprot record = do
  (_,_t132,_id133) <- readFieldBegin iprot
  if _t132 == T_STOP then return record else
    case _id133 of 
      1 -> if _t132 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Get_round_result_args_fields iprot record{f_Get_round_result_args_ticket=Just s}
        else do
          skip iprot _t132
          read_Get_round_result_args_fields iprot record
      _ -> do
        skip iprot _t132
        readFieldEnd iprot
        read_Get_round_result_args_fields iprot record
read_Get_round_result_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_round_result_args_fields iprot (Get_round_result_args{f_Get_round_result_args_ticket=Nothing})
  readStructEnd iprot
  return record
data Get_round_result_result = Get_round_result_result{f_Get_round_result_result_success :: Maybe RoundResult,f_Get_round_result_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Get_round_result_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_round_result_result_success record   `hashWithSalt` f_Get_round_result_result_ex record  
write_Get_round_result_result oprot record = do
  writeStructBegin oprot "Get_round_result_result"
  case f_Get_round_result_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_RoundResult oprot _v
    writeFieldEnd oprot}
  case f_Get_round_result_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_round_result_result_fields iprot record = do
  (_,_t137,_id138) <- readFieldBegin iprot
  if _t137 == T_STOP then return record else
    case _id138 of 
      0 -> if _t137 == T_STRUCT then do
        s <- (read_RoundResult iprot)
        read_Get_round_result_result_fields iprot record{f_Get_round_result_result_success=Just s}
        else do
          skip iprot _t137
          read_Get_round_result_result_fields iprot record
      1 -> if _t137 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Get_round_result_result_fields iprot record{f_Get_round_result_result_ex=Just s}
        else do
          skip iprot _t137
          read_Get_round_result_result_fields iprot record
      _ -> do
        skip iprot _t137
        readFieldEnd iprot
        read_Get_round_result_result_fields iprot record
read_Get_round_result_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_round_result_result_fields iprot (Get_round_result_result{f_Get_round_result_result_success=Nothing,f_Get_round_result_result_ex=Nothing})
  readStructEnd iprot
  return record
data Get_game_result_args = Get_game_result_args{f_Get_game_result_args_ticket :: Maybe Ticket} deriving (Show,Eq,Typeable)
instance Hashable Get_game_result_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_game_result_args_ticket record  
write_Get_game_result_args oprot record = do
  writeStructBegin oprot "Get_game_result_args"
  case f_Get_game_result_args_ticket record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ticket",T_STRUCT,1)
    write_Ticket oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_game_result_args_fields iprot record = do
  (_,_t142,_id143) <- readFieldBegin iprot
  if _t142 == T_STOP then return record else
    case _id143 of 
      1 -> if _t142 == T_STRUCT then do
        s <- (read_Ticket iprot)
        read_Get_game_result_args_fields iprot record{f_Get_game_result_args_ticket=Just s}
        else do
          skip iprot _t142
          read_Get_game_result_args_fields iprot record
      _ -> do
        skip iprot _t142
        readFieldEnd iprot
        read_Get_game_result_args_fields iprot record
read_Get_game_result_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_game_result_args_fields iprot (Get_game_result_args{f_Get_game_result_args_ticket=Nothing})
  readStructEnd iprot
  return record
data Get_game_result_result = Get_game_result_result{f_Get_game_result_result_success :: Maybe GameResult,f_Get_game_result_result_ex :: Maybe GameException} deriving (Show,Eq,Typeable)
instance Hashable Get_game_result_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Get_game_result_result_success record   `hashWithSalt` f_Get_game_result_result_ex record  
write_Get_game_result_result oprot record = do
  writeStructBegin oprot "Get_game_result_result"
  case f_Get_game_result_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_GameResult oprot _v
    writeFieldEnd oprot}
  case f_Get_game_result_result_ex record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("ex",T_STRUCT,1)
    write_GameException oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Get_game_result_result_fields iprot record = do
  (_,_t147,_id148) <- readFieldBegin iprot
  if _t147 == T_STOP then return record else
    case _id148 of 
      0 -> if _t147 == T_STRUCT then do
        s <- (read_GameResult iprot)
        read_Get_game_result_result_fields iprot record{f_Get_game_result_result_success=Just s}
        else do
          skip iprot _t147
          read_Get_game_result_result_fields iprot record
      1 -> if _t147 == T_STRUCT then do
        s <- (read_GameException iprot)
        read_Get_game_result_result_fields iprot record{f_Get_game_result_result_ex=Just s}
        else do
          skip iprot _t147
          read_Get_game_result_result_fields iprot record
      _ -> do
        skip iprot _t147
        readFieldEnd iprot
        read_Get_game_result_result_fields iprot record
read_Get_game_result_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Get_game_result_result_fields iprot (Get_game_result_result{f_Get_game_result_result_success=Nothing,f_Get_game_result_result_ex=Nothing})
  readStructEnd iprot
  return record
process_enter_arena (seqid, iprot, oprot, handler) = do
  args <- read_Enter_arena_args iprot
  readMessageEnd iprot
  rs <- return (Enter_arena_result Nothing)
  res <- (do
    res <- Iface.enter_arena handler (f_Enter_arena_args_request args)
    return rs{f_Enter_arena_result_success= Just res})
  writeMessageBegin oprot ("enter_arena", M_REPLY, seqid);
  write_Enter_arena_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_get_game_info (seqid, iprot, oprot, handler) = do
  args <- read_Get_game_info_args iprot
  readMessageEnd iprot
  rs <- return (Get_game_info_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.get_game_info handler (f_Get_game_info_args_ticket args)
      return rs{f_Get_game_info_result_success= Just res})
    (\e  -> 
      return rs{f_Get_game_info_result_ex =Just e}))
  writeMessageBegin oprot ("get_game_info", M_REPLY, seqid);
  write_Get_game_info_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_get_hand (seqid, iprot, oprot, handler) = do
  args <- read_Get_hand_args iprot
  readMessageEnd iprot
  rs <- return (Get_hand_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.get_hand handler (f_Get_hand_args_ticket args)
      return rs{f_Get_hand_result_success= Just res})
    (\e  -> 
      return rs{f_Get_hand_result_ex =Just e}))
  writeMessageBegin oprot ("get_hand", M_REPLY, seqid);
  write_Get_hand_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_pass_cards (seqid, iprot, oprot, handler) = do
  args <- read_Pass_cards_args iprot
  readMessageEnd iprot
  rs <- return (Pass_cards_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.pass_cards handler (f_Pass_cards_args_ticket args) (f_Pass_cards_args_cards args)
      return rs{f_Pass_cards_result_success= Just res})
    (\e  -> 
      return rs{f_Pass_cards_result_ex =Just e}))
  writeMessageBegin oprot ("pass_cards", M_REPLY, seqid);
  write_Pass_cards_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_get_trick (seqid, iprot, oprot, handler) = do
  args <- read_Get_trick_args iprot
  readMessageEnd iprot
  rs <- return (Get_trick_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.get_trick handler (f_Get_trick_args_ticket args)
      return rs{f_Get_trick_result_success= Just res})
    (\e  -> 
      return rs{f_Get_trick_result_ex =Just e}))
  writeMessageBegin oprot ("get_trick", M_REPLY, seqid);
  write_Get_trick_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_play_card (seqid, iprot, oprot, handler) = do
  args <- read_Play_card_args iprot
  readMessageEnd iprot
  rs <- return (Play_card_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.play_card handler (f_Play_card_args_ticket args) (f_Play_card_args_card args)
      return rs{f_Play_card_result_success= Just res})
    (\e  -> 
      return rs{f_Play_card_result_ex =Just e}))
  writeMessageBegin oprot ("play_card", M_REPLY, seqid);
  write_Play_card_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_get_round_result (seqid, iprot, oprot, handler) = do
  args <- read_Get_round_result_args iprot
  readMessageEnd iprot
  rs <- return (Get_round_result_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.get_round_result handler (f_Get_round_result_args_ticket args)
      return rs{f_Get_round_result_result_success= Just res})
    (\e  -> 
      return rs{f_Get_round_result_result_ex =Just e}))
  writeMessageBegin oprot ("get_round_result", M_REPLY, seqid);
  write_Get_round_result_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
process_get_game_result (seqid, iprot, oprot, handler) = do
  args <- read_Get_game_result_args iprot
  readMessageEnd iprot
  rs <- return (Get_game_result_result Nothing Nothing)
  res <- (Control.Exception.catch
    (do
      res <- Iface.get_game_result handler (f_Get_game_result_args_ticket args)
      return rs{f_Get_game_result_result_success= Just res})
    (\e  -> 
      return rs{f_Get_game_result_result_ex =Just e}))
  writeMessageBegin oprot ("get_game_result", M_REPLY, seqid);
  write_Get_game_result_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "enter_arena" -> process_enter_arena (seqid,iprot,oprot,handler)
  "get_game_info" -> process_get_game_info (seqid,iprot,oprot,handler)
  "get_hand" -> process_get_hand (seqid,iprot,oprot,handler)
  "pass_cards" -> process_pass_cards (seqid,iprot,oprot,handler)
  "get_trick" -> process_get_trick (seqid,iprot,oprot,handler)
  "play_card" -> process_play_card (seqid,iprot,oprot,handler)
  "get_round_result" -> process_get_round_result (seqid,iprot,oprot,handler)
  "get_game_result" -> process_get_game_result (seqid,iprot,oprot,handler)
  _ -> do
    skip iprot T_STRUCT
    readMessageEnd iprot
    writeMessageBegin oprot (name,M_EXCEPTION,seqid)
    writeAppExn oprot (AppExn AE_UNKNOWN_METHOD ("Unknown function " ++ TL.unpack name))
    writeMessageEnd oprot
    tFlush (getTransport oprot)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  return True
