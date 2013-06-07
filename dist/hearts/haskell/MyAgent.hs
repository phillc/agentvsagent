module Main where

import Game
import Hearts_Types

noPoints :: [Card] -> [Card]
noPoints cards = filter (\card -> suit card /= SPADES && rank card /= QUEEN) $ noHearts cards

noHearts :: [Card] -> [Card]
noHearts cards = filter (\card -> suit card /= HEARTS) cards

noPointsInFirstTrick :: Round -> [Card] -> [Card]
noPointsInFirstTrick round cards =
  if (length (tricks round)) == 1 then noPoints cards else cards

isHeartsBroken :: Round -> Bool
isHeartsBroken round =
  any (\trick -> any (\card -> suit card == HEARTS) (played trick)) (tricks round)

noHeartsLeadUntilBroken :: Round -> [Card] -> [Card]
noHeartsLeadUntilBroken round cards =
  case (played (currentTrick round)) of
    [] -> if isHeartsBroken round then cards else validCards $ noHearts cards
    xs -> cards
  where
    validCards [] = cards
    validCards xs = xs

followSuit :: Trick -> [Card] -> [Card]
followSuit trick cards =
  case (played trick) of
    [] -> cards
    (leadCard:_) -> validCards $ filter (\card -> suit card == suit leadCard) cards
  where
    validCards [] = cards
    validCards xs = xs

leadWithTwoClubs :: [Card] -> [Card]
leadWithTwoClubs cards =
  validCards $ filter (\card -> suit card == CLUBS && rank card == TWO) cards
  where
    validCards [] = cards
    validCards xs = xs

playableCards :: Round -> [Card]
playableCards round =
  noPointsInFirstTrick round . noHeartsLeadUntilBroken round . leadWithTwoClubs . followSuit trick $ held round
  where
    trick = currentTrick round

doPassCards :: GameState -> [Card]
doPassCards gs = take 3 . dealt . currentRound $ gs

doPlayCard :: GameState -> Card
doPlayCard gs = head . playableCards $ currentRound gs

main = play doPassCards doPlayCard
