Card = require '../../lib/hearts/card'
Rank = require '../../lib/hearts/rank'
Suit = require '../../lib/hearts/suit'
types = require './types/hearts_types'

suitMapping = [
  [Suit.CLUBS, types.Suit.CLUBS]
  [Suit.DIAMONDS, types.Suit.DIAMONDS]
  [Suit.SPADES, types.Suit.SPADES]
  [Suit.HEARTS, types.Suit.HEARTS]
]

rankMapping = [
  [Rank.TWO, types.Rank.TWO]
  [Rank.THREE, types.Rank.THREE]
  [Rank.FOUR, types.Rank.FOUR]
  [Rank.FIVE, types.Rank.FIVE]
  [Rank.SIX, types.Rank.SIX]
  [Rank.SEVEN, types.Rank.SEVEN]
  [Rank.EIGHT, types.Rank.EIGHT]
  [Rank.NINE, types.Rank.NINE]
  [Rank.TEN, types.Rank.TEN]
  [Rank.JACK, types.Rank.JACK]
  [Rank.QUEEN, types.Rank.QUEEN]
  [Rank.KING, types.Rank.KING]
  [Rank.ACE, types.Rank.ACE]
]

exports.suitToThrift = suitToThrift = (suit) ->
  for mapping in suitMapping
    return mapping[1] if suit == mapping[0]

exports.thriftToSuit = thriftToSuit = (thriftSuit) ->
  for mapping in suitMapping
    return mapping[0] if thriftSuit == mapping[1]

exports.rankToThrift = rankToThrift = (rank) ->
  for mapping in rankMapping
    return mapping[1] if rank == mapping[0]

exports.thriftToRank = thriftToRank = (thriftRank) ->
  for mapping in rankMapping
    return mapping[0] if thriftRank == mapping[1]

exports.cardToThrift = cardToThrift = (card) ->
  new types.Card(suit: suitToThrift(card.suit), rank: rankToThrift(card.rank))

exports.thriftToCard = thriftToCard = (thriftCard) ->
  new Card(thriftToSuit(thriftCard.suit), thriftToRank(thriftCard.rank))

exports.trickToThrift = trickToThrift = (trick) ->
  cards = trick.played.cards.map cardToThrift
  new types.Trick leader: trick.leader, played: cards

exports.errorToThrift = errorToThrift = (err) ->
  switch err.type
    when "outOfSequence" then new types.OutOfSequenceException(message: err.message)
    when "invalidMove" then new types.InvalidMoveException(message: err.message)
    when "gameAborted" then new types.GameAbortedException(message: err.message)
