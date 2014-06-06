Game = require('./game').Game
und = require('underscore')

isLeadingTrick = (trick) ->
  trick.played.length == 0

isHeartsBroken = (trick)  ->
  trick.round.tricks.some((trick) -> trick.played.some((card) -> card.suit == Game.Suit.HEARTS))

onlyTwoClubs = (cards) ->
  und.select(cards, (card) -> card.suit == Game.Suit.CLUBS && card.rank == Game.Rank.TWO)

noHearts = (cards) ->
  und.reject(cards, (card) -> card.suit == Game.Suit.HEARTS)

noPoints = (cards) ->
  und.reject(noHearts(cards), (card) -> card.suit == Game.Suit.SPADES && card.rank == Game.Rank.QUEEN)

followSuit = (cards, trick) ->
  suit = trick.played[0].suit
  matching = und.filter(cards, (card) -> card.suit == suit)
  if matching.length > 0
    matching
  else
    cards

playableCards = (trick) ->
  validCards = trick.round.held.slice(0)

  validCards = onlyTwoClubs(validCards) if trick.number == 1 && isLeadingTrick(trick)
  validCards = noPoints(validCards) if trick.number == 1
  validCards = noHearts(validCards) if isLeadingTrick(trick) && !isHeartsBroken(trick) && noHearts(trick.round.held).length > 0
  validCards = followSuit(validCards, trick) if !isLeadingTrick(trick)

  trick.log "Valid cards:", validCards
  validCards

doPassCards = (round) ->
  cardsToPass = round.dealt[0..2]
  round.log "Passing cards", cardsToPass

  cardsToPass

doPlayCard = (trick) ->
  trick.log "Current trick:", trick.played
  cardToPlay = playableCards(trick)[0]
  trick.log "Playing card:", cardToPlay

  cardToPlay

Game.play(doPassCards, doPlayCard)

