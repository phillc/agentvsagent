und = require 'underscore'
Card = require("./card")

module.exports = class Pile
  @createDeck: ->
    new Pile(Card.all())

  @createShuffledDeck: ->
    deck = @createDeck()
    deck.shuffle()
    deck

  constructor: (cards) ->
    @cards = cards || []

  addCard: (card) ->
    @cards.push card

  allOfSuit: (suit) ->
    new Pile(@cards.filter (card) -> card.suit == suit)

  highestRankedCard: ->
    cards = @cards.sort (a, b) ->
      b.rank.order - a.rank.order

    cards[0]

  findCard: (suit, rank) ->
    for card in @cards
      return card if card.suit == suit && card.rank == rank

  moveCardsTo: (number, otherPile) ->
    movedCards = @cards.splice(0, number)
    for card in movedCards
      otherPile.addCard(card)

  moveAllCardsTo: (otherPile) ->
    @moveCardsTo @cards.length, otherPile

  copyAllCardsTo: (otherPile) ->
    for card in @cards
      otherPile.addCard(card)

  shuffle: ->
    @cards = und.shuffle(@cards)

