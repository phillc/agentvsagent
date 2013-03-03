Card = require("./card")

module.exports = class Pile
  @createDeck: ->
    new Pile(Card.all())

  constructor: (cards) ->
    @cards = cards || []

  addCard: (card) ->
    @cards.push card

  allOfSuit: (suit) ->
    @cards.filter (card) -> card.suit == suit

  moveCardsTo: (number, otherPile) ->
    movedCards = @cards.splice(0, number)
    for card in movedCards
      otherPile.addCard(card)

  moveAllCardsTo: (otherPile) ->
    @moveCardsTo @cards.length, otherPile

  copyAllCardsTo: (otherPile) ->
    for card in @cards
      otherPile.addCard(card)

