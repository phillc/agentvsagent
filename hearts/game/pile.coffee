Card = require("./card")

module.exports = class Pile
  @createDeck: ->
    pile = new Pile()
    for card in Card.all()
      pile.addCard(card)
    pile

  constructor: ->
    @cards = []

  addCard: (card) ->
    @cards.push card

  allOfSuit: (suit) ->
    @cards.filter (card) -> card.suit == suit

  moveCardsTo: (number, otherPile) ->
    movedCards = @cards.splice(0, number)
    for card in movedCards
      otherPile.addCard(card)

