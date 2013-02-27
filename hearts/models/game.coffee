Pile = require "./pile"
Card = require "./card"

module.exports = class Game
  constructor: (player1, player2, player3, player4) ->
    @players = [player1, player2, player3, player4]

  play: ->
    # loop until player reaches 100 (term: rounds, sequence)
      # createDeck
      # shuffle
      # deal
      # pass
      # loop 13 (term: tricks)
        # 4x (term: rotation)
          # get moves
        # get winner


  createDeck: ->
    pile = new Pile()
    for suit in Card.suits
      for rank in Card.ranks
        pile.addCard(new Card(suit, rank))

    pile

  deal: (deck) ->
    deck.moveCardsTo(13, @players[0].held)
    deck.moveCardsTo(13, @players[1].held)
    deck.moveCardsTo(13, @players[2].held)
    deck.moveCardsTo(13, @players[3].held)

  passCards: (stategy) ->

