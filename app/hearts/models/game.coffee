Pile = require "./pile"
Card = require "./card"

module.exports = class Game

  constructor: (player1, player2, player3, player4) ->
    @players = [player1, player2, player3, player4]

  play: ->
    # loop until player reaches 100 (term: rounds)
      # createDeck
      # shuffle
      # deal
      # pass
      # loop 13
        # 4x
          # get moves
        # get winner


  createDeck: ->
    pile = new Pile()
    for suit in Card.suits
      for rank in Card.ranks
        pile.addCard(new Card(suit, rank))

    pile




