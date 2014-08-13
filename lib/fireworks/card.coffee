module.exports = class Card
  @suits: ['red', 'yellow', 'green', 'blue', 'white']
  @ranks: [1, 1, 1, 2, 2, 3, 3, 4, 4, 5]
  @all: ->
    cards = []
    for suit in @suits
      for rank in @ranks
        cards.push new Card(suit, rank)
    cards

  constructor: (@suit, @rank) ->

  toJSON: ->
    suit: @suit, rank: @rank

  inspect: ->
    "#{@rank} of #{@suit}"
