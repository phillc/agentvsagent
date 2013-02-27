class Suit
  constructor: (@name) ->

class Rank
  constructor: (@order, @name) ->

module.exports = class Card
  @suits: ['clubs', 'diamonds', 'spades', 'hearts']
  @ranks: [2, 3, 4, 5, 6, 7, 8, 9, 10, 'J', 'Q', 'K', 'A']

  constructor: (@suit, @rank) ->
