module.exports = class Suit
  @CLUBS = new Suit('clubs')
  @DIAMONDS = new Suit('diamonds')
  @SPADES = new Suit('spades')
  @HEARTS = new Suit('hearts')

  @all: ->
    [@CLUBS, @DIAMONDS, @SPADES, @HEARTS]

  constructor: (@name) ->
