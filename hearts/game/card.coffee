Suit = require("./suit")
Rank = require("./rank")

module.exports = class Card
  @all: ->
    cards = []
    for suit in Suit.all()
      for rank in Rank.all()
        cards.push new Card(suit, rank)
    cards

  constructor: (@suit, @rank) ->

  score: ->
    if @suit == Suit.HEARTS
      1
    else if @suit == Suit.SPADES && @rank == Rank.QUEEN
      13
    else
      0

  # For debug
  inspect: ->
    "#{@rank.name} of #{@suit.name}"
