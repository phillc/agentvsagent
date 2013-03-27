Pile = require './pile'

module.exports = class Trick
  constructor: (@leader) ->
    @played = new Pile()

  winner: ->
    winningCard = @played.allOfSuit(@played.cards[0].suit).highestRankedCard()
    winningCardIndex = @played.cards.indexOf(winningCard)
    @positionsFromLeader()[winningCardIndex]

  score: ->
    @played.cards.reduce (memo, card) ->
      memo + card.score()
    , 0

  positionsFromLeader: ->
    positions = ["north", "east", "south", "west"]
    leaderIndex = positions.indexOf(@leader)
    positions.slice(leaderIndex, 4).concat(positions.slice(0, leaderIndex))
