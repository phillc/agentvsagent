Pile = require './pile'

module.exports = class Trick
  constructor: (@leader) ->
    @played = new Pile()

  winner: ->
    # TODO: refactor out positions array
    positions = ["north", "east", "south", "west"]
    leaderIndex = positions.indexOf(@leader)
    positionsFromLeader = positions.slice(leaderIndex, 4).concat(positions.slice(0, leaderIndex))

    winningCard = @played.allOfSuit(@played.cards[0].suit).highestRankedCard()
    winningCardIndex = @played.cards.indexOf(winningCard)
    positionsFromLeader[winningCardIndex]
