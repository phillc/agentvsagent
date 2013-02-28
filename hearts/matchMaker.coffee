module.exports = class MatchMaker
  constructor: ->
    @waitingPlayers = []

  addPlayer: (player) ->
    @waitingPlayers.push player
    "123"


