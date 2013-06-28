{EventEmitter} = require 'events'
und = require 'underscore'
IdGenerator = require '../idGenerator'
logger = require '../logger'

module.exports = class Game extends EventEmitter
  constructor: (player1, player2) ->
    @id = IdGenerator.generate()
    @players = [player1, player2]
    @positions = {}

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  positionOf: (player) ->
    if @positions.X == player
      "X"
    else if @positions.O == player
      "O"

  start: ->
    availablePositions = ["X", "O"]
    for player in und.shuffle(@players)
      @positions[availablePositions.shift()] = player

      player.out.sendStartedGame @id

    @positions.X.out.sendGameInfo position: "X", opponentsMove: []
