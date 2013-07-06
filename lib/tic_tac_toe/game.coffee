und = require 'underscore'
IdGenerator = require '../idGenerator'
logger = require '../logger'
Engine = require './engine'

module.exports = class Game
  constructor: (player1, player2) ->
    #TODO: remove id (or atleast move it to arena)
    @id = IdGenerator.generate()
    @players = [player1, player2]
    @positions = {}
    availablePositions = ["X", "O"]
    for player in und.shuffle(@players)
      @positions[availablePositions.shift()] = player

    @state =
      board: []
      moves: []

    @engine = new Engine({}, @state)

  getPlayer: (playerId) ->
    #TODO: remove me
    for player in @players
      return player if player.id == playerId


  positionOf: (player) ->
    if @positions.X == player
      "X"
    else if @positions.O == player
      "O"

  on: (args...) ->
    @engine.on(args...)

  start: ->
    @engine.handle("start")

