und = require 'underscore'
IdGenerator = require '../idGenerator'
logger = require '../logger'
machina = require('machina')()

module.exports = class Game extends machina.Fsm
  constructor: (player1, player2) ->
    #TODO: remove id (or atleast move it to arena)
    @id = IdGenerator.generate()
    @players = [player1, player2]
    @positions = {}
    availablePositions = ["X", "O"]
    for player in und.shuffle(@players)
      @positions[availablePositions.shift()] = player

    @data =
      board: []
      turn: null
      moves: []

    super()
    @on "transition", (details) ->
      logger.verbose "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  getPlayer: (playerId) ->
    #TODO: remove me
    for player in @players
      return player if player.id == playerId

  positionOf: (player) ->
    if @positions.X == player
      "X"
    else if @positions.O == player
      "O"

  start: ->
    @handle("start")

  initialState: "initialized"
  states:
    initialized:
      start: ->
        @transition "started"
    started:
      _onEnter: ->
        @emit "X.started"
        @emit "Y.started"
      "ready.X": ->
        @data.readyX = true
        if @data.readyX && @data.readyY
          @transition("waitingForX")
      "ready.Y": ->
        @data.readyY = true
        if @data.readyX && @data.readyY
          @transition("waitingForX")
    waitingForX:
      _onEnter: ->
        @emit "X.turn"
      "move.X": ->
        @transition("waitingForY")
    waitingForY:
      _onEnter: ->
        @emit "Y.turn"
      "move.Y": ->
        @transition("waitingForX")

