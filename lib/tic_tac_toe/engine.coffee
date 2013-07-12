machina = require('machina')()
logger = require '../logger'
Move = require './move'

module.exports = Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      # console.log "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
  initialState: "initialized"
  states:
    initialized:
      start: ->
        @transition "started"
    started:
      _onEnter: ->
        for position in ["X", "O"]
          # not happy about sending game... it supports a ticket being made, but maybe arena should just be responsible for it
          @game.positions[position].send("started", player: @game.positions[position], game: @game)
      "ready.X": ->
        console.log("ready.X")
        @readyX = true
        if @readyX && @readyO
          @transition("waitingForX")
      "ready.O": ->
        console.log("ready.O")
        @readyO = true
        if @readyX && @readyO
          @transition("waitingForX")
    waitingForX:
      _onEnter: ->
        @game.positions.X.send("turn", position: "X")
      "move.X": (boardRow, boardCol, squareRow, squareCol) ->
        move = new Move("X", boardRow, boardCol, squareRow, squareCol)
        # move.valid?
        move.execute(@game)
        if @game.winner()
          @transition("gameEnded")
        else
          @transition("waitingForO")
    waitingForO:
      _onEnter: ->
        @game.positions.O.send("turn", position: "O")
      "move.O": (coordinates) ->
        @transition("waitingForX")

    gameEnded: {}


