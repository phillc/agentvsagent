machina = require('machina')()
logger = require '../logger'
Move = require './move'

module.exports = Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      # console.log "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
  initialState: "initialized"
  states:
    initialized:
      start: ->
        @transition "started"
    started:
      _onEnter: ->
        for position in ["X", "O"]
          @game.positions[position].send("started", player: @game.positions[position], game: @game)
      "ready.X": ->
        @readyX = true
        if @readyX && @readyO
          @transition("waitingForX")
      "ready.O": ->
        @readyO = true
        if @readyX && @readyO
          @transition("waitingForX")
    waitingForX:
      _onEnter: ->
        @game.positions.X.send("turn")
      "move.X": (coordinates...) ->
        move = new Move("X", coordinates...)
        # move.valid?
        move.execute(@game)
        if @game.winner()
          @transition("gameEnded")
        else
          @transition("waitingForO")
    waitingForO:
      _onEnter: ->
        @game.positions.O.send("turn")
      "move.O": (coordinates) ->
        @transition("waitingForX")

    gameEnded: {}


