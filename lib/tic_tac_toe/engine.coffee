machina = require('machina')()
logger = require '../logger'

module.exports = Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      console.log "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
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
        @game.state.readyX = true
        if @game.state.readyX && @game.state.readyO
          @transition("waitingForX")
      "ready.O": ->
        @game.state.readyO = true
        if @game.state.readyX && @game.state.readyO
          @transition("waitingForX")
    waitingForX:
      _onEnter: ->
        @game.positions.X.send("turn")
      "move.X": (coordinates) ->
        @transition("waitingForO")
    waitingForO:
      _onEnter: ->
        @game.positions.O.send("turn")
      "move.O": (coordinates) ->
        @transition("waitingForX")


