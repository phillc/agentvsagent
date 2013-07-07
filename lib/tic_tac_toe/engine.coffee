machina = require('machina')()
logger = require '../logger'

module.exports = Engine = machina.Fsm.extend
  initialize: (options, @data) ->
    @on "transition", (details) ->
      logger.verbose "changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
  initialState: "initialized"
  states:
    initialized:
      start: ->
        @transition "started"
    started:
      _onEnter: ->
        @emit "X.started"
        @emit "O.started"
      "ready.X": ->
        @data.readyX = true
        if @data.readyX && @data.readyO
          @transition("waitingForX")
      "ready.O": ->
        @data.readyO = true
        if @data.readyX && @data.readyO
          @transition("waitingForX")
    waitingForX:
      _onEnter: ->
        @emit "X.turn"
      "move.X": (coordinates) ->
        @transition("waitingForO")
    waitingForO:
      _onEnter: ->
        @emit "O.turn"
      "move.O": (coordinates) ->
        @transition("waitingForX")


