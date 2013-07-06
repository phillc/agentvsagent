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
      "move.X": (coordinates) ->
        @transition("waitingForY")
    waitingForY:
      _onEnter: ->
        @emit "Y.turn"
      "move.Y": (coordinates) ->
        @transition("waitingForX")


