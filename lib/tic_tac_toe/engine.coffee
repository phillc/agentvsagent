machina = require('machina')()
logger = require '../logger'
Move = require './move'

waitingFor = (waitingForPosition) ->
  oppositePosition = if waitingForPosition == "X" then "O" else if waitingForPosition == "O" then "X"

  handlers = {}
  handlers["_onEnter"] = ->
    # @game.positions.X.send("turn", position: "X")
    @game.emit "#{waitingForPosition}.turn", position: waitingForPosition

  handlers["move.#{waitingForPosition}"] = (boardRow, boardCol, squareRow, squareCol) ->
    move = new Move(waitingForPosition, boardRow, boardCol, squareRow, squareCol)
    # move.valid?
    move.execute(@game)
    if @game.winner()
      @transition("gameEnded")
    else
      @transition("waitingFor#{oppositePosition}")

  handlers

module.exports = Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      # console.log "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
  initialState: "initialized"
  states:
    initialized: {}
    waitingForX: waitingFor("X")
    waitingForO: waitingFor("O")
    gameEnded: {}


