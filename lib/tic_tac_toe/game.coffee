und = require 'underscore'
logger = require '../logger'
Board = require './board'
{EventEmitter} = require 'events'

module.exports = class Game
  constructor: ->
    @engine = engine = new Engine(game: this)
    @emitter = new EventEmitter()
    @state =
      boards: [
        [ new Board(), new Board(), new Board() ]
        [ new Board(), new Board(), new Board() ]
        [ new Board(), new Board(), new Board() ]
      ]
      moves: []

  on: (args...) ->
    @emitter.on(args...)

  emit: (message, data) ->
    logger.verbose "game emitting #{message} with #{data}"
    @emitter.emit(message, data)

  start: ->
    @engine.transition "waitingForX"

  handle: (args...) ->
    @engine.handle(args...)

  boardAt: (row, column) ->
    @state.boards[row][column]

  squareAt: (boardRow, boardCol, row, col) ->
    @boardAt(boardRow, boardCol).squareAt(row, col)

  winner: ->
    if @state.moves.length > 5
      "X"

machina = require('machina')()
# logger = require '../logger'

waitingForState = (waitingForPosition) ->
  oppositePosition = if waitingForPosition == "X" then "O" else if waitingForPosition == "O" then "X"

  handlers = {}
  handlers["_onEnter"] = ->
    # @game.positions.X.send("turn", position: "X")
    @game.emit "#{waitingForPosition}.turn", position: waitingForPosition

  handlers["move.#{waitingForPosition}"] = (move) ->
    # move.valid?
    @game.state.moves.push(move)
    move.execute(@game, waitingForPosition)
    if @game.winner()
      @transition("gameEnded")
    else
      @transition("waitingFor#{oppositePosition}")

  handlers

Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      # console.log "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
  initialState: "initialized"
  states:
    initialized: {}
    waitingForX: waitingForState("X")
    waitingForO: waitingForState("O")
    gameEnded: {}


