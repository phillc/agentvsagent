und = require 'underscore'
logger = require '../logger'
Engine = require './engine'
Board = require './board'
{EventEmitter} = require 'events'

module.exports = class Game extends EventEmitter
  constructor: ->
    @engine = engine = new Engine(game: this)
    @state =
      boards: [
        [ new Board(), new Board(), new Board() ]
        [ new Board(), new Board(), new Board() ]
        [ new Board(), new Board(), new Board() ]
      ]
      moves: []

    # for position, connection of @positions
    #   do (position, connection) =>
    #     connection.on "message", (messageType, data) =>
    #       @engine.handle [messageType, position].join("."), data

  start: ->
    @engine.transition "waitingForX"

  handle: (args...) ->
    @engine.handle(args...)

  boardAt: (row, column) ->
    @state.boards[row][column]

  squareAt: (boardRow, boardCol, row, col) ->
    @boardAt(boardRow, boardCol).squareAt(row, col)

  winner: ->
    # if @state.moves.length > 5
    #   "X"
