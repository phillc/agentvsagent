und = require 'underscore'
IdGenerator = require '../idGenerator'
logger = require '../logger'
Engine = require './engine'
Board = require './board'

module.exports = class Game
  constructor: (player1, player2) ->
    #TODO: remove id (or atleast move it to arena)
    @id = IdGenerator.generate()
    @players = [player1, player2]
    @positions = {}
    @engine = engine = new Engine(game: this)
    @boards = [
      [ new Board(), new Board(), new Board() ]
      [ new Board(), new Board(), new Board() ]
      [ new Board(), new Board(), new Board() ]
    ]
    @moves = []

    availablePositions = ["X", "O"]
    for player in und.shuffle(@players)
      position = availablePositions.shift()
      @positions[position] = player
      do (player, position, engine) ->
        player.on "message", (message, data) ->
          engine.handle "#{message}.#{position}", data

  getPlayer: (playerId) ->
    #TODO: remove me
    for player in @players
      return player if player.id == playerId

  on: (args...) ->
    @engine.on(args...)

  start: ->
    @engine.handle("start")

  boardAt: (row, column) ->
    @boards[row][column]

  squareAt: (boardRow, boardCol, row, col) ->
    @boardAt(boardRow, boardCol).squareAt(row, col)

  winner: ->
    if @moves.length > 5
      "X"
