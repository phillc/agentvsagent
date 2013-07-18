module.exports = class Move
  constructor: (@boardRow, @boardCol, @squareRow, @squareCol) ->

  execute: (game, position) ->
    square = game.squareAt(@boardRow, @boardCol, @squareRow, @squareCol)
    square.winner = position
