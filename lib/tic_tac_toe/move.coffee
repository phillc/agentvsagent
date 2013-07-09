module.exports = class Move
  constructor: (@position, @boardRow, @boardCol, @squareRow, @squareCol) ->

  execute: (game) ->
    square = game.squareAt(@boardRow, @boardCol, @squareRow, @squareCol)
    square.winner = @position
