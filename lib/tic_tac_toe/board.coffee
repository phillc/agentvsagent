class Square
  constructor: ->
    @winner = null

module.exports = class Board
  constructor: ->
    @squares = [
      [ new Square(), new Square(), new Square() ]
      [ new Square(), new Square(), new Square() ]
      [ new Square(), new Square(), new Square() ]
    ]

  squareAt: (row, col) ->
    @squares[row][col]
