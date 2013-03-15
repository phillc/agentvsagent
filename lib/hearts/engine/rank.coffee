module.exports = class Rank
  @TWO = new Rank(1, 2)
  @THREE = new Rank(2, 3)
  @FOUR = new Rank(3, 4)
  @FIVE = new Rank(4, 5)
  @SIX = new Rank(5, 6)
  @SEVEN = new Rank(6, 7)
  @EIGHT = new Rank(7, 8)
  @NINE = new Rank(8, 9)
  @TEN = new Rank(9, 10)
  @JACK = new Rank(10, 'J')
  @QUEEN = new Rank(11, 'Q')
  @KING = new Rank(12, 'K')
  @ACE = new Rank(13, 'A')

  @all: ->
    [
      @TWO
      @THREE
      @FOUR
      @FIVE
      @SIX
      @SEVEN
      @EIGHT
      @NINE
      @TEN
      @JACK
      @QUEEN
      @KING
      @ACE
    ]

  constructor: (@order, @name) ->
