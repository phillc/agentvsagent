module.exports = class Rank
  @TWO = new Rank(1, "two")
  @THREE = new Rank(2, "three")
  @FOUR = new Rank(3, "four")
  @FIVE = new Rank(4, "five")
  @SIX = new Rank(5, "six")
  @SEVEN = new Rank(6, "seven")
  @EIGHT = new Rank(7, "eight")
  @NINE = new Rank(8, "nine")
  @TEN = new Rank(9, "ten")
  @JACK = new Rank(10, "jack")
  @QUEEN = new Rank(11, "queen")
  @KING = new Rank(12, "king")
  @ACE = new Rank(13, "ace")

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

  toJSON: ->
    @name

  @fromJSON: (json) ->
    for rank in @all()
      return rank if json == rank.name


