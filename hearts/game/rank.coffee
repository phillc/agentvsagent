module.exports = class Rank
  @values: [2, 3, 4, 5, 6, 7, 8, 9, 10, 'J', 'Q', 'K', 'A']

  @all: ->
    @values.map (value, index) ->
      new Rank(index + 1, value)

  constructor: (@order, @name) ->
