Pile = require './pile'

class Position
  constructor: ->
    @dealt = new Pile()
    @passed = new Pile()
    @played = new Pile()

module.exports = class Round
  constructor: ->
    @north = new Position()
    @east = new Position()
    @south = new Position()
    @west = new Position()
