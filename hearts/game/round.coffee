Seat = require './seat'

module.exports = class Round
  constructor: ->
    @north = new Seat()
    @east = new Seat()
    @south = new Seat()
    @west = new Seat()

    @tricks = []

  currentTrick: ->
    @tricks[@tricks.length - 1]

  allHavePassed: ->
    @north.hasPassed() && @east.hasPassed() && @south.hasPassed() && @west.hasPassed()

