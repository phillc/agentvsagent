Pile = require "./pile"

module.exports = class Seat
  constructor: ->
    @dealt = new Pile()
    @passed = new Pile()
    @received = new Pile()
    @held = new Pile()

  hasPassed: ->
    @passed.cards.length > 0

