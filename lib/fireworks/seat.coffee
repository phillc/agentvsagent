Pile = require './pile'

module.exports = class Seat
  constructor: ->
    @held = new Pile()
    @messages = []

