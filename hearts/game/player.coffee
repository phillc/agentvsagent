Pile = require "./Pile"

module.exports = class Player
  constructor: ->
    @held = new Pile()
    @takenTricks = []

