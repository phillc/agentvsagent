AbstractPlayer = require '../abstractPlayer'

module.exports = class Player extends AbstractPlayer
  constructor: ->
    @events = ['startedGame', 'gameInfo']
    super

