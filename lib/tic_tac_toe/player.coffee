AbstractPlayer = require '../abstractplayer'

module.exports = class Player extends AbstractPlayer
  constructor: ->
    @events = ['startedGame']
    super

