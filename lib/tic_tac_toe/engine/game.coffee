{EventEmitter} = require 'events'
IdGenerator = require '../../idgenerator'
logger = require '../../logger'

module.exports = class Game extends EventEmitter
  constructor: (player1, player2) ->
    @id = IdGenerator.generate()
    @players = [player1, player2]

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  start: ->
