logger = require '../../logger'
types = require './types/tic_tac_toe_types'
# mapper = require './mapper'

module.exports = class Handler
  constructor: (@arena) ->

  _game: (ticket) ->
    @arena.getGame(ticket.gameId)

  _player: (ticket) ->
    game = @_game(ticket)
    game.getPlayer(ticket.agentId)

