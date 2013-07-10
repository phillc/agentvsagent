logger = require '../../lib/logger'
types = require './types/tic_tac_toe_types'

mapper = {}
mapper.positionToThrift = positionToThrift = (position) ->
  switch position
    when "X" then types.Position.X
    when "O" then types.Position.O

module.exports = class Handler
  constructor: (@arena) ->

  _game: (ticket) ->
    @arena.getGame(ticket.gameId)

  _player: (ticket) ->
    game = @_game(ticket)
    game.getPlayer(ticket.agentId)

  enter_arena: (request, result) ->
    player = @arena.createPlayer()
    player.forward("join").then (value) ->
      ticket = new types.Ticket(agentId: value.data.player.id, gameId: value.data.game.id)
      response = new types.EntryResponse(ticket: ticket)

      result null, response

  get_game_info: (ticket, result) ->
    # This should be an event as well...
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    thriftPosition = mapper.positionToThrift(game.positionOf(player))
    gameInfo = new types.GameInfo(position: thriftPosition)

    result null, gameInfo

  make_move: (ticket, boardRow, boardCol, squareRow, squareCol, result) ->
  get_game_result: (ticket, result) ->
