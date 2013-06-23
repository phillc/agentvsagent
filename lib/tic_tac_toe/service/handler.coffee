logger = require '../../logger'
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
    player.recvStartedGame (err, gameId) ->
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      result null, response

  get_game_info: (ticket, result) ->
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    thriftPosition = mapper.positionToThrift(game.positionOf(player))
    gameInfo = new types.GameInfo(position: thriftPosition)

    result null, gameInfo

  make_move: (ticket, coordinates, result) ->
  get_game_result: (ticket, result) ->
