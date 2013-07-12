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
    console.log("enter_arena")
    player = @arena.createPlayer()
    player.forward("join")
      .then (value) ->
        ticket = new types.Ticket(agentId: value.data.player.id, gameId: value.data.game.id)
        response = new types.EntryResponse(ticket: ticket)

        console.log("respond enter_arena")
        result null, response
      .done()

  get_game_info: (ticket, result) ->
    console.log("get_game_info")
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    player.forward("ready")
      .then (value) ->
        console.log("back..", value)
        thriftPosition = mapper.positionToThrift(value.data.position)
        gameInfo = new types.GameInfo(position: thriftPosition)

        console.log("respond get_game_info")
        result null, gameInfo
      .done()

  make_move: (ticket, boardRow, boardCol, squareRow, squareCol, result) ->
  get_game_result: (ticket, result) ->
