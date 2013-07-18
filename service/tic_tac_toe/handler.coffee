logger = require '../../lib/logger'
types = require './types/tic_tac_toe_types'
AbstractHandler = require '../abstractHandler'

mapper = {}
mapper.positionToThrift = positionToThrift = (position) ->
  switch position
    when "X" then types.Position.X
    when "O" then types.Position.O

module.exports = class Handler extends AbstractHandler
  enter_arena: (request, result) ->
    console.log("enter_arena")

    agentId = @_createAgent()

    @_getAgent(agentId).forward("join")
      .then ->
        ticket = new types.Ticket(agentId: agentId)
        response = new types.EntryResponse(ticket: ticket)

        console.log("respond enter_arena")
        result null, response
      .done()

  get_game_info: (ticket, result) ->
    console.log("get_game_info")
    agent = @_getAgent(ticket.agentId)
    agent.forward("ready")
      .then (value) ->
        console.log("back..", value)
        thriftPosition = mapper.positionToThrift(value.data.position)
        gameInfo = new types.GameInfo(position: thriftPosition)

        console.log("respond get_game_info")
        result null, gameInfo
      .done()

  make_move: (ticket, boardRow, boardCol, squareRow, squareCol, result) ->
    console.log "make move"
  get_game_result: (ticket, result) ->
