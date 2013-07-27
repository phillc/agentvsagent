logger = require '../../lib/logger'
types = require './types/tic_tac_toe_types'
AbstractHandler = require '../abstractHandler'
Move = require '../../lib/tic_tac_toe/move'

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
      .then (message) ->
        gameInfo = new types.GameInfo(position: message.data.position)

        console.log("respond get_game_info")
        result null, gameInfo
      .done()

  make_move: (ticket, boardRow, boardCol, squareRow, squareCol, result) ->
    console.log "make move"
    agent = @_getAgent(ticket.agentId)
    # move = new Move(boardRow, boardCol, squareRow, squareCol)
    move = new Move(0, 0, 0, 0)
    agent.forward("move", move)
      .then (message) ->
        opponentsMove = new types.Move(boardRow: 0, boardCol: 0, squareRow: 0, squareCol: 0)
        moveResult = new types.MoveResult(opponents_move: opponentsMove, status: types.GameStatus.NEXT_MOVE)
        console.log("respond make move")
        result null, moveResult
      .done()

  get_game_result: (ticket, result) ->
