logger = require '../../lib/logger'
types = require './types/fireworks_types'
# mapper = require './mapper'
AbstractHandler = require '../abstractHandler'

module.exports = class Handler extends AbstractHandler
  @ticketForwardingMethod: (handlerFn) ->
    return (ticket, args..., result) ->
      handlerFn args..., (forwardingMessage, data..., callback) =>
        @_getAgent(ticket.agentId).forward(forwardingMessage, data...)
          .then(
            (message) -> result null, callback(message)
            (message) -> result mapper.errorToThrift(message.data)
          )
          .catch ->
            logger.error "A failure occured while attempting to deliver a message", arguments
          .done()

  enter_arena: (request, result) ->
    agentId = @_createAgent()

    @_getAgent(agentId).forward("join")
      .then(
        ->
          ticket = new types.Ticket(agentId: agentId)
          response = new types.EntryResponse(ticket: ticket)
          result null, response
      ).fail ->
        logger.error "A failure occured while attempting to deliver a message", arguments
      .done()

  get_game_info:
    @ticketForwardingMethod (forward) ->
      logger.verbose "Get game info"

      forward "ready", (message) ->
        gameInfo = new types.GameInfo(position: message.data.position)

        logger.verbose "Returning game info", gameInfo
        gameInfo

  get_game_result:
    @ticketForwardingMethod (forward) ->
      forward "finishedGame", (message) ->
        new types.GameResult(message.data.gameScores)
