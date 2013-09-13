actions = require '../../lib/hearts/actions'
logger = require '../../lib/logger'
types = require './types/hearts_types'
mapper = require './mapper'
AbstractHandler = require '../abstractHandler'

module.exports = class Handler extends AbstractHandler
  @ticketForwardingMethod: (handlerFn) ->
    return (ticket, args..., result) ->
      handlerFn args..., (forwardingMessage, data..., callback) =>
        @_getAgent(ticket.agentId).forward(forwardingMessage, data...)
          .then (message) ->
            result null, callback(message)
          .fail (message) ->
            result mapper.errorToThrift(message.data)
          .done()

  enter_arena: (request, result) ->
    agentId = @_createAgent()

    @_getAgent(agentId).forward("join")
      .then ->
        ticket = new types.Ticket(agentId: agentId)
        response = new types.EntryResponse(ticket: ticket)

        #player disconnected before response returned
        #if this were updated in npm, would be fine
        # so either need to wrap in a try, or somehow get that newest version
        result null, response
      .done()

  get_game_info:
    @ticketForwardingMethod (forward) ->
      logger.verbose "Get game info"

      forward "ready", (message) ->
        gameInfo = new types.GameInfo(position: message.data.position)

        logger.verbose "Returning game info", gameInfo
        gameInfo

  get_hand:
    @ticketForwardingMethod (forward) ->
      logger.verbose "Get hand"

      forward "readyForRound", (message) ->
        message.data.cards.map mapper.cardToThrift

  pass_cards:
    @ticketForwardingMethod (cards, forward) ->
      logger.verbose "Pass cards", cards

      mappedCards = cards.map mapper.thriftToCard
      action = new actions.PassCards(mappedCards)

      forward "passCards", action, (message) ->
        message.data.cards.map mapper.cardToThrift

  get_trick:
    @ticketForwardingMethod (forward) ->
      forward "readyForTrick", (message) ->
        mapper.trickToThrift(message.data.trick)

  play_card:
    @ticketForwardingMethod (card, forward) ->
      action = new actions.PlayCard(mapper.thriftToCard(card))
      forward "playCard", action, (message) ->
        mapper.trickToThrift(message.data.trick)

  get_round_result:
    @ticketForwardingMethod (forward) ->
      logger.verbose "get_round_result"

      forward "finishedRound", (message) ->
        roundResult = new types.RoundResult(message.data.roundScores)
        roundResult.status = switch message.data.status
          when "endGame" then types.GameStatus.END_GAME
          when "nextRound" then types.GameStatus.NEXT_ROUND
        logger.verbose "returning get_round_result", roundResult
        roundResult

  get_game_result:
    @ticketForwardingMethod (forward) ->
      forward "finishedGame", (message) ->
        new types.GameResult(message.data.gameScores)
