actions = require '../../lib/hearts/actions'
logger = require '../../lib/logger'
types = require './types/hearts_types'
mapper = require './mapper'
AbstractHandler = require '../abstractHandler'

module.exports = class Handler extends AbstractHandler
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

  get_game_info: (ticket, result) ->
    logger.verbose "Get game info", ticket

    agent = @_getAgent(ticket.agentId)
    agent.forward("ready")
      .then (message) ->
        gameInfo = new types.GameInfo(position: message.data.position)

        logger.verbose "Returning game info", ticket
        result null, gameInfo
      .done()

  get_hand: (ticket, result) ->
    logger.verbose "Get hand", ticket

    agent = @_getAgent(ticket.agentId)
    agent.forward("readyForRound")
      .then (message) ->
        # return result mapper.errorToThrift(err), null if err
        thriftCards = message.data.cards.map mapper.cardToThrift
        result null, thriftCards
      .done()

  pass_cards: (ticket, cards, result) ->
    logger.verbose "Pass cards", ticket, cards

    mappedCards = cards.map mapper.thriftToCard
    action = new actions.PassCards(mappedCards)

    agent = @_getAgent(ticket.agentId)
    agent.forward("passCards", action)
      .then (message) ->
        thriftCards = message.data.cards.map mapper.cardToThrift
        result null, thriftCards
      .fail (message) ->
        result mapper.errorToThrift(message.data)
      .done()

  get_trick: (ticket, result) ->
    logger.verbose "Get trick", ticket

    agent = @_getAgent(ticket.agentId)
    agent.forward("readyForTrick")
      .then (message) ->
        result null, mapper.trickToThrift(message.data.trick)
      .fail (message) ->
        result mapper.errorToThrift(message.data)
      .done()

  play_card: (ticket, card, result) ->
    logger.verbose "play_card", ticket

    agent = @_getAgent(ticket.agentId)
    action = new actions.PlayCard(mapper.thriftToCard(card))

    agent.forward("playCard", action)
      .then (message) ->
        result null, mapper.trickToThrift(message.data.trick)
      .fail (message) ->
        result mapper.errorToThrift(message.data)
      .done()

  get_round_result: (ticket, result) ->
    logger.verbose "get_round_result", ticket

    agent = @_getAgent(ticket.agentId)
    agent.forward("finishedRound")
      .then (message) ->
      # return result mapper.errorToThrift(err), null if err
        roundResult = new types.RoundResult(message.data.roundScores)
        roundResult.status = switch message.data.status
          when "endGame" then types.GameStatus.END_GAME
          when "nextRound" then types.GameStatus.NEXT_ROUND
        logger.verbose "returning get_round_result", roundResult
        result null, roundResult
      .done()

  get_game_result: (ticket, result) ->
    agent = @_getAgent(ticket.agentId)
    agent.forward("finishedGame")
      .then (message) ->
        # return result mapper.errorToThrift(err), null if err
        gameResult = new types.GameResult(message.data.gameScores)
        result null, gameResult
      .done()
