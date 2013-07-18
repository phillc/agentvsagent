actions = require '../../lib/hearts/actions'
logger = require '../../lib/logger'
types = require './types/hearts_types'
mapper = require './mapper'
AbstractHandler = require '../abstractHandler'

module.exports = class Handler extends AbstractHandler
  constructor: (@arena) ->

  _game: (ticket) ->
    @arena.getGame(ticket.gameId)

  _player: (ticket) ->
    game = @_game(ticket)
    game.getPlayer(ticket.agentId)

  enter_arena: (request, result) ->
    player = @arena.createPlayer()
    # Instead of a message from the game, maybe the arena should have responded
    player.out.recvStartedGame (err, gameId) ->
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      #player disconnected before response returned
      #if this were updated in npm, would be fine
      # so either need to wrap in a try, or somehow get that newest version
      result null, response

  get_game_info: (ticket, result) ->
    logger.verbose "Get game info", ticket
    game = @arena.getGame(ticket.gameId)
    player = game.getPlayer(ticket.agentId)

    # Perhaps this should have been pushed from the game, instead
    # of the logic being here
    thriftPosition = mapper.positionToThrift(game.positionOf(player))
    gameInfo = new types.GameInfo(position: thriftPosition)
    logger.verbose "Returning game info", ticket

    result null, gameInfo

  get_hand: (ticket, result) ->
    logger.verbose "Get hand", ticket

    @_player(ticket).out.recvDealt (err, cards) ->
      return result mapper.errorToThrift(err), null if err
      thriftCards = cards.map mapper.cardToThrift
      result null, thriftCards

  pass_cards: (ticket, cards, result) ->
    logger.verbose "Pass cards", ticket, cards
    player = @_player(ticket)
    mappedCards = cards.map mapper.thriftToCard
    action = new actions.PassCards(player, mappedCards)
    @_game(ticket).handleAction(action)
    player.out.recvPassed (err, cards) ->
      return result mapper.errorToThrift(err), null if err
      thriftCards = cards.map mapper.cardToThrift
      result null, thriftCards

  get_trick: (ticket, result) ->
    logger.verbose "Get trick", ticket

    @_player(ticket).out.recvTurn (err, trick) ->
      return result mapper.errorToThrift(err), null if err
      logger.verbose "Returning recvTurn", trick
      result null, mapper.trickToThrift(trick)

  play_card: (ticket, card, result) ->
    logger.verbose "play_card", ticket

    player = @_player(ticket)
    action = new actions.PlayCard(player, mapper.thriftToCard(card))
    @_game(ticket).handleAction(action)
    player.out.recvEndTrick (err, trick) ->
      return result mapper.errorToThrift(err), null if err
      result null, mapper.trickToThrift(trick)

  get_round_result: (ticket, result) ->
    logger.verbose "get_round_result", ticket

    @_player(ticket).out.recvEndRound (err, scores, status) ->
      return result mapper.errorToThrift(err), null if err
      roundResult = new types.RoundResult(scores)
      roundResult.status = switch status
        when "endGame" then types.GameStatus.END_GAME
        when "nextRound" then types.GameStatus.NEXT_ROUND
      result null, roundResult

  get_game_result: (ticket, result) ->
    @_player(ticket).out.recvEndGame (err, scores) ->
      return result mapper.errorToThrift(err), null if err
      gameResult = new types.GameResult(scores)
      result null, gameResult
