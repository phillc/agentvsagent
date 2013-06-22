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

  enter_arena: (request, result) ->
    player = @arena.createPlayer()
    player.recvStartedGame (err, gameId) ->
      ticket = new types.Ticket(agentId: player.id, gameId: gameId)
      response = new types.EntryResponse(ticket: ticket)

      result null, response

  get_game_info: (ticket, result) ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: @game.positions.x.id, gameId: @game.id)

    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        should.not.exist(err)
        gameInfo.position.should.equal(types.Position.X)
        done()

  make_move: (ticket, result) ->
  get_game_result: (ticket, result) ->
