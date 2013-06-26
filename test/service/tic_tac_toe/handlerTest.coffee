Handler = require '../../../service/tic_tac_toe/handler'
types = require '../../../service/tic_tac_toe/types/tic_tac_toe_types'
TicTacToe = require '../../../service/tic_tac_toe/types/TicTacToe'
TicTacToeFactory = require '../../../lib/tic_tac_toe/factory'
Factory = require '../../factory'
should = require("should")

describe "Handler", ->
  beforeEach ->
    @arena = Factory.createArena(factory: new TicTacToeFactory())
    @arena.createPlayer()
    @handler = new Handler(@arena)
    @game = Factory.createGame(arena: @arena)

  it "implements everything declared in the service", ->
    methods = Object.keys(TicTacToe.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in methods
      @handler.should.have.property method
      @handler[method].length.should.equal(TicTacToe.Client.prototype[method].length)

    Object.keys(Handler.prototype).filter((name) -> name[0] != "_").length.should.equal(methods.length)

  describe "#enter_arena", ->
    it "returns when there is a game to be played", (done) ->
      @handler.enter_arena new types.EntryRequest(), (err, response) ->
        should.not.exist(err)
        response.ticket.agentId.should.be.a("string")
        response.ticket.gameId.should.be.a("string")
        done()

      @arena.createGame(@arena.waitingRoom[0..1])

  describe "#get_game_info", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: @game.positions.X.id, gameId: @game.id)

    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        should.not.exist(err)
        gameInfo.position.should.equal(types.Position.X)
        done()

