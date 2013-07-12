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

  it "implements everything declared in the service", ->
    serviceMethods = Object.keys(TicTacToe.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in serviceMethods
      expect(@handler).to.have.property(method)
      expect(@handler[method]).to.have.length(TicTacToe.Client.prototype[method].length)

    expect(Object.keys(Handler.prototype).filter((name) -> name[0] != "_").length).to.equal(serviceMethods.length)

  describe "#enter_arena", ->
    it "returns when there is a game to be played", (done) ->
      @handler.enter_arena new types.EntryRequest(), (err, response) =>
        expect(err).to.not.exist
        expect(response.ticket.agentId).to.be.a("string")
        expect(response.ticket.gameId).to.be.a("string")
        done()

      @arena.createGame(@arena.waitingRoom[0..2])

  describe "#get_game_info", ->
    beforeEach ->
      @arena.createPlayer()
      @game = @arena.createGame(@arena.waitingRoom[0..2])
      @ticket = new types.Ticket(agentId: @game.positions.X.id, gameId: @game.id)

    it "returns when the first move is ready", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        expect(err).to.not.exist
        expect(gameInfo.position).to.equal(types.Position.X)
        done()

      @game.engine.transition("waitingForX")




