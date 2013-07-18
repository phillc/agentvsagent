Handler = require '../../../service/tic_tac_toe/handler'
types = require '../../../service/tic_tac_toe/types/tic_tac_toe_types'
TicTacToe = require '../../../service/tic_tac_toe/types/TicTacToe'
TicTacToeBuilder = require '../../../lib/tic_tac_toe/builder'

describe "Handler", ->
  beforeEach ->
    @handler = new Handler()

  it "implements everything declared in the service", ->
    serviceMethods = Object.keys(TicTacToe.Client.prototype).filter (method) ->
      method[0..3] != "send" && method[0..3] != "recv"

    for method in serviceMethods
      expect(@handler).to.have.property(method)
      expect(@handler[method]).to.have.length(TicTacToe.Client.prototype[method].length)

    handlerMethods = Object.keys(Handler.prototype).filter((name) -> name[0] != "_" && name != "constructor")

    expect(handlerMethods).to.eql(serviceMethods)

  describe "#enter_arena", ->
    it "returns when joined", (done) ->
      @handler.on 'connect', (connection) ->
        connection.on 'join', ->
          connection.send("joined")

      @handler.enter_arena new types.EntryRequest(), (err, response) =>
        expect(err).to.not.exist
        expect(response.ticket.agentId).to.be.a("string")
        done()

  describe "#get_game_info", ->
    beforeEach ->
      agentId = @handler._createAgent()
      @agent = @handler._getAgent(agentId)
      @ticket = new types.Ticket(agentId: agentId)

    it "returns when the move is sent", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        expect(err).to.not.exist
        expect(gameInfo.position).to.equal(types.Position.X)
        done()

      @agent.send("turn", position: "X")

  describe "#make_move", ->
    beforeEach ->
      agentId = @handler._createAgent()
      @agent = @handler._getAgent(agentId)
      @ticket = new types.Ticket(agentId: agentId)

    it "returns when the move is sent", (done) ->
      @handler.make_move @ticket, 0, 0, 0, 0, (err, moveResult) ->
        expect(err).to.not.exist
        expect(moveResult.opponents_move.boardRow).to.equal(0)
        done()

      @agent.send("turn", position: "X")


