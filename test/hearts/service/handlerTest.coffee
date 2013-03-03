Arena = require '../../../hearts/arena'
Handler = require '../../../hearts/service/handler'
types = require '../../../hearts/lib/hearts_types'
Factory = require '../factory'
should = require("chai").should()

describe "Handler", ->
  beforeEach ->
    @arena = new Arena()
    @arena.createPlayer()
    @arena.createPlayer()
    @arena.createPlayer()
    @handler = new Handler(@arena)

  it "implements everything declared in the service"

  describe "#enter_arena", ->
    it "returns when there is a game to be played", (done) ->
      @handler.enter_arena (err, response) ->
        should.not.exist(err)
        response.ticket.agentId.should.be.a("string")
        response.ticket.gameId.should.be.a("string")
        done()

      @arena.createGame(@arena.waitingRoom[0..3])

  describe "#get_game_info", ->
    beforeEach ->
      @ticket = new types.Ticket(agentId: "123", gameId: "456")

    it "returns game info", (done) ->
      @handler.get_game_info @ticket, (err, gameInfo) ->
        should.not.exist(err)
        gameInfo.position.should.equal(types.Position.NORTH)
        done()

  describe.skip "#get_hand", ->
  # describe "#get_hand", ->
    beforeEach ->
      @game = Factory.createGame(arena: @arena)
      @ticket = new types.Ticket(agentId: "123", gameId: @game.id)

    it "returns the players hand", (done) ->
      @handler.get_hand @ticket, (err, hand) ->
        should.not.exist(err)
        hand.length.should.equal(13)
        done()


