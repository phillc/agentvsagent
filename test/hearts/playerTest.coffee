Player = require "../../lib/hearts/player"
require("should")

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    @player.should.have.property('id')

  describe "startedGame", ->
    it "returns the gameId if previously broadcasted", (done) ->
      @player.sendStartedGame "12345"
      @player.recvStartedGame (gameId) ->
        gameId.should.equal("12345")
        done()

    it "returns the gameId if later broadcasted", (done) ->
      @player.recvStartedGame (gameId) ->
        gameId.should.equal("12345")
        done()
      @player.sendStartedGame "12345"

  describe "dealt", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.sendDealt ["1", "2"]
      @player.recvDealt (cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.recvDealt (cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.sendDealt ["1", "2"]

  describe "passed", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.sendPassed ["1", "2"]
      @player.recvPassed (cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.recvPassed (cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.sendPassed ["1", "2"]

  describe "turn", ->
    it "returns the trick if previously broadcasted", (done) ->
      @player.sendTurn {leader: "north"}
      @player.recvTurn (trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the trick if later broadcasted", (done) ->
      @player.recvTurn (trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.sendTurn {leader: "north"}

  describe "endTrick", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.sendEndTrick {leader: "north"}
      @player.recvEndTrick (trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.recvEndTrick (trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.sendEndTrick {leader: "north"}

  describe "endRound", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.sendEndRound "foo", "bar"
      @player.recvEndRound (round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.recvEndRound (round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()
      @player.sendEndRound "foo", "bar"


