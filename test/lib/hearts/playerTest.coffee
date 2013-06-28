Player = require "../../../lib/hearts/player"
should = require("should")

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    @player.should.have.property('id')

  describe "startedGame", ->
    it "returns the gameId if previously broadcasted", (done) ->
      @player.out.sendStartedGame "12345"
      @player.out.recvStartedGame (err, gameId) ->
        gameId.should.equal("12345")
        done()

    it "returns the gameId if later broadcasted", (done) ->
      @player.out.recvStartedGame (err, gameId) ->
        gameId.should.equal("12345")
        done()
      @player.out.sendStartedGame "12345"

  describe "dealt", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.out.sendDealt ["1", "2"]
      @player.out.recvDealt (err, cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.out.recvDealt (err, cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.out.sendDealt ["1", "2"]

  describe "passed", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.out.sendPassed ["1", "2"]
      @player.out.recvPassed (err, cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.out.recvPassed (err, cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.out.sendPassed ["1", "2"]

  describe "turn", ->
    it "returns the trick if previously broadcasted", (done) ->
      @player.out.sendTurn {leader: "north"}
      @player.out.recvTurn (err, trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the trick if later broadcasted", (done) ->
      @player.out.recvTurn (err, trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.out.sendTurn {leader: "north"}

  describe "endTrick", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.out.sendEndTrick {leader: "north"}
      @player.out.recvEndTrick (err, trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.out.recvEndTrick (err, trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.out.sendEndTrick {leader: "north"}

  describe "endRound", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.out.sendEndRound "foo", "bar"
      @player.out.recvEndRound (err, round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.out.recvEndRound (err, round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()
      @player.out.sendEndRound "foo", "bar"

  describe "raiseError", ->
    it "returns an exception to any future callback", (done) ->
      @player.raiseError("foo")
      @player.out.recvPassed (err, cards) ->
        err.should.eql("foo")
        should.not.exist(cards)
        done()

    it "returns an exception to any existing callback", (done) ->
      @player.out.recvPassed (err, cards) ->
        err.should.eql("foo")
        should.not.exist(cards)
        done()
      @player.raiseError("foo")

    it "trumps the queue", (done) ->
      @player.out.sendPassed ["1", "2"]
      @player.raiseError("foo")
      @player.out.recvPassed (err, cards) ->
        err.should.eql("foo")
        should.not.exist(cards)
        done()

    it "remains in front of the queue", (done) ->
      @player.out.sendPassed ["1", "2"]
      @player.raiseError("foo")
      @player.out.sendPassed ["1", "2"]
      @player.out.recvPassed (err, cards) ->
        err.should.eql("foo")
        should.not.exist(cards)
        done()


