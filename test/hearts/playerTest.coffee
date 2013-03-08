Player = require("../../hearts/player")
require("should")

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    @player.should.have.property('id')

  describe "#waitForGame", ->
    it "returns the gameId if previously broadcasted", (done) ->
      @player.emit "started", "12345"
      @player.waitForGame (gameId) ->
        gameId.should.equal("12345")
        done()

    it "returns the gameId if later broadcasted", (done) ->
      @player.waitForGame (gameId) ->
        gameId.should.equal("12345")
        done()
      @player.emit "started", "12345"

  describe "#waitForHand", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.emit "dealt", ["1", "2"]
      @player.waitForHand (cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.waitForHand (cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.emit "dealt", ["1", "2"]

  describe "#waitForPassed", ->
    it "returns the cards if previously broadcasted", (done) ->
      @player.emit "passed", ["1", "2"]
      @player.waitForPassed (cards) ->
        cards.should.eql ["1", "2"]
        done()

    it "returns the cards if later broadcasted", (done) ->
      @player.waitForPassed (cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.emit "passed", ["1", "2"]

  describe "#waitForTurn", ->
    it "returns the trick if previously broadcasted", (done) ->
      @player.emit "turn", {leader: "north"}
      @player.waitForTurn (trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the trick if later broadcasted", (done) ->
      @player.waitForTurn (trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.emit "turn", {leader: "north"}

  describe "#waitForTrickFinished", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.emit "endTrick", {leader: "north"}
      @player.waitForTrickFinished (trick) ->
        trick.should.eql {leader: "north"}
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.waitForTrickFinished (trick) ->
        trick.should.eql {leader: "north"}
        done()
      @player.emit "endTrick", {leader: "north"}

  describe "#waitForRoundEnd", ->
    it "returns the hand if previously broadcasted", (done) ->
      @player.emit "endRound", "foo", "bar"
      @player.waitForRoundEnd (round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()

    it "returns the hand if later broadcasted", (done) ->
      @player.waitForRoundEnd (round, status) ->
        round.should.equal "foo"
        status.should.equal "bar"
        done()
      @player.emit "endRound", "foo", "bar"


