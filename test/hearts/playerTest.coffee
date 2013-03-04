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

    it "returns the gameId if later broadcasted", (done) ->
      @player.waitForHand (cards) ->
        cards.should.eql ["1", "2"]
        done()
      @player.emit "dealt", ["1", "2"]
