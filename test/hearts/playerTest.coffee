Player = require("../../hearts/player")
require("chai").should()

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    @player.should.have.property('id')

  # it "has an empty held pile", ->
  #   @player.held.cards.should.eql([])

  describe "#waitForGame", ->
    it "returns the gameId if previously broadcasted", (done) ->
      @player.emit "start", "12345"
      @player.waitForGame (gameId) ->
        gameId.should.equal("12345")
        done()

    it "returns the gameId if later broadcasted", (done) ->
      @player.waitForGame (gameId) ->
        gameId.should.equal("12345")
        done()
      @player.emit "start", "12345"
# 
#   it "has an empty taken tricks", ->
#     @player.takenTricks.should.eql([])

  # describe "#getCardsToPass", ->
  #   it "gets the cards to pass from the agent", (done) ->
  #     @player.getCardsToPass ->
  #       done()

  #   it "calls the callback", (done) ->
  #     @player.getCardsToPass done

