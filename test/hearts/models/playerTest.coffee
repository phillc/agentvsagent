Player = require("../../../app/hearts/models/player")
require("chai").should()

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an empty held pile", ->
    @player.held.cards.should.eql([])

  it "has an empty taken tricks", ->
    @player.taken.should.eql([])

  describe "#getCardsToPass", ->
    it "gets the cards to pass from the agent", (done) ->
      @player.getCardsToPass ->
        done()

    it "calls the callback", (done) ->
      @player.getCardsToPass done

