Round = require "../../../hearts/game/round"
Pile = require "../../../hearts/game/pile"
should = require("should")

describe "Round", ->
  describe "#allHavePassed", ->
    beforeEach ->
      @round = new Round()
    it "is false if none have passed cards", ->
      @round.allHavePassed().should.equal(false)

    it "is false if some have passed cards", ->
      Pile.createDeck().moveCardsTo 1, @round.north.passed
      @round.allHavePassed().should.equal(false)

    it "is true if all have passed cards", ->
      Pile.createDeck().moveCardsTo 1, @round.north.passed
      Pile.createDeck().moveCardsTo 1, @round.east.passed
      Pile.createDeck().moveCardsTo 1, @round.south.passed
      Pile.createDeck().moveCardsTo 1, @round.west.passed
      @round.allHavePassed().should.equal(true)



