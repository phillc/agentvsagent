Seat = require "../../../hearts/game/seat"
Pile = require "../../../hearts/game/pile"
should = require("should")

describe "Seat", ->
  describe "#hasPassed", ->
    beforeEach ->
      @seat = new Seat()
    it "is false if no passed cards", ->
      @seat.hasPassed().should.equal(false)

    it "is true if passed cards", ->
      Pile.createDeck().moveCardsTo 1, @seat.passed
      @seat.hasPassed().should.equal(true)

