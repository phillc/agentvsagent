Seat = require "../../../src/server/hearts/seat"
Pile = require "../../../src/server/hearts/pile"

describe "Seat", ->
  describe "#hasPassed", ->
    beforeEach ->
      @seat = new Seat()

    it "is false if no passed cards", ->
      expect(@seat.hasPassed()).to.equal(false)

    it "is true if passed cards", ->
      Pile.createDeck().moveCardsTo 1, @seat.passed
      expect(@seat.hasPassed()).to.equal(true)
