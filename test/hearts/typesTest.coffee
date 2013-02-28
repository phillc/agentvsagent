types = require("../../hearts/lib/hearts_types")
require("chai").should()

describe "types", ->
  describe "Card", ->
    beforeEach ->
      @card = new Card(suit: types.Suit.SPADES, rank: types.Rank.FOUR)

    it "has a suit", ->
      @card.suit.should.equal(types.Suit.SPADES)

    it "has a rank", ->
      @card.rank.should.equal(types.Rank.FOUR)

