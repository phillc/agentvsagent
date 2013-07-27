types = require("../../../service/hearts/types/hearts_types")

describe "types", ->
  describe "Card", ->
    beforeEach ->
      @card = new types.Card(suit: types.Suit.SPADES, rank: types.Rank.FOUR)

    it "has a suit", ->
      expect(@card.suit).to.equal(types.Suit.SPADES)

    it "has a rank", ->
      expect(@card.rank).to.equal(types.Rank.FOUR)

