Card = require("../../../hearts/game/card")
Suit = require("../../../hearts/game/suit")
Rank = require("../../../hearts/game/rank")

require("chai").should()

describe "Card", ->
  beforeEach ->
    @suit = Suit.all()[0]
    @rank = Rank.all()[0]
    @card = new Card(@suit, @rank)

  it "has a suit", ->
    @card.suit.should.equal(@suit)

  it "has a rank", ->
    @card.rank.should.equal(@rank)

  describe "::all", ->
    beforeEach ->
      @cards = Card.all()

    it "has 52 cards", ->
      @cards.length.should.equal(52)

    it "has 13 diamonds", ->
      diamonds = @cards.filter (card) ->
        card.suit.name == 'diamonds'
      diamonds.length.should.equal(13)

    it "has 4 J's", ->
      jacks = @cards.filter (card) ->
        card.rank.name == 'J'
      jacks.length.should.equal(4)
