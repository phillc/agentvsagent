Card = require "../../../lib/hearts/engine/card"
Suit = require "../../../lib/hearts/engine/suit"
Rank = require "../../../lib/hearts/engine/rank"

require("should")

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

  describe "score", ->
    it "is 1 for hearts", ->
      new Card(Suit.HEARTS, Rank.ACE).score().should.equal(1)
      new Card(Suit.HEARTS, Rank.KING).score().should.equal(1)
      new Card(Suit.HEARTS, Rank.QUEEN).score().should.equal(1)
      new Card(Suit.HEARTS, Rank.THREE).score().should.equal(1)
      new Card(Suit.HEARTS, Rank.TWO).score().should.equal(1)

    it "is 0 for clubs", ->
      new Card(Suit.CLUBS, Rank.ACE).score().should.equal(0)
      new Card(Suit.CLUBS, Rank.KING).score().should.equal(0)
      new Card(Suit.CLUBS, Rank.QUEEN).score().should.equal(0)
      new Card(Suit.CLUBS, Rank.THREE).score().should.equal(0)
      new Card(Suit.CLUBS, Rank.TWO).score().should.equal(0)

    it "is 0 for diamonds", ->
      new Card(Suit.DIAMONDS, Rank.ACE).score().should.equal(0)
      new Card(Suit.DIAMONDS, Rank.KING).score().should.equal(0)
      new Card(Suit.DIAMONDS, Rank.QUEEN).score().should.equal(0)
      new Card(Suit.DIAMONDS, Rank.THREE).score().should.equal(0)
      new Card(Suit.DIAMONDS, Rank.TWO).score().should.equal(0)

    it "is 13 for the queen of spades", ->
      new Card(Suit.SPADES, Rank.ACE).score().should.equal(0)
      new Card(Suit.SPADES, Rank.KING).score().should.equal(0)
      new Card(Suit.SPADES, Rank.QUEEN).score().should.equal(13)
      new Card(Suit.SPADES, Rank.THREE).score().should.equal(0)
      new Card(Suit.CLUBS, Rank.TWO).score().should.equal(0)

