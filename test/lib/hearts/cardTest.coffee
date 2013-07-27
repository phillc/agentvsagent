Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"

describe "Card", ->
  beforeEach ->
    @suit = Suit.all()[0]
    @rank = Rank.all()[0]
    @card = new Card(@suit, @rank)

  it "has a suit", ->
    expect(@card.suit).to.equal(@suit)

  it "has a rank", ->
    expect(@card.rank).to.equal(@rank)

  describe "::all", ->
    beforeEach ->
      @cards = Card.all()

    it "has 52 cards", ->
      expect(@cards).to.have.length(52)

    it "has 13 diamonds", ->
      diamonds = @cards.filter (card) ->
        card.suit.name == 'diamonds'
      expect(diamonds).to.have.length(13)

    it "has 4 J's", ->
      jacks = @cards.filter (card) ->
        card.rank.name == 'J'
      expect(jacks).to.have.length(4)

  describe "score", ->
    it "is 1 for hearts", ->
      expect(new Card(Suit.HEARTS, Rank.ACE).score()).to.equal(1)
      expect(new Card(Suit.HEARTS, Rank.KING).score()).to.equal(1)
      expect(new Card(Suit.HEARTS, Rank.QUEEN).score()).to.equal(1)
      expect(new Card(Suit.HEARTS, Rank.THREE).score()).to.equal(1)
      expect(new Card(Suit.HEARTS, Rank.TWO).score()).to.equal(1)

    it "is 0 for clubs", ->
      expect(new Card(Suit.CLUBS, Rank.ACE).score()).to.equal(0)
      expect(new Card(Suit.CLUBS, Rank.KING).score()).to.equal(0)
      expect(new Card(Suit.CLUBS, Rank.QUEEN).score()).to.equal(0)
      expect(new Card(Suit.CLUBS, Rank.THREE).score()).to.equal(0)
      expect(new Card(Suit.CLUBS, Rank.TWO).score()).to.equal(0)

    it "is 0 for diamonds", ->
      expect(new Card(Suit.DIAMONDS, Rank.ACE).score()).to.equal(0)
      expect(new Card(Suit.DIAMONDS, Rank.KING).score()).to.equal(0)
      expect(new Card(Suit.DIAMONDS, Rank.QUEEN).score()).to.equal(0)
      expect(new Card(Suit.DIAMONDS, Rank.THREE).score()).to.equal(0)
      expect(new Card(Suit.DIAMONDS, Rank.TWO).score()).to.equal(0)

    it "is 13 for the queen of spades", ->
      expect(new Card(Suit.SPADES, Rank.ACE).score()).to.equal(0)
      expect(new Card(Suit.SPADES, Rank.KING).score()).to.equal(0)
      expect(new Card(Suit.SPADES, Rank.QUEEN).score()).to.equal(13)
      expect(new Card(Suit.SPADES, Rank.THREE).score()).to.equal(0)
      expect(new Card(Suit.CLUBS, Rank.TWO).score()).to.equal(0)

