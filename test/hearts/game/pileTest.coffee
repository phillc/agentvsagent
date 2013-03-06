Pile = require("../../../hearts/game/pile")
Card = require("../../../hearts/game/card")
Suit = require("../../../hearts/game/suit")
Rank = require("../../../hearts/game/rank")
should = require("should")

describe "Pile", ->
  beforeEach ->
    @pile = new Pile()

  it "starts with no cards", ->
    @pile.cards.length.should.equal(0)

  describe "::createDeck", ->
    it "makes a pile of all 52 cards", ->
      Pile.createDeck().should.have.property('cards').with.length(52)

  describe "::createShuffledDeck", ->
    it "shuffles the pile of 52", ->
      Pile.createShuffledDeck().should.have.property('cards').with.length(52)

  describe "#addCard", ->
    it "adds a card", ->
      @pile.addCard(Card.all()[0])
      @pile.cards.length.should.equal(1)

  describe "#allOfSuit", ->
    beforeEach ->
      @pile = Pile.createDeck()

    it "returns a pile with all the clubs", ->
      @pile.allOfSuit(Suit.CLUBS).cards.length.should.equal(13)

    it "returns a pile with all the diamonds", ->
      @pile.allOfSuit(Suit.DIAMONDS).cards.length.should.equal(13)

    it "returns a pile with all the spades", ->
      @pile.allOfSuit(Suit.SPADES).cards.length.should.equal(13)

    it "returns a pile with all the hearts", ->
      @pile.allOfSuit(Suit.HEARTS).cards.length.should.equal(13)

  describe "#highestRankedCard", ->
    beforeEach ->
      @pile = new Pile()

    it "returns the highest card of the same suit", ->
      @pile.cards.push(new Card(Suit.DIAMONDS, Rank.FOUR))
      @pile.cards.push(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @pile.cards.push(new Card(Suit.HEARTS, Rank.ACE))
      @pile.cards.push(new Card(Suit.HEARTS, Rank.KING))

      card = @pile.highestRankedCard()
      card.suit.should.equal Suit.HEARTS
      card.rank.should.equal Rank.ACE

  describe "#moveCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "moves the first n cards to the other pile", ->
      @pile.moveCardsTo(2, @otherPile)

      @pile.cards.length.should.equal(1)
      @otherPile.cards.length.should.equal(2)

  describe "#moveAllCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "moves all cards to the other pile", ->
      @pile.moveAllCardsTo(@otherPile)

      @pile.cards.length.should.equal(0)
      @otherPile.cards.length.should.equal(3)

  describe "#copyAllCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "copies all cards to the other pile", ->
      @pile.copyAllCardsTo(@otherPile)

      @pile.cards.length.should.equal(3)
      @otherPile.cards.length.should.equal(3)

  describe "findCard", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))

    it "returns the card", ->
      @pile.findCard(Suit.CLUBS, Rank.TWO).suit.should.equal(Suit.CLUBS)
      @pile.findCard(Suit.CLUBS, Rank.TWO).rank.should.equal(Rank.TWO)

    it "returns null if card not in the pile", ->
      should.not.exist(@pile.findCard(Suit.CLUBS, Rank.THREE))

