Pile = require "../../../lib/hearts/pile"
Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"

describe "Pile", ->
  beforeEach ->
    @pile = new Pile()

  it "starts with no cards", ->
    expect(@pile.cards).to.have.length(0)

  describe "::createDeck", ->
    it "makes a pile of all 52 cards", ->
      expect(Pile.createDeck()).to.have.property('cards').with.length(52)

  describe "::createShuffledDeck", ->
    it "shuffles the pile of 52", ->
      expect(Pile.createShuffledDeck()).to.have.property('cards').with.length(52)

  describe "#addCard", ->
    it "adds a card", ->
      @pile.addCard(Card.all()[0])
      expect(@pile.cards).to.have.length(1)

  describe "#allOfSuit", ->
    beforeEach ->
      @pile = Pile.createDeck()

    it "returns a pile with all the clubs", ->
      expect(@pile.allOfSuit(Suit.CLUBS).cards).to.have.length(13)

    it "returns a pile with all the diamonds", ->
      expect(@pile.allOfSuit(Suit.DIAMONDS).cards).to.have.length(13)

    it "returns a pile with all the spades", ->
      expect(@pile.allOfSuit(Suit.SPADES).cards).to.have.length(13)

    it "returns a pile with all the hearts", ->
      expect(@pile.allOfSuit(Suit.HEARTS).cards).to.have.length(13)

  describe "#highestRankedCard", ->
    beforeEach ->
      @pile = new Pile()

    it "returns the highest card of the same suit", ->
      @pile.cards.push(new Card(Suit.DIAMONDS, Rank.FOUR))
      @pile.cards.push(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @pile.cards.push(new Card(Suit.HEARTS, Rank.ACE))
      @pile.cards.push(new Card(Suit.HEARTS, Rank.KING))

      card = @pile.highestRankedCard()
      expect(card.suit).to.equal Suit.HEARTS
      expect(card.rank).to.equal Rank.ACE

  describe "#moveCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "moves the first n cards to the other pile", ->
      @pile.moveCardsTo(2, @otherPile)

      expect(@pile.cards).to.have.length(1)
      expect(@otherPile.cards).to.have.length(2)

  describe "#moveAllCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "moves all cards to the other pile", ->
      @pile.moveAllCardsTo(@otherPile)

      expect(@pile.cards).to.have.length(0)
      expect(@otherPile.cards).to.have.length(3)

  describe "#copyAllCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.DIAMONDS, Rank.NINE))
      @pile.addCard(new Card(Suit.SPADES, Rank.THREE))

      @otherPile = new Pile()

    it "copies all cards to the other pile", ->
      @pile.copyAllCardsTo(@otherPile)

      expect(@pile.cards).to.have.length(3)
      expect(@otherPile.cards).to.have.length(3)

  describe "#moveCardTo", ->
    it "moves a card from one pile to another", ->
      anotherPile = new Pile()

      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @pile.addCard(new Card(Suit.CLUBS, Rank.THREE))
      @pile.addCard(new Card(Suit.CLUBS, Rank.FOUR))
      @pile.addCard(new Card(Suit.CLUBS, Rank.FIVE))
      expect(@pile.cards).to.have.length(4)
      expect(anotherPile.cards).to.have.length(0)

      cardToMove = new Card(Suit.CLUBS, Rank.FOUR)
      @pile.moveCardTo(cardToMove, anotherPile)

      expect(@pile.findCard(cardToMove.suit, cardToMove.rank)).to.not.exist
      expect(@pile.cards).to.have.length(3)
      expect(anotherPile.cards).to.have.length(1)
      expect(anotherPile.findCard(cardToMove.suit, cardToMove.rank).rank).to.equal cardToMove.rank

  describe "#findCard", ->
    beforeEach ->
      @pile.addCard(new Card(Suit.CLUBS, Rank.TWO))

    it "returns the card", ->
      expect(@pile.findCard(Suit.CLUBS, Rank.TWO).suit).to.equal(Suit.CLUBS)
      expect(@pile.findCard(Suit.CLUBS, Rank.TWO).rank).to.equal(Rank.TWO)

    it "returns null if card not in the pile", ->
      expect(@pile.findCard(Suit.CLUBS, Rank.THREE)).to.not.exist

