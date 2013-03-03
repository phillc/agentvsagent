Pile = require("../../../hearts/game/pile")
Card = require("../../../hearts/game/card")
Suit = require("../../../hearts/game/suit")
require("chai").should()

describe "Pile", ->
  beforeEach ->
    @pile = new Pile()

  it "starts with no cards", ->
    @pile.cards.length.should.equal(0)

  describe "::createDeck", ->
    it "makes a pile of all 52 cards", ->
      Pile.createDeck().should.have.property('cards').with.length(52)

  describe "#addCard", ->
    it "adds a card", ->
      @pile.addCard(Card.all()[0])
      @pile.cards.length.should.equal(1)

  describe "#allOfSuit", ->
    beforeEach ->
      @pile = Pile.createDeck()

    it "returns all the clubs", ->
      @pile.allOfSuit(Suit.CLUBS).length.should.equal(13)

    it "returns all the diamonds", ->
      @pile.allOfSuit(Suit.DIAMONDS).length.should.equal(13)

    it "returns all the spades", ->
      @pile.allOfSuit(Suit.SPADES).length.should.equal(13)

    it "returns all the hearts", ->
      @pile.allOfSuit(Suit.HEARTS).length.should.equal(13)

  describe "#moveCardsTo", ->
    beforeEach ->
      @pile.addCard(new Card('clubs', 2))
      @pile.addCard(new Card('diamonds', 9))
      @pile.addCard(new Card('spades', 3))

      @otherPile = new Pile()

    it "removes the first n cards to the other pile", ->
      @pile.moveCardsTo(2, @otherPile)

      @pile.cards.length.should.equal(1)
      @otherPile.cards.length.should.equal(2)
