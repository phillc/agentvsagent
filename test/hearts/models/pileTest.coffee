Pile = require("../../../app/hearts/models/pile")
Card = require("../../../app/hearts/models/card")
require("chai").should()

describe "Pile", ->
  beforeEach ->
    @pile = new Pile()

  it "starts with no cards", ->
    @pile.cards.should.eql([])

  describe "#addCard", ->
    it "adds a card", ->
      @pile.addCard(new Card('spades', 4))
      @pile.cards.length.should.equal(1)

  describe "#allOfSuit", ->
    beforeEach ->
      @pile.addCard(new Card('clubs', 2))
      @pile.addCard(new Card('clubs', 9))
      @pile.addCard(new Card('clubs', 10))
      @pile.addCard(new Card('clubs', 'Q'))
      @pile.addCard(new Card('clubs', 'K'))
      @pile.addCard(new Card('diamonds', 4))
      @pile.addCard(new Card('diamonds', 5))
      @pile.addCard(new Card('diamonds', 6))
      @pile.addCard(new Card('diamonds', 9))
      @pile.addCard(new Card('spades', 3))
      @pile.addCard(new Card('spades', 9))
      @pile.addCard(new Card('spades', 'A'))
      @pile.addCard(new Card('hearts', 2))
      @pile.addCard(new Card('hearts', 'J'))

    it "returns all the clubs", ->
      @pile.allOfSuit('clubs').length.should.equal(5)

    it "returns all the diamonds", ->
      @pile.allOfSuit('diamonds').length.should.equal(4)

    it "returns all the spades", ->
      @pile.allOfSuit('spades').length.should.equal(3)

    it "returns all the hearts", ->
      @pile.allOfSuit('hearts').length.should.equal(2)

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
