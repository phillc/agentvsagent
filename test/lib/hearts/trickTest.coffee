Trick = require "../../../lib/hearts/trick"
Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"

describe "Trick", ->
  describe "#winner", ->
    beforeEach ->
      @trick = new Trick("south")

    it "returns north when north is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.FOUR))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.ACE))

      expect(@trick.winner()).to.equal("north")

    it "returns east when east is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.FOUR))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.ACE))

      expect(@trick.winner()).to.equal("east")

    it "returns south when south is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.ACE))
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))

      expect(@trick.winner()).to.equal("south")

    it "returns west when west is the winner", ->
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))

      expect(@trick.winner()).to.equal("west")

  describe "#score", ->
    it "returns the score", ->
      trick = new Trick("south")
      expect(trick.score()).to.equal(0)
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      expect(trick.score()).to.equal(1)
      trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      expect(trick.score()).to.equal(1)
      trick.played.addCard(new Card(Suit.HEARTS, Rank.ACE))
      expect(trick.score()).to.equal(2)
      trick.played.addCard(new Card(Suit.SPADES, Rank.QUEEN))
      expect(trick.score()).to.equal(15)

  describe "#positionsMissing", ->
    it "returns the starting position", ->
      trick = new Trick("south")
      expect(trick.positionsMissing()).to.eql(["south", "west", "north", "east"])

    it "returns the second position", ->
      trick = new Trick("south")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      expect(trick.positionsMissing()).to.eql(["west", "north", "east"])

    it "returns the third position", ->
      trick = new Trick("south")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      expect(trick.positionsMissing()).to.eql(["north", "east"])

    it "returns the fourth position", ->
      trick = new Trick("south")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      expect(trick.positionsMissing()).to.eql(["east"])

    it "returns nothing when all four cards", ->
      trick = new Trick("south")
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      expect(trick.positionsMissing()).to.eql([])


