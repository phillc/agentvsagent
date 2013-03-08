Trick = require "../../../hearts/game/trick"
Card = require "../../../hearts/game/card"
Suit = require "../../../hearts/game/suit"
Rank = require "../../../hearts/game/rank"

describe "Trick", ->
  describe "#winner", ->
    beforeEach ->
      @trick = new Trick("south")

    it "returns north when north is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.FOUR))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.ACE))

      @trick.winner().should.equal("north")

    it "returns east when east is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.FOUR))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.ACE))

      @trick.winner().should.equal("east")

    it "returns south when south is the winner", ->
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.ACE))
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))

      @trick.winner().should.equal("south")

    it "returns west when west is the winner", ->
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @trick.played.addCard(new Card(Suit.CLUBS, Rank.QUEEN))
      @trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      @trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))

      @trick.winner().should.equal("west")

  describe "#score", ->
    it "returns the score", ->
      trick = new Trick("south")
      trick.score().should.equal(0)
      trick.played.addCard(new Card(Suit.HEARTS, Rank.THREE))
      trick.score().should.equal(1)
      trick.played.addCard(new Card(Suit.DIAMONDS, Rank.KING))
      trick.score().should.equal(1)
      trick.played.addCard(new Card(Suit.HEARTS, Rank.ACE))
      trick.score().should.equal(2)
      trick.played.addCard(new Card(Suit.SPADES, Rank.QUEEN))
      trick.score().should.equal(15)

