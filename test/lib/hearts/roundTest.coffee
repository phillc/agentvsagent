Round = require "../../../lib/hearts/round"
Pile = require "../../../lib/hearts/pile"
Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"
Trick = require "../../../lib/hearts/trick"
should = require("should")

describe "Round", ->
  beforeEach ->
    @round = new Round()

  describe "#allHavePassed", ->
    it "is false if none have passed cards", ->
      @round.allHavePassed().should.equal(false)

    it "is false if some have passed cards", ->
      Pile.createDeck().moveCardsTo 1, @round.north.passed
      @round.allHavePassed().should.equal(false)

    it "is true if all have passed cards", ->
      Pile.createDeck().moveCardsTo 1, @round.north.passed
      Pile.createDeck().moveCardsTo 1, @round.east.passed
      Pile.createDeck().moveCardsTo 1, @round.south.passed
      Pile.createDeck().moveCardsTo 1, @round.west.passed
      @round.allHavePassed().should.equal(true)

  describe "#newTrick", ->
    it "starts the first trick with the player with the 2 of clubs", ->
      @round.north.held.addCard(new Card(Suit.DIAMONDS, Rank.TWO))
      @round.north.held.addCard(new Card(Suit.CLUBS, Rank.THREE))
      @round.east.held.addCard(new Card(Suit.SPADES, Rank.TWO))
      @round.south.held.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @round.west.held.addCard(new Card(Suit.HEARTS, Rank.TWO))
      @round.tricks.should.have.length(0)
      @round.newTrick()
      @round.tricks.should.have.length(1)
      @round.currentTrick().leader.should.equal("south")

    it "starts the next round with winner of previous round", ->
      @round.newTrick()
      trick = @round.currentTrick()
      trick.leader = "north"
      trick.played.addCard new Card(Suit.CLUBS, Rank.FOUR)
      trick.played.addCard new Card(Suit.CLUBS, Rank.THREE)
      trick.played.addCard new Card(Suit.CLUBS, Rank.TWO)
      trick.played.addCard new Card(Suit.CLUBS, Rank.ACE)
      @round.tricks.should.have.length(1)
      @round.newTrick()
      @round.tricks.should.have.length(2)
      @round.currentTrick().leader.should.equal("west")

  describe "#scores", ->
    it "returns the scores", ->
      trick1 = new Trick("north")
      trick1.played.addCard(new Card(Suit.SPADES, Rank.THREE))
      trick1.played.addCard(new Card(Suit.SPADES, Rank.ACE)) #east wins 13
      trick1.played.addCard(new Card(Suit.SPADES, Rank.QUEEN))
      trick1.played.addCard(new Card(Suit.SPADES, Rank.FOUR))

      trick2 = new Trick("east")
      trick2.played.addCard(new Card(Suit.SPADES, Rank.TWO))
      trick2.played.addCard(new Card(Suit.SPADES, Rank.KING)) #south wins 1
      trick2.played.addCard(new Card(Suit.SPADES, Rank.FIVE))
      trick2.played.addCard(new Card(Suit.HEARTS, Rank.FOUR))

      trick3 = new Trick("south")
      trick3.played.addCard(new Card(Suit.HEARTS, Rank.FIVE)) #south wins 1
      trick3.played.addCard(new Card(Suit.CLUBS, Rank.FOUR))
      trick3.played.addCard(new Card(Suit.CLUBS, Rank.QUEEN))
      trick3.played.addCard(new Card(Suit.CLUBS, Rank.THREE))

      @round.tricks.push trick1
      @round.tricks.push trick2
      @round.tricks.push trick3
      scores = @round.scores()
      scores.north.should.equal(0)
      scores.east.should.equal(13)
      scores.south.should.equal(2)
      scores.west.should.equal(0)

    it "accounts for shooting the moon", ->
      trick = new Trick("north")
      trick.score = ->
        26
      trick.winner = ->
        "north"
      @round.tricks.push trick

      scores = @round.scores()
      scores.north.should.equal(0)
      scores.east.should.equal(26)
      scores.south.should.equal(26)
      scores.west.should.equal(26)



