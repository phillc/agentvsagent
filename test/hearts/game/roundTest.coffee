Round = require "../../../hearts/game/round"
Pile = require "../../../hearts/game/pile"
Suit = require "../../../hearts/game/suit"
Rank = require "../../../hearts/game/rank"
Card = require "../../../hearts/game/card"
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
      trick.played.addCard new Card(Suit.CLUBS, Rank.FOUR)
      trick.played.addCard new Card(Suit.CLUBS, Rank.THREE)
      trick.played.addCard new Card(Suit.CLUBS, Rank.TWO)
      trick.played.addCard new Card(Suit.CLUBS, Rank.ACE)
      @round.tricks.should.have.length(1)
      @round.newTrick()
      @round.tricks.should.have.length(2)
      @round.currentTrick().leader.should.equal("west")




