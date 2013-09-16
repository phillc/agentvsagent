Round = require "../../../lib/hearts/round"
Pile = require "../../../lib/hearts/pile"
Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"
Trick = require "../../../lib/hearts/trick"

describe "Round", ->
  beforeEach ->
    @round = new Round()

  describe "#allHavePassed", ->
    it "is false if none have passed cards", ->
      expect(@round.allHavePassed()).to.equal(false)

    it "is false if some have passed cards", ->
      Pile.createDeck().moveCardsTo(1, @round.seats.north.passed)
      expect(@round.allHavePassed()).to.equal(false)

    it "is true if all have passed cards", ->
      Pile.createDeck().moveCardsTo(1, @round.seats.north.passed)
      Pile.createDeck().moveCardsTo(1, @round.seats.east.passed)
      Pile.createDeck().moveCardsTo(1, @round.seats.south.passed)
      Pile.createDeck().moveCardsTo(1, @round.seats.west.passed)
      expect(@round.allHavePassed()).to.equal(true)

  describe "#newTrick", ->
    it "starts the first trick with the player with the 2 of clubs", ->
      @round.seats.north.held.addCard(new Card(Suit.DIAMONDS, Rank.TWO))
      @round.seats.north.held.addCard(new Card(Suit.CLUBS, Rank.THREE))
      @round.seats.east.held.addCard(new Card(Suit.SPADES, Rank.TWO))
      @round.seats.south.held.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @round.seats.west.held.addCard(new Card(Suit.HEARTS, Rank.TWO))
      expect(@round.tricks).to.have.length(0)
      @round.newTrick()
      expect(@round.tricks).to.have.length(1)
      expect(@round.currentTrick().leader).to.equal("south")

    it "starts the next round with winner of previous round", ->
      @round.newTrick()
      trick = @round.currentTrick()
      trick.leader = "north"
      trick.played.addCard new Card(Suit.CLUBS, Rank.FOUR)
      trick.played.addCard new Card(Suit.CLUBS, Rank.THREE)
      trick.played.addCard new Card(Suit.CLUBS, Rank.TWO)
      trick.played.addCard new Card(Suit.CLUBS, Rank.ACE)
      expect(@round.tricks).to.have.length(1)
      @round.newTrick()
      expect(@round.tricks).to.have.length(2)
      expect(@round.currentTrick().leader).to.equal("west")

    it "returns the current trick", ->
      expect(@round.newTrick()).to.equal(@round.currentTrick())

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
      expect(scores.north).to.equal(0)
      expect(scores.east).to.equal(13)
      expect(scores.south).to.equal(2)
      expect(scores.west).to.equal(0)
      expect(scores.shooter).to.not.exist

    it "accounts for shooting the moon", ->
      trick = new Trick("north")
      trick.score = ->
        26
      trick.winner = ->
        "north"
      @round.tricks.push trick

      scores = @round.scores()
      expect(scores.north).to.equal(0)
      expect(scores.east).to.equal(26)
      expect(scores.south).to.equal(26)
      expect(scores.west).to.equal(26)
      expect(scores.shooter).to.equal("north")

  describe "#exchange", ->
    beforeEach ->
      @round.deal()
      @northPassedCards = @round.seats.north.dealt.cards[0..2]
      @eastPassedCards = @round.seats.east.dealt.cards[0..2]
      @southPassedCards = @round.seats.south.dealt.cards[0..2]
      @westPassedCards = @round.seats.west.dealt.cards[0..2]
      (new Pile(@northPassedCards)).copyAllCardsTo @round.seats.north.passed
      (new Pile(@eastPassedCards)).copyAllCardsTo @round.seats.east.passed
      (new Pile(@southPassedCards)).copyAllCardsTo @round.seats.south.passed
      (new Pile(@westPassedCards)).copyAllCardsTo @round.seats.west.passed

    it "passes left", ->
      @round.passing = "left"

      @round.exchange()

      for card in @northPassedCards
        expect(@round.seats.north.held.findCard(card.suit, card.rank)).to.not.exist
        expect(@round.seats.north.passed.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.east.received.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.east.held.findCard(card.suit, card.rank)).to.equal(card)

    it "passes right", ->
      @round.passing = "right"

      @round.exchange()

      for card in @northPassedCards
        expect(@round.seats.north.held.findCard(card.suit, card.rank)).to.not.exist
        expect(@round.seats.north.passed.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.west.received.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.west.held.findCard(card.suit, card.rank)).to.equal(card)

    it "passes across", ->
      @round.passing = "across"

      @round.exchange()

      for card in @northPassedCards
        expect(@round.seats.north.held.findCard(card.suit, card.rank)).to.not.exist
        expect(@round.seats.north.passed.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.south.received.findCard(card.suit, card.rank)).to.equal(card)
        expect(@round.seats.south.held.findCard(card.suit, card.rank)).to.equal(card)
