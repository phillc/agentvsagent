Game = require "../../../lib/hearts/game"
Card = require "../../../lib/hearts/card"
Suit = require "../../../lib/hearts/suit"
Rank = require "../../../lib/hearts/rank"
actions = require "../../../lib/hearts/actions"

describe "Game", ->
  beforeEach ->
    @game = new Game()

  describe "#scores", ->
    it "returns the scores", ->
      @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})
      @game.state.rounds.push({ scores: -> { north: 0, east: 0, south: 15, west: 11 }})
      @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})
      @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})

      scores = @game.scores()
      expect(scores.north).to.equal(30)
      expect(scores.east).to.equal(0)
      expect(scores.south).to.equal(60)
      expect(scores.west).to.equal(14)

  describe "#maxPenaltyReached", ->
    it "returns true if 100 is reached by a player", ->
      @game.scores = -> { north: 100, east: 10, south: 20, west: 3 }
      expect(@game.maxPenaltyReached()).to.be.true

    it "returns true if 100 is reached by multiple players", ->
      @game.scores = -> { north: 100, east: 10, south: 105, west: 3 }
      expect(@game.maxPenaltyReached()).to.be.true

    it "returns false if 100 is not reached by any player", ->
      @game.scores = -> { north: 99, east: 10, south: 5, west: 3 }
      expect(@game.maxPenaltyReached()).to.be.false

    describe "configured to end at 40", ->
      beforeEach ->
        @game = new Game(heartsMaxPoints: 40)

      it "returns false if not reached", ->
        @game.scores = -> { north: 39, east: 10, south: 0, west: 3 }
        expect(@game.maxPenaltyReached()).to.be.false

      it "returns true if reached", ->
        @game.scores = -> { north: 40, east: 10, south: 0, west: 3 }
        expect(@game.maxPenaltyReached()).to.be.true

  describe "#start", ->
    it "transitions to starting", ->
      @game.start()
      expect(@game.engine.state).to.equal("startingRound")

    it "emits north starting", (done) ->
      @game.on "north.roundStarted", (data) ->
        expect(data.position).to.equal("north")
        done()

      @game.start()

    it "emits east starting", (done) ->
      @game.on "east.roundStarted", (data) ->
        expect(data.position).to.equal("east")
        done()

      @game.start()

    it "emits south starting", (done) ->
      @game.on "south.roundStarted", (data) ->
        expect(data.position).to.equal("south")
        done()

      @game.start()

    it "emits west starting", (done) ->
      @game.on "west.roundStarted", (data) ->
        expect(data.position).to.equal("west")
        done()

      @game.start()

  describe "#startRound", ->
    it "adds another round", ->
      expect(@game.state.rounds).to.have.length(0)
      @game.startRound()
      expect(@game.state.rounds).to.have.length(1)

    it "deals cards to all players", ->
      @game.startRound()
      expect(@game.currentRound().seats.north.dealt.cards).to.have.length(13)
      expect(@game.currentRound().seats.north.held.cards).to.have.length(13)
      expect(@game.currentRound().seats.east.dealt.cards).to.have.length(13)
      expect(@game.currentRound().seats.east.held.cards).to.have.length(13)
      expect(@game.currentRound().seats.south.dealt.cards).to.have.length(13)
      expect(@game.currentRound().seats.south.held.cards).to.have.length(13)
      expect(@game.currentRound().seats.west.dealt.cards).to.have.length(13)
      expect(@game.currentRound().seats.west.held.cards).to.have.length(13)

    it "emits cards that were dealt to north", (done) ->
      @game.on "north.dealt", (data) ->
        expect(data.cards).to.have.length(13)
        done()
      @game.startRound()

    it "emits cards that were dealt to east", (done) ->
      @game.on "east.dealt", (data) ->
        expect(data.cards).to.have.length(13)
        done()
      @game.startRound()

    it "emits cards that were dealt to south", (done) ->
      @game.on "south.dealt", (data) ->
        expect(data.cards).to.have.length(13)
        done()
      @game.startRound()

    it "emits cards that were dealt to west", (done) ->
      @game.on "west.dealt", (data) ->
        expect(data.cards).to.have.length(13)
        done()
      @game.startRound()

    it "moves to passing", ->
      @game.startRound()
      expect(@game.engine.state).to.equal("passing")

    it "starts a new trick", ->
      @game.startRound()
      @game.startRound()
      @game.startRound()
      @game.startRound()
      expect(@game.engine.state).to.equal("startingTrick")

  describe "#newRound", ->
    it "passes on the first round", ->
      round = @game.newRound()
      expect(round.passing).to.equal("left")

    it "passes on the second round", ->
      @game.newRound()
      round = @game.newRound()
      expect(round.passing).to.equal("right")

    it "passes on the third round", ->
      @game.newRound()
      @game.newRound()
      round = @game.newRound()
      expect(round.passing).to.equal("across")

    it "goes straight to playing on the fourth round", ->
      @game.newRound()
      @game.newRound()
      @game.newRound()
      round = @game.newRound()
      expect(round.passing).to.not.exist

    it "starts over on the fifth round", ->
      @game.newRound()
      @game.newRound()
      @game.newRound()
      @game.newRound()
      round = @game.newRound()
      expect(round.passing).to.equal("left")

  describe "#startTrick", ->
    beforeEach ->
      @game.startRound()

    it "waits for whoever has the two of clubs", ->
      twoClubs = new Card(Suit.CLUBS, Rank.TWO)
      @game.currentRound().seats.north.held.cards.splice(0, 13)
      @game.currentRound().seats.south.held.cards.splice(0, 13)
      @game.currentRound().seats.west.held.cards.splice(0, 13)
      @game.currentRound().seats.east.held.addCard(new Card(Suit.CLUBS, Rank.TWO))
      @game.startTrick()
      expect(@game.engine.state).to.equal("waitingForCardFromEast")

    it "adds a trick to the round", ->
      expect(@game.currentRound().tricks).to.have.length(0)
      @game.startTrick()
      expect(@game.currentRound().tricks).to.have.length(1)
      @game.currentRound().currentTrick().played.addCard({})
      @game.startTrick()
      expect(@game.currentRound().tricks).to.have.length(2)

  describe "#finishTrick", ->
    beforeEach ->
      @game.startRound()
      @game.startTrick()

    it "emits the trick", (done) ->
      @game.on "north.finishedTrick", (data) =>
        expect(data).to.equal(@game.currentRound().currentTrick())
        done()

      @game.finishTrick()

    it "moves to the next trick", ->
      expect(@game.state.rounds).to.have.length(1)
      @game.finishTrick()
      expect(@game.engine.state).to.equal("startingTrick")

    it "ends the round", ->
      for i in [1..12]
        @game.currentRound().tricks.push({winner: -> "north"})
      @game.finishTrick()
      expect(@game.engine.state).to.equal("endingRound")

  describe "#finishRound", ->
    it "starts a new round if no one is over 100, and emits new round on each player", (done) ->
      @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})

      @game.on "north.roundFinished", (data) =>
        expect(@game.engine.state).to.equal('startingRound')
        expect(data.status).to.equal('nextRound')
        expect(data.roundScores.north).to.equal(10)
        expect(data.roundScores.east).to.equal(0)
        expect(data.roundScores.south).to.equal(15)
        expect(data.roundScores.west).to.equal(1)
        done()

      @game.finishRound()

    it "ends the game if someone reaches 100, and emits game end on each player", (done) ->
      @game.state.rounds.push({ scores: -> { north: 101, east: 0, south: 15, west: 1 }})

      @game.on "north.roundFinished", (data) =>
        expect(@game.engine.state).to.equal('endingGame')
        expect(data.status).to.equal('endGame')
        expect(data.roundScores.north).to.equal(101)
        expect(data.roundScores.east).to.equal(0)
        expect(data.roundScores.south).to.equal(15)
        expect(data.roundScores.west).to.equal(1)
        done()

      @game.finishRound()

  describe "#finish", ->
    beforeEach ->
      @game.state.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})
      @game.state.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})

    it "send the game scores to each player", (done) ->
      @game.on "north.end", (data) ->
        expect(data.gameScores.north).to.equal(100)
        expect(data.gameScores.east).to.equal(0)
        expect(data.gameScores.south).to.equal(30)
        expect(data.gameScores.west).to.equal(2)
        done()

      @game.finish()

    it "moves on", ->
      @game.finish()
      expect(@game.engine.state).to.equal("finished")

  describe "#waitingForCardFrom", ->
    beforeEach ->
      @game.newRound()
      @game.currentRound().newTrick()

    it "emits turn for that player", (done) ->
      @game.on "north.turn", (data) =>
        expect(data).to.equal(@game.currentRound().currentTrick())
        done()
      @game.waitingForCardFrom("north")

  describe "states", ->
    describe "startingRound", ->
      it "moves to the next state once all have checked in", ->
        @game.engine.transition("startingRound")
        @game.handle "readyForRound.north"
        expect(@game.engine.state).to.equal("startingRound")
        @game.handle "readyForRound.east"
        expect(@game.engine.state).to.equal("startingRound")
        @game.handle "readyForRound.south"
        expect(@game.engine.state).to.equal("startingRound")
        @game.handle "readyForRound.west"
        expect(@game.engine.state).to.equal("passing")

      it "response to other events with outOfSequence", (done) ->
        @game.engine.transition("startingRound")

        @game.on "north.error", (error) ->
          expect(error.type).to.equal("outOfSequence")
          done()

        @game.handle "passCards.north"
        expect(@game.engine.state).to.equal("aborted")

    describe "passing", ->
      beforeEach ->
        @game.startRound()
        @game.engine.transition("passing")
        @northCards = @game.currentRound().seats.north.held.cards[0..2].map((c) -> c.toJSON())
        @eastCards = @game.currentRound().seats.east.held.cards[0..2].map((c) -> c.toJSON())
        @southCards = @game.currentRound().seats.south.held.cards[0..2].map((c) -> c.toJSON())
        @westCards = @game.currentRound().seats.west.held.cards[0..2].map((c) -> c.toJSON())

      it "starts trick after all have passed", ->
        @game.handle "passCards.north", cards: @northCards
        expect(@game.engine.state).to.equal("passing")
        @game.handle "passCards.east", cards: @eastCards
        expect(@game.engine.state).to.equal("passing")
        @game.handle "passCards.south", cards: @southCards
        expect(@game.engine.state).to.equal("passing")
        @game.handle "passCards.west", cards: @westCards
        expect(@game.engine.state).to.equal("startingTrick")

      it "emits passed", (done) ->
        @game.on "north.received", (data) ->
          expect(data.cards).to.not.be.empty
          done()

        @game.handle "passCards.north", cards: @northCards
        @game.handle "passCards.east", cards: @eastCards
        @game.handle "passCards.south", cards: @southCards
        @game.handle "passCards.west", cards: @westCards

      it "aborts if passing invalid cards", ->
        @game.handle "passCards.east", cards: @eastCards
        @game.handle "passCards.north", cards: @northCards
        @game.handle "passCards.west", cards: @westCards

        @game.handle "passCards.south", cards: @westCards #invalid

        expect(@game.engine.state).to.equal("aborted")

      it "notifies the culprit of an invalid action", (done) ->
        @game.handle "passCards.east", cards: @eastCards
        @game.handle "passCards.north", cards: @northCards
        @game.handle "passCards.west", cards: @westCards

        @game.on "south.error", (error) ->
          expect(error.type).to.equal("invalidMove")
          done()

        @game.handle "passCards.south", cards: @westCards #invalid

      it "notifies others of an invalid action", (done) ->
        @game.handle "passCards.east", cards: @eastCards
        @game.handle "passCards.north", cards: @northCards
        @game.handle "passCards.west", cards: @westCards

        @game.on "north.error", (error) ->
          expect(error.type).to.equal("gameAborted")
          done()

        @game.handle "passCards.south", cards: @westCards #invalid

    describe "startingTrick", ->
      beforeEach ->
        @game.startRound()
        @game.engine.transition("startingTrick")

      it "waits for the first player after all are ready", ->
        @game.handle "readyForTrick.north"
        expect(@game.engine.state).to.equal("startingTrick")
        @game.handle "readyForTrick.east"
        expect(@game.engine.state).to.equal("startingTrick")
        @game.handle "readyForTrick.south"
        expect(@game.engine.state).to.equal("startingTrick")
        @game.handle "readyForTrick.west"
        expect(@game.engine.state).to.include("waitingForCardFrom")

    describe "waitingForCard", ->
      beforeEach ->
        @game.startRound()
        @northCard = new Card(Suit.CLUBS, Rank.TWO)
        @eastCard = new Card(Suit.CLUBS, Rank.THREE)
        @southCard = new Card(Suit.CLUBS, Rank.FOUR)
        @westCard = new Card(Suit.CLUBS, Rank.FIVE)
        @game.currentRound().seats.north.held.cards = [@northCard]
        @game.currentRound().seats.east.held.cards = [@eastCard]
        @game.currentRound().seats.south.held.cards = [@southCard]
        @game.currentRound().seats.west.held.cards = [@westCard]
        @game.startTrick()
        expect(@game.engine.state).to.equal("waitingForCardFromNorth")

      it "applies the card to the player", ->
        @game.handle "playCard.north", card: @northCard.toJSON()

        expect(@game.currentRound().currentTrick().played.cards[0]).to.equal(@northCard)

      it "waits for the next player", ->
        @game.handle "playCard.north", card: @northCard.toJSON()

        expect(@game.engine.state).to.equal("waitingForCardFromEast")

      it "aborts if playing an invalid card", ->
        @game.handle "playCard.north", card: @southCard.toJSON()

        expect(@game.engine.state).to.equal("aborted")

      it "notifies the culprit of an invalid action", (done) ->
        @game.on "north.error", (error) ->
          expect(error.type).to.equal("invalidMove")
          done()

        @game.handle "playCard.north", card: @southCard.toJSON()

    describe "endingRound", ->
      it "moves on after all have checked in", ->
        @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})
        @game.engine.transition("endingRound")
        @game.handle "finishedRound.north"
        expect(@game.engine.state).to.equal("endingRound")
        @game.handle "finishedRound.east"
        expect(@game.engine.state).to.equal("endingRound")
        @game.handle "finishedRound.south"
        expect(@game.engine.state).to.equal("endingRound")
        @game.handle "finishedRound.west"
        expect(@game.engine.state).to.equal("startingRound")

    describe "endingGame", ->
      beforeEach ->
        @game.state.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})
        @game.state.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})

      it "moves on after all have checked in", ->
        @game.engine.transition("endingGame")
        @game.handle "finishedGame.north"
        expect(@game.engine.state).to.equal("endingGame")
        @game.handle "finishedGame.east"
        expect(@game.engine.state).to.equal("endingGame")
        @game.handle "finishedGame.south"
        expect(@game.engine.state).to.equal("endingGame")
        @game.handle "finishedGame.west"
        expect(@game.engine.state).to.equal("finished")

    describe "aborted", ->
      it "can be reached by a timeout", ->
        @game.engine.transition("startingRound")

        @game.handle "timeout.north"

        expect(@game.engine.state).to.equal("aborted")

      # it "reponds to everyone that the game is over", ->
      #   @game.engine.transition("aborted")

      #   @game.on "south.error", (error) ->
      #     expect(error.type).to.equal("gameEnded")
      #     done()

      #   @game.handle "something.west"
      it "moves to finished after everyone has checked in"
