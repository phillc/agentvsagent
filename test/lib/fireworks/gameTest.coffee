Game = require "../../../lib/fireworks/game"

describe "Game", ->
  beforeEach ->
    @game = new Game(positions: ["player1", "player2", "player3"])

  describe "#start", ->
    it "emits p1 starting", (done) ->
      @game.on "player1.starting", (data) ->
        expect(data.position).to.equal("player1")
        expect(data.hands["player1"]).to.not.exist
        expect(data.hands["player2"].cards).to.have.length(5)
        expect(data.hands["player3"].cards).to.have.length(5)
        done()

      @game.start()

    it "emits p2 starting", (done) ->
      @game.on "player2.starting", (data) ->
        expect(data.position).to.equal("player2")
        expect(data.hands["player1"].cards).to.have.length(5)
        expect(data.hands["player2"]).to.not.exist
        expect(data.hands["player3"].cards).to.have.length(5)
        done()

      @game.start()

    it "emits p3 starting", (done) ->
      @game.on "player3.starting", (data) ->
        expect(data.position).to.equal("player3")
        expect(data.hands["player1"].cards).to.have.length(5)
        expect(data.hands["player2"].cards).to.have.length(5)
        expect(data.hands["player3"]).to.not.exist
        done()

      @game.start()

    it "deals", ->
      @game.start()
      expect(@game.seats["player1"].cards).to.have.length(5)
      expect(@game.seats["player2"].cards).to.have.length(5)
      expect(@game.seats["player3"].cards).to.have.length(5)

    it "deals 4 cards in a 4 player game", ->
      game = new Game(positions: ["player1", "player2", "player3", "player4"])
      game.start()
      expect(game.seats["player1"].cards).to.have.length(4)
      expect(game.seats["player2"].cards).to.have.length(4)
      expect(game.seats["player3"].cards).to.have.length(4)
      expect(game.seats["player4"].cards).to.have.length(4)


  describe "#waitingFor", ->
    it "emits turn for that player", (done) ->
      @game.on "player1.turn", (data) =>
        expect(data.moves).to.have.length(0)
        done()
      @game.waitingFor("player1")

  describe "#finishMove", ->
    it "moves to player 2", ->
      @game.engine.transition("waitingForPlayer1")

      @game.finishMove()

      expect(@game.engine.state).to.equal("waitingForPlayer2")

    it.skip "ends the game when every one plays one last turn"
    it "ends the game when the deck is empty", ->
      @game.deck.cards = []
      @game.engine.transition("waitingForPlayer1")

      @game.finishMove()

      expect(@game.engine.state).to.equal("endingGame")

  describe "states", ->
    describe "starting", ->
      it "moves to the next state once all have checked in", ->
        @game.engine.transition("starting")
        @game.handle "ready.player1"
        expect(@game.engine.state).to.equal("starting")
        @game.handle "ready.player2"
        expect(@game.engine.state).to.equal("starting")
        @game.handle "ready.player3"
        expect(@game.engine.state).to.equal("waitingForPlayer1")

    describe "waitingForPlayer#", ->
      it "allows a discard", ->
        @game.engine.transition("waitingForPlayer1")

        @game.handle "move.player1", discard: 0

        expect(@game.engine.state).to.equal("waitingForPlayer2")
