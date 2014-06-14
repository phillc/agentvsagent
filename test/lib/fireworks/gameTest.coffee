Game = require "../../../lib/fireworks/game"

describe "Game", ->
  beforeEach ->
    @game = new Game(positions: ["player1", "player2", "player3"])

  describe "#start", ->
    it "emits p1 starting", (done) ->
      @game.on "player1.starting", (data) ->
        expect(data.position).to.equal("player1")
        done()

      @game.start()

    it "emits p2 starting", (done) ->
      @game.on "player2.starting", (data) ->
        expect(data.position).to.equal("player2")
        done()

      @game.start()

    it "emits p3 starting", (done) ->
      @game.on "player3.starting", (data) ->
        expect(data.position).to.equal("player3")
        done()

      @game.start()

  describe "#waitingFor", ->
    it "emits turn for that player", (done) ->
      @game.on "player1.turn", (data) =>
        expect(data.moves).to.have.length(0)
        done()
      @game.waitingFor("player1")

  describe "states", ->
    describe "starting", ->
      it "moves to the next state once all have checked in", ->
        @game.engine.transition("starting")
        @game.handle "ready.player1"
        expect(@game.engine.state).to.equal("starting")
        @game.handle "ready.player2"
        expect(@game.engine.state).to.equal("starting")
        @game.handle "ready.player3"
        expect(@game.engine.state).to.equal("dealing")

    describe "waitingFor...", ->
      it "does something", ->
        @game.engine.transition("waitingForPlayer1")

