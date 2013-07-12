Game = require "../../../lib/tic_tac_toe/game"
Player = require "../../../lib/tic_tac_toe/player"

describe "Game", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @game = new Game(@player1, @player2)

  it "has an id", ->
    expect(@game).to.have.property('id')

  it "has players", ->
    expect(@game.players).to.have.length(2)

  it "assigns each player a position", ->
    positions = [
      @game.positions.X.id
      @game.positions.O.id
    ]

    players = @game.players.map (player) -> player.id

    expect(positions.sort()).to.eql(players.sort())

  it "listens for player messages and handles them", (done) ->
    @game.engine["*"] = (message, data) ->
      expect(message).to.equal("foo.X")
      expect(data).to.equal("bar")
      done()

    @game.positions.X.forward 'foo', 'bar'

  describe "#getPlayer", ->
    it "returns a player by id", ->
      expect(@game.getPlayer(@player2.id).id).to.equal(@player2.id)

    it "returns nothing if player doesn't exist", ->
      expect(@game.getPlayer("foo")).to.not.exist

  describe "states", ->
    describe "initialized", ->
      it "is the starting state", ->
        expect(@game.engine.state).to.equal("initialized")

      it "starts", ->
        @game.start()
        expect(@game.engine.state).to.equal("started")

    describe "started", ->
      it "emits X started", (done) ->
        @game.positions.X.forward().then (value) =>
          expect(value.message).to.equal("started")
          expect(value.data.game).to.equal(@game)
          expect(value.data.player).to.equal(@game.positions.X)
          done()
        @game.engine.transition("started")

      it "emits O started", (done) ->
        @game.positions.O.forward().then (value) =>
          expect(value.message).to.equal("started")
          expect(value.data.game).to.equal(@game)
          expect(value.data.player).to.equal(@game.positions.O)
          done()
        @game.engine.transition("started")

      it "moves to the next state when both players have readied", ->
        @game.engine.transition("started")
        @game.engine.handle("ready.X")
        expect(@game.engine.state).to.equal("started")
        @game.engine.handle("ready.O")
        expect(@game.engine.state).to.equal("waitingForX")

    describe "waitingForX", ->
      it "emits X turn", (done) ->
        @game.positions.X.forward().then (value) ->
          expect(value.message).to.equal("turn")
          expect(value.data.position).to.equal("X")
          done()
        @game.engine.transition("waitingForX")

      it "moves to waitingForO on move by X", ->
        @game.engine.transition("waitingForX")

        @game.engine.handle("move.X", 0, 0, 0, 0)
        expect(@game.engine.state).to.equal("waitingForO")

      it "makes the move", ->
        @game.engine.transition("waitingForX")

        @game.engine.handle("move.X", 1, 2, 2, 0)

        expect(@game.squareAt(1, 2, 2, 0).winner).to.equal("X")

      it "moves to game ended if won", ->
        @game.engine.transition("waitingForX")

        @game.winner = -> "X"
        @game.engine.handle("move.X", 1, 2, 2, 0)
        expect(@game.engine.state).to.equal("gameEnded")

    describe "waitingForO", ->
      it "emits O turn", (done) ->
        @game.positions.O.forward().then (value) ->
          expect(value.message).to.equal("turn")
          expect(value.data.position).to.equal("O")
          done()
        @game.engine.transition("waitingForO")

      it "moves to waitingForX on move by O", ->
        @game.engine.transition("waitingForO")

        @game.engine.handle("move.O")
        expect(@game.engine.state).to.equal("waitingForX")
