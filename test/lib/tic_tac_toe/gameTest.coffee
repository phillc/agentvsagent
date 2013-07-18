Game = require "../../../lib/tic_tac_toe/game"

describe "Game", ->
  beforeEach ->
    @game = new Game()

  describe "states", ->
    describe "initialized", ->
      it "is the starting state", ->
        expect(@game.engine.state).to.equal("initialized")

      it "starts with X", ->
        @game.start()
        expect(@game.engine.state).to.equal("waitingForX")

    describe "waitingForX", ->
      it "emits X turn", (done) ->
        @game.once "X.turn", (data) ->
          expect(data.position).to.equal("X")
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
        @game.once "O.turn", (data) ->
          expect(data.position).to.equal("O")
          done()
        @game.engine.transition("waitingForO")

      it "moves to waitingForX on move by O", ->
        @game.engine.transition("waitingForO")

        @game.engine.handle("move.O", 0, 0, 0, 0)
        expect(@game.engine.state).to.equal("waitingForX")
