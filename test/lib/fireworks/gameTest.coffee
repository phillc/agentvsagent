Game = require "../../../lib/fireworks/game"

describe "Game", ->
  beforeEach ->
    @game = new Game(positions: ["player1", "player2"])

  describe "#start", ->
    it "emits p1 starting", (done) ->
      @game.on "player1.started", (data) ->
        expect(data.position).to.equal("player1")
        done()

      @game.start()

    it "emits p2 starting", (done) ->
      @game.on "player2.started", (data) ->
        expect(data.position).to.equal("player2")
        done()

      @game.start()

