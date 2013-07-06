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

  describe "#getPlayer", ->
    it "returns a player by id", ->
      expect(@game.getPlayer(@player2.id).id).to.equal(@player2.id)

    it "returns nothing if player doesn't exist", ->
      expect(@game.getPlayer("foo")).to.not.exist

  describe "states", ->
    describe "initialized", ->
      it "is the starting state", ->
        expect(@game.state).to.equal("initialized")

      it "starts", ->
        @game.start()
        expect(@game.state).to.equal("started")

    describe "started", ->
      it "emits X started", (done) ->
        @game.on "X.started", ->
          done()
        @game.transition("started")

      it "emits Y started", (done) ->
        @game.on "Y.started", ->
          done()
        @game.transition("started")

      it "moves to the next state when both players have readied", ->
        @game.transition("started")
        @game.handle("ready.X")
        expect(@game.state).to.equal("started")
        @game.handle("ready.Y")
        expect(@game.state).to.equal("waitingForX")

    describe "waitingForX", ->
      it "emits X turn", (done) ->
        @game.on "X.turn", ->
          done()
        @game.transition("waitingForX")

      it "moves to waitingForY on move by X", ->
        @game.transition("waitingForX")

        @game.handle("move.X")
        expect(@game.state).to.equal("waitingForY")


    describe "waitingForY", ->
      it "emits Y turn", (done) ->
        @game.on "Y.turn", ->
          done()
        @game.transition("waitingForY")

      it "moves to waitingForX on move by Y", ->
        @game.transition("waitingForY")

        @game.handle("move.Y")
        expect(@game.state).to.equal("waitingForX")
