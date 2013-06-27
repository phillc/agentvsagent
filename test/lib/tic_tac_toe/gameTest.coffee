Game = require "../../../lib/tic_tac_toe/game"
Player = require "../../../lib/tic_tac_toe/player"
should = require("should")

describe "Game", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @game = new Game(@player1, @player2, @player3, @player4)

  it "has an id", ->
    @game.should.have.property('id')

  it "has players", ->
    @game.players.length.should.equal(2)

  describe "#getPlayer", ->
    it "returns a player by id", ->
      @game.getPlayer(@player2.id).id.should.equal(@player2.id)

    it "returns nothing if player doesn't exist", ->
      should.not.exist(@game.getPlayer("foo"))

  describe "#start", ->
    it "assigns each player a position", ->
      @game.start()

      positions = [
        @game.positions.X.id
        @game.positions.O.id
      ]

      players = @game.players.map (player) -> player.id

      positions.sort().should.eql players.sort()

    it "emits a started event on the players", (done) ->
      @game.start()

      @player1.recvStartedGame (err, gameId) =>
        should.not.exist(err)
        gameId.should.equal(@game.id)
        done()

    it "emits a game info event on the X player", (done) ->
      @game.start()

      @game.positions.X.messages.should.have.length(2)
      @game.positions.O.messages.should.have.length(1)
      @game.positions.X.recvStartedGame (err, gameId) =>
        @game.positions.X.recvGameInfo (err, gameInfo) =>
          should.not.exist(err)
          gameInfo.should.eql({position: "X", opponentsMove: []})
          done()

    it "waits for a move from X then emits game info to Y", (done) ->
      @game.start()

      @game.positions.X.messages.should.have.length(2)
      @game.positions.O.messages.should.have.length(1)
      @game.positions.X.recvStartedGame (err, gameId) =>
        @game.positions.X.recvGameInfo (err, gameInfo) =>
          should.not.exist(err)
          gameInfo.should.eql({position: "X", opponentsMove: []})
          done()


