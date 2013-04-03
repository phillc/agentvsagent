Factory = require './factory'
Arena = require '../lib/arena'
HeartsFactory = require '../lib/hearts/factory'
should = require("should")

describe "Arena", ->
  beforeEach ->
    factory = new HeartsFactory()
    @arena = new Arena(factory)

  describe "#createPlayer", ->
    it "adds players", ->
      @arena.waitingRoom.length.should.equal(0)
      @arena.createPlayer()
      @arena.waitingRoom.length.should.equal(1)

    it "emits an event", (done) ->
      @arena.on 'newPlayer', done
      @arena.createPlayer()

  describe "#removePlayer", ->
    beforeEach ->
      @player1 = @arena.createPlayer()
      @player2 = @arena.createPlayer()
      @player3 = @arena.createPlayer()
      @player4 = @arena.waitingRoom.length.should.equal(3)

    it "removes players", ->
      @arena.removePlayer @player2
      @arena.waitingRoom.length.should.equal(2)
      @arena.waitingRoom[0].should.equal(@player1)
      @arena.waitingRoom[1].should.equal(@player3)

  describe "#createGame", ->
    beforeEach ->
      for _ in [1..10]
        @arena.createPlayer()
      @arena.waitingRoom.length.should.equal(10)
      Object.keys(@arena.runningMatches).length.should.equal(0)
      @matchedPlayers = @arena.waitingRoom[2..5]

    it "removes the players from the waiting room", ->
      @arena.createGame @matchedPlayers
      @arena.waitingRoom.length.should.equal(6)

    it "creates a game out of the players", ->
      @arena.createGame @matchedPlayers
      Object.keys(@arena.runningMatches).length.should.equal(1)

    it "returns the game", ->
      game = @arena.createGame @matchedPlayers
      game.id.should.equal(Object.keys(@arena.runningMatches)[0])

  describe "#getGame", ->
    it "returns the game", ->
      gameId = Factory.createGame(arena: @arena).id
      game = @arena.getGame gameId
      game.id.should.equal(gameId)

    it "returns nothing if there is no game", ->
      should.not.exist(@arena.getGame("foo"))

