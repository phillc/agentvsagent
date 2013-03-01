Arena = require '../../hearts/arena'
require("chai").should()

describe "Arena", ->
  beforeEach ->
    @arena = new Arena()

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

  describe "#createMatch", ->
    beforeEach ->
      for _ in [1..10]
        @arena.createPlayer()
      @arena.waitingRoom.length.should.equal(10)
      Object.keys(@arena.runningMatches).length.should.equal(0)
      @matchedPlayers = @arena.waitingRoom[2..5]

    it "removes the players from the waiting room", ->
      @arena.createMatch @matchedPlayers
      @arena.waitingRoom.length.should.equal(6)

    it "creates a match out of the players", ->
      @arena.createMatch @matchedPlayers
      Object.keys(@arena.runningMatches).length.should.equal(1)

    it "returns the match id", ->
      returnValue = @arena.createMatch @matchedPlayers
      returnValue.should.equal(Object.keys(@arena.runningMatches)[0])

