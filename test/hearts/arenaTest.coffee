Arena = require '../../hearts/arena'
IdGenerator = require '../../hearts/idgenerator'
require("chai").should()

describe "Arena", ->
  beforeEach ->
    @idGenerator = new IdGenerator()
    @arena = new Arena(@idGenerator)

  describe "#createPlayer", ->
    it "adds players", ->
      @arena.waitingRoom.length.should.equal(0)
      @arena.createPlayer()
      @arena.waitingRoom.length.should.equal(1)

    it "emits an event", (done) ->
      @arena.on 'newPlayer', done
      @arena.createPlayer()

    it "returns a player with an id", ->
      @idGenerator.generate = -> "123"
      @arena.createPlayer().id.should.equal("123")

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
      Object.keys(@arena.matches).length.should.equal(0)
      @matchedPlayers = @arena.waitingRoom[2..5]

    it "removes the players from the waiting room", ->
      @returnValue = @arena.createMatch @matchedPlayers
      @arena.waitingRoom.length.should.equal(6)

    it "creates a match out of the players", ->
      @returnValue = @arena.createMatch @matchedPlayers
      Object.keys(@arena.matches).length.should.equal(1)

    it "returns the match id", ->
      @idGenerator.generate = -> "567"
      @returnValue = @arena.createMatch @matchedPlayers
      @returnValue.should.equal("567")

