MatchMaker = require '../../hearts/matchmaker'
Arena = require '../../hearts/arena'
Player = require '../../hearts/player'
require("should")

describe "MatchMaker", ->
  beforeEach ->
    @arena = new Arena()
    @matchMaker = new MatchMaker(@arena)

  describe "#findMatch", ->
    it "does nothing if there are only three players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.runningMatches).length.should.equal(0)
      @arena.waitingRoom.length.should.equal(3)

    it "creates a game if there are four players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.runningMatches).length.should.equal(1)
      @arena.waitingRoom.length.should.equal(0)

    it "creates a game if there are five players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.runningMatches).length.should.equal(1)
      @arena.waitingRoom.length.should.equal(1)

  describe "#start", ->
    it "automatically finds matches as players join", ->
      Object.keys(@arena.runningMatches).length.should.equal(0)
      @matchMaker.start()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      Object.keys(@arena.runningMatches).length.should.equal(1)


