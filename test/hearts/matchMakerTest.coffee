MatchMaker = require '../../hearts/matchmaker'
Arena = require '../../hearts/arena'
IdGenerator = require '../../hearts/idgenerator'
Player = require '../../hearts/player'
require("chai").should()

describe "MatchMaker", ->
  describe "#findMatch", ->
    beforeEach ->
      @arena = new Arena(new IdGenerator())
      @matchMaker = new MatchMaker(@arena)

    it "does nothing if there are only three players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.matches).length.should.equal(0)
      @arena.waitingRoom.length.should.equal(3)

    it "creates a game if there are four players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.matches).length.should.equal(1)
      @arena.waitingRoom.length.should.equal(0)

    it "creates a game if there are five players", ->
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()
      @arena.createPlayer()

      @matchMaker.findMatch()

      Object.keys(@arena.matches).length.should.equal(1)
      @arena.waitingRoom.length.should.equal(1)

