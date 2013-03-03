Game = require("../../../hearts/game")
Player = require("../../../hearts/player")
should = require("chai").should()

describe "Game", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @player3 = new Player()
    @player4 = new Player()
    @game = new Game(@player1, @player2, @player3, @player4)

  it "has an id", ->
    @game.should.have.property('id')

  it "has players", ->
    @game.players.length.should.equal(4)

  describe "#getPlayer", ->
    it "returns a player by id", ->
      @game.getPlayer(@player4.id).id.should.equal(@player4.id)

    it "returns nothing if player doesn't exist", ->
      should.not.exist(@game.getPlayer("foo"))




  # describe "#passCards", ->
  #   describe "on the right turn", ->
  #     beforeEach ->
  #       @game.passCards("right")

  #     it "asks each player for"

  #   describe "on the left turn", ->
  #     beforeEach ->
  #       @game.passCards("left")

  #   describe "on the across turn", ->
  #     beforeEach ->
  #       @game.passCards("across")

  #   describe "on the hold turn", ->
  #     beforeEach ->
  #       @game.passCards("hold")


