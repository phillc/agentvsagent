Game = require("../../../hearts/game")
Player = require("../../../hearts/player")
states = require("../../../hearts/game/states")
require("chai").should()

describe "states", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @player3 = new Player()
    @player4 = new Player()
    @game = new Game(@player1, @player2, @player3, @player4)

    @nextStateCalls = 0
    @game.nextState = =>
      @nextStateCalls++


  describe "StartingGame", ->
    beforeEach ->
      @state = new states.StartingGame(@game)

    it "emits a start game event on the players", (done) ->
      @player1.once 'start', (gameId) =>
        gameId.should.equal(@game.id)
        done()

      @state.run()

    it "pushes the next states on the stack", ->
      @game.stack.should.have.length(0)
      @state.run()
      @game.stack.should.have.length(2)
      @game.stack[1].should.equal("startingRound")
      @game.stack[0].should.equal("endingGame")

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "StartingRound", ->
    beforeEach ->
      @state = new states.StartingRound(@game)

    it "pushes the next states on the stack", ->
      @game.stack.should.have.length(0)
      @state.run()
      @game.stack.should.have.length(3)
      @game.stack[2].should.equal("dealing")
      @game.stack[1].should.equal("passing")
      @game.stack[0].should.equal("startTrick")

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Dealing", ->
    beforeEach ->
      @state = new states.Dealing(@game)

    it "deals cards to all players", ->
      @state.run()
      @game.players[0].held.cards.should.have.length(13)
      @game.players[1].held.cards.should.have.length(13)
      @game.players[2].held.cards.should.have.length(13)
      @game.players[3].held.cards.should.have.length(13)

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Passing", ->
    beforeEach ->
      @state = new states.Passing(@game)

    it "get cards from all players", ->
      @state.run()

    it "goes to the next state after all four have passed cards", ->
      @state.run()
      @nextStateCalls.should.equal(1)
