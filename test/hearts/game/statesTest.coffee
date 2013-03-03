Game = require("../../../hearts/game")
Pile = require("../../../hearts/game/pile")
Card = require("../../../hearts/game/card")
Player = require("../../../hearts/player")
states = require("../../../hearts/game/states")
actions = require("../../../hearts/game/actions")
require("should")

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

    it "assigns each player a position", ->
      @state.run()
      console.log "GAME:   ", @game

      positions = [
        @game.northPlayer.id
        @game.eastPlayer.id
        @game.southPlayer.id
        @game.westPlayer.id
      ]

      players = @game.players.map (player) -> player.id

      positions.sort().should.eql players.sort()

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
      @game.stack[0].should.equal("startingTrick")

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Dealing", ->
    beforeEach ->
      new states.StartingRound(@game).run()
      @nextStateCalls = 0
      @state = new states.Dealing(@game)

    it "deals cards to all players", ->
      @state.run()
      @game.currentRound.north.dealt.cards.should.have.length(13)
      @game.currentRound.east.dealt.cards.should.have.length(13)
      @game.currentRound.south.dealt.cards.should.have.length(13)
      @game.currentRound.west.dealt.cards.should.have.length(13)

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Passing", ->
    beforeEach ->
      new states.StartingRound(@game).run()
      @nextStateCalls = 0
      @state = new states.Passing(@game, "left")

    it "get cards from players", ->
      cards = Card.all()[0..2]
      action = new actions.PassCards(@game.northPlayer, cards)

      @state.handleAction(action)

      @game.currentRound.north.passed.cards.should.eql(cards)

    it "goes to the next state after all four have passed cards", ->
      cards = Card.all()[0..2]
      @state.handleAction new actions.PassCards(@game.northPlayer, cards)
      @state.handleAction new actions.PassCards(@game.eastPlayer, cards)
      @state.handleAction new actions.PassCards(@game.southPlayer, cards)
      @state.handleAction new actions.PassCards(@game.westPlayer, cards)
      @nextStateCalls.should.equal(1)

    it "does not go to the next state if the same player passes four times", ->
      cards = Card.all()[0..2]
      @state.handleAction new actions.PassCards(@game.northPlayer, cards)
      @state.handleAction new actions.PassCards(@game.northPlayer, cards)
      @state.handleAction new actions.PassCards(@game.northPlayer, cards)
      @state.handleAction new actions.PassCards(@game.northPlayer, cards)
      @nextStateCalls.should.equal(0)
