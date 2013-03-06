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

      positions = [
        @game.positions.north.id
        @game.positions.east.id
        @game.positions.south.id
        @game.positions.west.id
      ]

      players = @game.players.map (player) -> player.id

      positions.sort().should.eql players.sort()

    it "emits a started event on the players", (done) ->
      @player1.once 'started', (gameId) =>
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
      @game.stack.should.have.length(15)
      @game.stack[14].should.equal("dealing")
      @game.stack[13].should.equal("passingRight")
      @game.stack[12].should.equal("startingTrick")
      @game.stack[0].should.equal("startingTrick")

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Dealing", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @nextStateCalls = 0
      @state = new states.Dealing(@game)

    it "deals cards to all players", ->
      @state.run()
      @game.currentRound().north.dealt.cards.should.have.length(13)
      @game.currentRound().east.dealt.cards.should.have.length(13)
      @game.currentRound().south.dealt.cards.should.have.length(13)
      @game.currentRound().west.dealt.cards.should.have.length(13)

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

    it "emits a dealt event on the players", (done) ->
      @player1.once 'dealt', (cards) =>
        cards.should.have.length(13)
        done()

      @state.run()

  describe "Passing", ->
    beforeEach ->
      new states.StartingGame(@game).run()
      new states.StartingRound(@game).run()
      @nextStateCalls = 0
      @state = new states.Passing(@game, "left")

    it "get cards from players", ->
      cards = Card.all()[0..2]
      action = new actions.PassCards(@game.positions.north, cards)

      @state.handleAction(action)

      @game.currentRound().north.passed.cards.should.eql(cards)

    it "goes to the next state and emits an event after all four have passed cards", (done) ->
      cards = Card.all()[0..2]
      @state.handleAction new actions.PassCards(@game.positions.north, cards)
      @state.handleAction new actions.PassCards(@game.positions.east, cards)
      @state.handleAction new actions.PassCards(@game.positions.south, cards)
      @game.positions.north.once 'passed', (cards) ->
        cards.should.have.length(3)
        done()

      @state.handleAction new actions.PassCards(@game.positions.west, cards)
      @nextStateCalls.should.equal(1)

    it "does not go to the next state if the same player passes four times", ->
      cards = Card.all()[0..2]
      @state.handleAction new actions.PassCards(@game.positions.north, cards)
      @state.handleAction new actions.PassCards(@game.positions.north, cards)
      @state.handleAction new actions.PassCards(@game.positions.north, cards)
      @state.handleAction new actions.PassCards(@game.positions.north, cards)
      @nextStateCalls.should.equal(0)

  describe "StartingTrick", ->
    beforeEach ->
      new states.StartingGame(@game).run()
      new states.StartingRound(@game).run()
      @nextStateCalls = 0

    it "adds a trick to the round", ->
      @game.currentRound().tricks.should.have.length(0)
      state = new states.StartingTrick(@game).run()
      @game.currentRound().tricks.should.have.length(1)
      state = new states.StartingTrick(@game).run()
      @game.currentRound().tricks.should.have.length(2)

    it "adds the next states", ->
      @game.stack.splice(0, @game.stack.length)
      @game.stack.should.have.length(0)
      state = new states.StartingTrick(@game).run()
      @game.stack.should.have.length(5)
      @game.stack[4].should.equal("waitingForCardFromEast")
      @game.stack[3].should.equal("waitingForCardFromSouth")
      @game.stack[2].should.equal("waitingForCardFromWest")
      @game.stack[1].should.equal("waitingForCardFromNorth")
      @game.stack[0].should.equal("endingTrick")

    it "goes to the next state", ->
      new states.StartingTrick(@game).run()

      @nextStateCalls.should.equal(1)

  describe "WaitingForCard", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @game.states.startingTrick.run()
      @nextStateCalls = 0

    it "applies the card to the player", ->
      state = new states.WaitingForCard(@game, "north")
      card = Card.all()[0]
      action = new actions.PlayCard(@game.positions.north, card)
      state.handleAction(action)

      @game.currentRound().currentTrick().played.cards[0].should.equal(card)

    it "emits an event on the player with the current trick", (done) ->
      @game.positions.north.once 'turn', (trick) =>
        trick.should.equal @game.currentRound().currentTrick()
        done()
      state = new states.WaitingForCard(@game, "north").run()


    it "goes to the next state", ->
      state = new states.WaitingForCard(@game, "north")
      card = Card.all()[0]
      action = new actions.PlayCard(@game.positions.north, card)
      state.handleAction(action)

      @nextStateCalls.should.equal(1)

  describe "EndingTrick", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @game.states.startingTrick.run()
      @nextStateCalls = 0

    it "emits end trick event on each player", (done) ->
      @game.positions.north.once 'endTrick', (trick) =>
        trick.should.equal @game.currentRound().currentTrick()
        done()

      new states.EndingTrick(@game).run()


    it "goes to the next state", ->
      new states.EndingTrick(@game).run()
      @nextStateCalls.should.equal(1)
