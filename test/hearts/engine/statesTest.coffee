Game = require "../../../lib/hearts/engine/game"
Pile = require "../../../lib/hearts/engine/pile"
Card = require "../../../lib/hearts/engine/card"
Suit = require "../../../lib/hearts/engine/suit"
Rank = require "../../../lib/hearts/engine/rank"
Player = require "../../../lib/hearts/player"
states = require "../../../lib/hearts/engine/states"
actions = require "../../../lib/hearts/engine/actions"
should = require("should")

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
      @player1.recvStartedGame (err, gameId) =>
        gameId.should.equal(@game.id)
        done()

      @state.run()

    it "pushes the next states on the stack", ->
      @game.stack.should.have.length(0)
      @state.run()
      @game.stack.should.have.length(1)
      @game.stack[0].should.equal("startingRound")

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "StartingRound", ->
    beforeEach ->
      @state = new states.StartingRound(@game)

    it "pushes the next states on the stack", ->
      @game.stack.should.have.length(0)
      @state.run()
      @game.stack.should.have.length(16)
      @game.stack[15].should.equal("dealing")
      @game.stack[14].should.equal("passingLeft")
      @game.stack[13].should.equal("startingTrick")
      @game.stack[1].should.equal("startingTrick")
      @game.stack[0].should.equal("endingRound")

    it "passes in each direction", ->
      @state.run()
      @game.stack[14].should.equal("passingLeft")
      @game.stack.splice(0, 20)
      @state.run()
      @game.stack[14].should.equal("passingRight")
      @game.stack.splice(0, 20)
      @state.run()
      @game.stack[14].should.equal("passingAcross")
      @game.stack.splice(0, 20)
      @state.run()
      @game.stack[14].should.equal("dealing")
      @game.stack.splice(0, 20)
      @state.run()
      @game.stack[14].should.equal("passingLeft")
      @game.stack.splice(0, 20)

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

  describe "Dealing", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.positions.north.messages.splice(0, 10)
      @game.positions.east.messages.splice(0, 10)
      @game.positions.south.messages.splice(0, 10)
      @game.positions.west.messages.splice(0, 10)
      @game.states.startingRound.run()
      @nextStateCalls = 0
      @state = new states.Dealing(@game)

    it "deals cards to all players", ->
      @state.run()
      @game.currentRound().north.dealt.cards.should.have.length(13)
      @game.currentRound().north.held.cards.should.have.length(13)
      @game.currentRound().east.dealt.cards.should.have.length(13)
      @game.currentRound().east.held.cards.should.have.length(13)
      @game.currentRound().south.dealt.cards.should.have.length(13)
      @game.currentRound().south.held.cards.should.have.length(13)
      @game.currentRound().west.dealt.cards.should.have.length(13)
      @game.currentRound().west.held.cards.should.have.length(13)

    it "goes to the next state", ->
      @state.run()
      @nextStateCalls.should.equal(1)

    it "emits a dealt event on the players", (done) ->
      @player1.recvDealt (err, cards) =>
        cards.should.have.length(13)
        done()

      @state.run()

  describe "Passing", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @game.states.dealing.run()
      @northPassedCards = @game.currentRound().north.dealt.cards[0..2]
      @eastPassedCards = @game.currentRound().east.dealt.cards[0..2]
      @southPassedCards = @game.currentRound().south.dealt.cards[0..2]
      @westPassedCards = @game.currentRound().west.dealt.cards[0..2]
      @game.positions.north.messages.splice(0, 10)
      @nextStateCalls = 0
      @game.currentState = @state = new states.Passing(@game, "left")

    it "get cards from players", ->
      action = new actions.PassCards(@game.positions.north, @northPassedCards)

      @state.handleAction(action)

      @game.currentRound().north.passed.cards.should.eql(@northPassedCards)

    it "goes to the next state and emits an event after all four have passed cards", (done) ->
      @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
      @state.handleAction new actions.PassCards(@game.positions.east, @eastPassedCards)
      @state.handleAction new actions.PassCards(@game.positions.south, @southPassedCards)
      @game.positions.north.recvPassed (err, cards) ->
        should.not.exist(err)
        cards.should.have.length(3)
        done()

      @state.handleAction new actions.PassCards(@game.positions.west, @westPassedCards)
      @nextStateCalls.should.equal(1)

    it "does not go to the next state if the same player passes four times", ->
      @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
      @nextStateCalls.should.equal(0)

      @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
      @game.stack[0].should.equal("gameEnded")
      @nextStateCalls.should.equal(1)

      @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
      @game.stack[0].should.equal("gameEnded")
      @nextStateCalls.should.equal(2)

      @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
      @game.stack[0].should.equal("gameEnded")
      @nextStateCalls.should.equal(3)

    describe "strategies", ->
      setup = ->
        @state.handleAction new actions.PassCards(@game.positions.north, @northPassedCards)
        @state.handleAction new actions.PassCards(@game.positions.east, @eastPassedCards)
        @state.handleAction new actions.PassCards(@game.positions.south, @southPassedCards)
        @state.handleAction new actions.PassCards(@game.positions.west, @westPassedCards)

        @nextStateCalls.should.equal(1)

      it "passes left", ->
        @state.direction = "left"
        setup.apply(this)

        for card in @northPassedCards
          should.not.exist(@game.currentRound().north.held.findCard(card.suit, card.rank))
          @game.currentRound().north.passed.findCard(card.suit, card.rank).should.equal(card)
          @game.currentRound().east.held.findCard(card.suit, card.rank).should.equal(card)

      it "passes right", ->
        @state.direction = "right"
        setup.apply(this)

        for card in @northPassedCards
          should.not.exist(@game.currentRound().north.held.findCard(card.suit, card.rank))
          @game.currentRound().north.passed.findCard(card.suit, card.rank).should.equal(card)
          @game.currentRound().west.held.findCard(card.suit, card.rank).should.equal(card)

      it "passes across", ->
        @state.direction = "across"
        setup.apply(this)

        for card in @northPassedCards
          should.not.exist(@game.currentRound().north.held.findCard(card.suit, card.rank))
          @game.currentRound().north.passed.findCard(card.suit, card.rank).should.equal(card)
          @game.currentRound().south.held.findCard(card.suit, card.rank).should.equal(card)

    it "responds to other actions with an out of sequence error", (done) ->
      action = new actions.PlayCard(@game.positions.west, null)
      @state.handleAction(action)
      @game.positions.west.recvPassed (err, gameId) ->
        err.type.should.equal("outOfSequence")
        err.message.should.equal("Action requested out of sequence.")
        should.not.exist(gameId)
        done()

  describe "StartingTrick", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()

      @game.currentRound().east.held.addCard(new Card(Suit.CLUBS, Rank.TWO))

      @nextStateCalls = 0

    it "adds a trick to the round", ->
      @game.currentRound().tricks.should.have.length(0)
      @game.states.startingTrick.run()
      @game.currentRound().tricks.should.have.length(1)
      @game.currentRound().currentTrick().played.addCard({})
      @game.states.startingTrick.run()
      @game.currentRound().tricks.should.have.length(2)

    it "adds the next states", ->
      @game.stack.splice(0, @game.stack.length)
      @game.stack.should.have.length(0)
      @game.states.startingTrick.run()
      @game.stack.should.have.length(5)
      @game.stack[4].should.equal("waitingForCardFromEast")
      @game.stack[3].should.equal("waitingForCardFromSouth")
      @game.stack[2].should.equal("waitingForCardFromWest")
      @game.stack[1].should.equal("waitingForCardFromNorth")
      @game.stack[0].should.equal("endingTrick")

    it "waits for cards starting with the last winner south", ->
      @game.states.startingTrick.run()
      @game.currentRound().currentTrick().winner = -> "south"
      @game.stack.splice(0, @game.stack.length)
      @game.stack.should.have.length(0)
      @game.states.startingTrick.run()
      @game.stack.should.have.length(5)
      @game.stack[4].should.equal("waitingForCardFromSouth")
      @game.stack[3].should.equal("waitingForCardFromWest")
      @game.stack[2].should.equal("waitingForCardFromNorth")
      @game.stack[1].should.equal("waitingForCardFromEast")
      @game.stack[0].should.equal("endingTrick")

    it "waits for cards starting with the last winner north", ->
      @game.states.startingTrick.run()
      @game.currentRound().currentTrick().winner = -> "north"
      @game.stack.splice(0, @game.stack.length)
      @game.stack.should.have.length(0)
      @game.states.startingTrick.run()
      @game.stack.should.have.length(5)
      @game.stack[4].should.equal("waitingForCardFromNorth")
      @game.stack[3].should.equal("waitingForCardFromEast")
      @game.stack[2].should.equal("waitingForCardFromSouth")
      @game.stack[1].should.equal("waitingForCardFromWest")
      @game.stack[0].should.equal("endingTrick")

    it "goes to the next state", ->
      @game.states.startingTrick.run()

      @nextStateCalls.should.equal(1)

  describe "WaitingForCard", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @game.states.dealing.run()
      @game.positions.north.messages.splice(0, 10)
      @game.states.startingTrick.run()
      @game.currentState = @game.states.waitingForCardFromNorth
      @nextStateCalls = 0
      @card = new Card(Suit.CLUBS, Rank.TWO)
      @game.currentRound().north.held.cards = [@card]
      @game.turnTime = 0

    it "applies the card to the player", ->
      @game.currentState.run()
      action = new actions.PlayCard(@game.positions.north, @card)
      @game.currentState.handleAction(action)

      @game.currentRound().currentTrick().played.cards[0].should.equal(@card)

    it "emits an event on the player with the current trick", (done) ->
      @game.positions.north.recvTurn (err, trick) =>
        trick.should.equal @game.currentRound().currentTrick()
        done()
      @game.currentState.run()

    it "goes to the next state", ->
      action = new actions.PlayCard(@game.positions.north, @card)
      @nextStateCalls.should.equal(0)
      @game.currentState.handleAction(action)

      @nextStateCalls.should.equal(1)

    it "aborts if the player takes longer than the allowed time", (done) ->
      @game.turnTime = 50
      @game.currentState.run()

      @nextStateCalls.should.equal(0)
      setTimeout =>
        @nextStateCalls.should.equal(1)
        @game.stack[0].should.equal("gameEnded")
        @game.currentState = @game.states.gameEnded

        action = new actions.PlayCard(@game.positions.north, @card)
        @game.currentState.handleAction(action)
        @nextStateCalls.should.equal(1)

        # test the message
        @game.positions.north.recvTurn (err, trick) =>
          err.type.should.equal("invalidMove")
          err.message.should.equal("Your action took longer than allowed")
          done()
      , 75

    it "applies the card if the player takes less than the allowed time", (done) ->
      @game.turnTime = 200
      @game.currentState.run()

      setTimeout =>
        @nextStateCalls.should.equal(0)
        action = new actions.PlayCard(@game.positions.north, @card)
        @game.currentState.handleAction(action)
        @game.currentRound().currentTrick().played.cards[0].should.equal(@card)
        @nextStateCalls.should.equal(1)
        done()
      , 75

  describe "EndingTrick", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.states.startingRound.run()
      @game.positions.north.messages.splice(0, 10)
      @game.states.startingTrick.run()
      @nextStateCalls = 0

    it "emits end trick event on each player", (done) ->
      @game.positions.north.recvEndTrick (err, trick) =>
        trick.should.equal @game.currentRound().currentTrick()
        done()

      new states.EndingTrick(@game).run()


    it "goes to the next state", ->
      new states.EndingTrick(@game).run()
      @nextStateCalls.should.equal(1)

  describe "EndingRound", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.positions.north.messages.splice(0, 10)
      @game.stack = []
      @nextStateCalls = 0

    it "starts a new round if no one is over 100, and emits new round on each player", (done) ->
      @game.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})

      @game.positions.north.recvEndRound (err, round, status) ->
        status.should.equal 'nextRound'
        round.north.should.equal(10)
        round.east.should.equal(0)
        round.south.should.equal(15)
        round.west.should.equal(1)
        done()

      @game.states.endingRound.run()
      @game.stack[0].should.equal("startingRound")
      @nextStateCalls.should.equal(1)

    it "ends the game if someone reaches 100, and emits game end on each player", (done) ->
      @game.rounds.push({ scores: -> { north: 101, east: 0, south: 15, west: 1 }})

      @game.positions.north.recvEndRound (err, round, status) ->
        status.should.equal 'endGame'
        round.north.should.equal(101)
        round.east.should.equal(0)
        round.south.should.equal(15)
        round.west.should.equal(1)
        done()

      @game.states.endingRound.run()
      @game.stack.should.have.length(1)
      @game.stack[0].should.equal("endingGame")
      @nextStateCalls.should.equal(1)

  describe "EndingGame", ->
    beforeEach ->
      @game.states.startingGame.run()
      @game.positions.north.messages.splice(0, 10)
      @nextStateCalls = 0
      @game.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})
      @game.rounds.push({ scores: -> { north: 50, east: 0, south: 15, west: 1 }})

    it "send the game scores to each player", (done) ->
      @game.positions.north.recvEndGame (err, game) ->
        game.north.should.equal(100)
        game.east.should.equal(0)
        game.south.should.equal(30)
        game.west.should.equal(2)
        done()

      @game.states.endingGame.run()

    it "goes to the next state", ->
      @game.states.endingGame.run()
      @nextStateCalls.should.equal(1)

  describe "GameEnded", ->
    beforeEach ->
      @game.states.startingGame.run()
      @nextStateCalls = 0

    it "doesn't call next state", ->
      @game.states.gameEnded.run()
      @nextStateCalls.should.equal(0)

    it "emits a gameEnded event", (done) ->
      @game.once "gameEnded", ->
        done()
      @game.states.gameEnded.run()

