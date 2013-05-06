Factory = require "../../factory"
actions = require "../../../lib/hearts/engine/actions"
Card = require "../../../lib/hearts/engine/card"
Pile = require "../../../lib/hearts/engine/pile"
should = require("should")

describe "actions", ->
  beforeEach ->
    @game = Factory.createGame()
    @nextStateCalls = 0
    @game.nextState = =>
      @nextStateCalls++

  describe "PassCards", ->
    beforeEach ->
      @game.states.startingRound.run()
      @game.states.dealing.run()
      @game.currentState = @game.states.passingRight
      @game.positions.north.messages.splice(0, 10)
      @game.positions.east.messages.splice(0, 10)
      @game.positions.south.messages.splice(0, 10)
      @game.positions.west.messages.splice(0, 10)

    describe "#execute", ->
      it "applies and emits the passed cards for the player", ->
        northCards = @game.currentRound().north.dealt.cards[1..3]
        eastCards = @game.currentRound().east.dealt.cards[1..3]
        southCards = @game.currentRound().south.dealt.cards[1..3]
        westCards = @game.currentRound().west.dealt.cards[1..3]
        northAction = new actions.PassCards(@game.positions.north, northCards)
        eastAction = new actions.PassCards(@game.positions.east, eastCards)
        southAction = new actions.PassCards(@game.positions.south, southCards)
        westAction = new actions.PassCards(@game.positions.west, westCards)

        northAction.execute(@game)
        eastAction.execute(@game)
        southAction.execute(@game)
        westAction.execute(@game)

        @game.currentRound().north.passed.cards.should.eql(northCards)
        @game.currentRound().east.passed.cards.should.eql(eastCards)
        @game.currentRound().south.passed.cards.should.eql(southCards)
        @game.currentRound().west.passed.cards.should.eql(westCards)

        @game.positions.north.recvPassed (err, passed) ->
          should.not.exist(err)
          passed.should.equal(southCards)
          done()

    describe "#validate", ->
      it "returns null for valid actions", ->
        cards = @game.currentRound().north.dealt.cards[1..3]
        action = new actions.PassCards(@game.positions.north, cards)

        should.not.exist(action.validate(@game))

      it "returns an error if less than three cards", ->
        cards = @game.currentRound().north.dealt.cards[1..2]
        action = new actions.PassCards(@game.positions.north, cards)

        error = action.validate(@game)
        error.type.should.equal "invalidMove"
        error.message.should.equal "Must pass three cards. You passed 2."

      it "returns an error if more than three cards", ->
        cards = @game.currentRound().north.dealt.cards[1..4]
        action = new actions.PassCards(@game.positions.north, cards)

        error = action.validate(@game)
        error.type.should.equal "invalidMove"
        error.message.should.equal "Must pass three cards. You passed 4."

      it "returns an error if passing someone else's card", ->
        cards = @game.currentRound().south.dealt.cards[1..3]
        action = new actions.PassCards(@game.positions.north, cards)

        error = action.validate(@game)
        error.type.should.equal "invalidMove"
        error.message.should.equal "Must pass cards in your hand."

      it "returns an error if passing the same card multiple times", ->
        twoCards = @game.currentRound().south.dealt.cards[1..2]
        cards = twoCards.concat(twoCards[0])
        action = new actions.PassCards(@game.positions.south, cards)

        error = action.validate(@game)
        error.type.should.equal "invalidMove"
        error.message.should.equal "Must pass a card no more than once."

      it "returns an error when in wrong state", ->
        @game.currentState = @game.states.endingGame

        cards = @game.currentRound().north.dealt.cards[1..3]
        action = new actions.PassCards(@game.positions.north, cards)

        error = action.validate(@game)
        error.type.should.equal("outOfSequence")
        error.message.should.equal("Action requested out of sequence.")

      it "returns an error if passing multiple times", ->
        cards = @game.currentRound().north.dealt.cards[1..3]
        action = new actions.PassCards(@game.positions.north, cards)
        action.execute(@game)

        action = new actions.PassCards(@game.positions.north, cards)

        error = action.validate(@game)
        error.type.should.equal("invalidMove")
        error.message.should.equal("May not pass more than once in a round.")

  describe "PlayCard", ->
    beforeEach ->
      @game.states.startingRound.run()
      @game.states.dealing.run()
      @game.states.startingTrick.run()
      @game.currentState = @game.states.waitingForCardFromNorth

    describe "#execute", ->
      it "applies the card", ->
        card = @game.currentRound().north.dealt.cards[0]

        action = new actions.PlayCard(@game.positions.north, card)
        action.execute(@game)

        @game.currentRound().currentTrick().played.cards[0].should.equal(card)

    describe "#validate", ->
      it "returns null for valid actions", ->
        card = @game.currentRound().north.dealt.cards[0]
        action = new actions.PlayCard(@game.positions.north, card)

        should.not.exist action.validate(@game)

      it "returns an error if playing a card not in hand", ->
        someOtherCard = @game.currentRound().south.dealt.cards[0]
        action = new actions.PlayCard(@game.positions.north, someOtherCard)

        error = action.validate(@game)

        error.type.should.equal("invalidMove")
        error.message.should.equal("Must play a card in your hand.")

      it "returns an error if playing a heart when unbroken"
      it "returns an error if not playing two of clubs on first hand"
      it "returns an error if not following suit"
      it "returns an error if same card played multiple times"

      it "returns an error when in wrong state", ->
        @game.currentState = @game.states.endingGame

        card = @game.currentRound().north.held[0]
        action = new actions.PlayCard(@game.positions.north, card)

        error = action.validate(@game)
        error.type.should.equal("outOfSequence")
        error.message.should.equal("Action requested out of sequence.")

      it "returns an error when waiting for card from another player", ->
        card = @game.currentRound().south.held[0]
        action = new actions.PlayCard(@game.positions.south, card)

        error = action.validate(@game)
        error.type.should.equal("outOfSequence")
        error.message.should.equal("Card played out of sequence.")
