Factory = require "../../factory"
actions = require "../../../lib/hearts/engine/actions"
Card = require "../../../lib/hearts/engine/card"
Pile = require "../../../lib/hearts/engine/pile"
require("should")

describe "actions", ->
  beforeEach ->
    @game = Factory.createGame()
    @nextStateCalls = 0
    @game.nextState = =>
      @nextStateCalls++

  describe "PassCards", ->
    beforeEach ->
      @game.states.startingRound.run()

    it "applies the passed cards for the player", ->
      allCards = Card.all()
      northCards = allCards[0..2]
      southCards = allCards[7..9]
      northAction = new actions.PassCards(@game.positions.north, northCards)
      southAction = new actions.PassCards(@game.positions.south, southCards)

      northAction.execute(@game, ->)
      southAction.execute(@game, ->)

      @game.currentRound().north.passed.cards.should.eql(northCards)
      @game.currentRound().south.passed.cards.should.eql(southCards)

    it "returns an error if less than three cards", (done) ->
      allCards = Card.all()
      cards = allCards[0..1]
      action = new actions.PassCards(@game.positions.north, cards)

      action.execute @game, (err) ->
        err[0].should.equal "invalidMove"
        err[1].should.equal "Must pass three cards. You passed 2."
        done()

    it "returns an error if more than three cards", (done) ->
      allCards = Card.all()
      cards = allCards[1..4]
      action = new actions.PassCards(@game.positions.north, cards)

      action.execute @game, (err) ->
        err[0].should.equal "invalidMove"
        err[1].should.equal "Must pass three cards. You passed 4."
        done()

  describe "PlayCard", ->
    beforeEach ->
      @game.states.startingRound.run()
      @game.states.startingTrick.run()

    it "applies the card", ->
      card = Card.all()[0]
      action = new actions.PlayCard(@game.positions.north, card)
      action.execute(@game, ->)

      @game.currentRound().currentTrick().played.cards[0].should.equal(card)

