Factory = require "../factory"
actions = require("../../../hearts/game/actions")
Card = require("../../../hearts/game/card")
Pile = require("../../../hearts/game/pile")
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
      northCards = allCards[0..3]
      southCards = allCards[7..15]
      northAction = new actions.PassCards(@game.northPlayer, northCards)
      southAction = new actions.PassCards(@game.southPlayer, southCards)

      northAction.execute(@game)
      southAction.execute(@game)

      @game.currentRound.north.passed.cards.should.eql(northCards)
      @game.currentRound.south.passed.cards.should.eql(southCards)

  describe "PlayCard", ->
    beforeEach ->
      @game.states.startingRound.run()
      @game.states.startingTrick.run()

    it "applies the card", ->
      card = Card.all()[0]
      action = new actions.PlayCard(@game.northPlayer, card)
      action.execute(@game)

      @game.currentRound.tricks[0].north.should.equal(card)


