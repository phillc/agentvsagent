Factory = require "../factory"
actions = require("../../../hearts/game/actions")
Card = require("../../../hearts/game/card")
Pile = require("../../../hearts/game/pile")
require("should")

describe "actions", ->
  describe "PassCards", ->
    beforeEach ->
      @game = Factory.createGame()

      @nextStateCalls = 0
      @game.nextState = =>
        @nextStateCalls++

      @game.states.startingRound.run()

    it "applies the passed cards for the player", ->
      allCards = Card.all()
      northCards = allCards[0..3]
      southCards = allCards[7..15]
      northAction = new actions.PassCards(@game.northPlayer, northCards)
      southAction = new actions.PassCards(@game.southPlayer, southCards)

      northAction.run(@game)
      southAction.run(@game)

      @game.currentRound.north.passed.cards.should.eql(northCards)
      @game.currentRound.south.passed.cards.should.eql(southCards)



