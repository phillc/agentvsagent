Factory = require "../factory"
actions = require("../../../hearts/game/actions")
require("should")

describe "actions", ->
  describe "PassCards", ->
    beforeEach ->
      @game = Factory.createGame()

      @nextStateCalls = 0
      @game.nextState = =>
        @nextStateCalls++

      @player = @game.players[0]
      @game.states.startingRound.run()
      @action = new actions.PassCards(@player, ["1"])

    it "applies the passed cards for the player", ->
      @action.run(@game)

      @game.currentRound.north.passedCards[0].should.equal("1")



