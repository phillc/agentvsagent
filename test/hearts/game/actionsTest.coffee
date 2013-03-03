Factory = require "../factory"
actions = require("../../../hearts/game/actions")
require("chai").should()

describe "actions", ->
  describe "PassCards", ->
    beforeEach ->
      @game = Factory.createGame()
      @action = new actions.PassCards

    it "applies the passed cards for the player"
