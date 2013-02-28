Game = require("../../../hearts/server/models/game")
Player = require("../../../hearts/server/models/player")
require("chai").should()

# describe "Game", ->
  # beforeEach ->
  #   @player1 = new Player()
  #   @player2 = new Player()
  #   @player3 = new Player()
  #   @player4 = new Player()
  #   @game = new Game(@player1, @player2, @player3, @player4)

  # it "has players", ->
  #   @game.players.length.should.equal(4)

  # describe "#createDeck", ->
  #   beforeEach ->
  #     @deck = @game.createDeck()

  #   it "returns a pile with 52 cards", ->
  #     @deck.cards.length.should.equal(52)

  # describe "#deal", ->
  #   beforeEach ->
  #     @game.deal(@game.createDeck())

  #   it "gives 13 cards to each player", ->
  #     @player1.held.cards.length.should.equal(13)
  #     @player2.held.cards.length.should.equal(13)
  #     @player3.held.cards.length.should.equal(13)
  #     @player4.held.cards.length.should.equal(13)

  # describe "#passCards", ->
  #   describe "on the right turn", ->
  #     beforeEach ->
  #       @game.passCards("right")

  #     it "asks each player for"

  #   describe "on the left turn", ->
  #     beforeEach ->
  #       @game.passCards("left")

  #   describe "on the across turn", ->
  #     beforeEach ->
  #       @game.passCards("across")

  #   describe "on the hold turn", ->
  #     beforeEach ->
  #       @game.passCards("hold")


