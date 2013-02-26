Game = require("../../../app/hearts/models/game")
Player = require("../../../app/hearts/models/player")
require("chai").should()

describe "Game", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @player3 = new Player()
    @player4 = new Player()
    @game = new Game(@player1, @player2, @player3, @player4)

  it "has players", ->
    @game.players.length.should.equal(4)

  describe "#createDeck", ->
    beforeEach ->
      @deck = @game.createDeck()

    it "returns a pile with 52 cards", ->
      @deck.cards.length.should.equal(52)

  describe "#deal", ->
    it "gives 13 cards to each player", ->
      @player1.cards.length.should.equal(13)


