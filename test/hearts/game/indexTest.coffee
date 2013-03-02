Game = require("../../../hearts/game")
Player = require("../../../hearts/player")
should = require("chai").should()

describe "Game", ->
  beforeEach ->
    @player1 = new Player()
    @player2 = new Player()
    @player3 = new Player()
    @player4 = new Player()
    @game = new Game(@player1, @player2, @player3, @player4)

  it "has an id", ->
    @game.should.have.property('id')

  it "has players", ->
    @game.players.length.should.equal(4)

  describe "#getPlayer", ->
    it "returns a player by id", ->
      @game.getPlayer(@player4.id).id.should.equal(@player4.id)

    it "returns nothing if player doesn't exist", ->
      should.not.exist(@game.getPlayer("foo"))

  describe "#start", ->
    it "emits a start game event on the players", (done) ->
      @player1.once 'start', (gameId) =>
        gameId.should.equal(@game.id)
        done()

      @game.start()


  # describe.only "#createDeck", ->
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


