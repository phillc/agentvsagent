Game = require "../../../lib/fireworks/game"
actions = require "../../../lib/fireworks/actions"

describe "actions", ->
  beforeEach ->
    @game = new Game({positions: ["player1", "player2"]})

  describe "Discard", ->
    beforeEach ->
      # @game.startRound()

    describe "#execute", ->
      it "replaces the card", ->
        @game.deal()
        card0 = @game.seats["player1"].cards[0]
        card2 = @game.seats["player1"].cards[2]
        card3 = @game.seats["player1"].cards[3]
        card4 = @game.seats["player1"].cards[4]

        nextCard = @game.deck.cards[0]

        new actions.Discard(1).execute(@game, "player1")

        expect(@game.seats["player1"].cards[0]).to.equal(card0)
        expect(@game.seats["player1"].cards[2]).to.equal(card2)
        expect(@game.seats["player1"].cards[3]).to.equal(card3)
        expect(@game.seats["player1"].cards[4]).to.equal(card4)

        expect(@game.seats["player1"].cards[1]).to.equal(nextCard)

    describe "#validate", ->
      # it "requires that a card is still in that slot"
      # it "requires that there are still cards to draw"
