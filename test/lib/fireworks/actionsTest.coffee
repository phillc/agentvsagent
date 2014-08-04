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

      it "increases the number of available hints", ->
        @game.hints = 6
        new actions.Discard(1).execute(@game, "player1")
        expect(@game.hints).to.equal(7)

    describe "#validate", ->
      # it "requires that a card is still in that slot"
      # it "requires that there are still cards to draw"
      # it "requires that the maximum hints has not been reached"

  describe "Hint", ->
    beforeEach ->
      # @game.startRound()

    describe "#execute", ->
      it "reduces the number of available hints", ->
        expect(@game.hints).to.equal(8)
        new actions.SuitHint("hearts").execute(@game, "player1")
        expect(@game.hints).to.equal(7)

    describe "#validate", ->
      it "returns null for valid actions", ->
        action = new actions.SuitHint("hearts")

        expect(action.validate(@game, "player1")).to.not.exist

      it "requires that there are still hints to give", ->
        action = new actions.SuitHint("hearts")
        @game.hints = 0

        error = action.validate(@game, "player1")
        expect(error.type).to.equal "invalidMove"
        expect(error.message).to.equal "Out of hints."

