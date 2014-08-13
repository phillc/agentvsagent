Game = require "../../../lib/fireworks/game"
actions = require "../../../lib/fireworks/actions"

describe "actions", ->
  beforeEach ->
    @game = new Game({positions: ["player1", "player2"]})

  describe "Discard", ->
    describe "#execute", ->
      it "replaces the card", ->
        @game.deal()
        card0 = @game.seats["player1"].held.cards[0]
        card2 = @game.seats["player1"].held.cards[2]
        card3 = @game.seats["player1"].held.cards[3]
        card4 = @game.seats["player1"].held.cards[4]

        nextCard = @game.deck.cards[0]

        new actions.Discard(1).execute(@game, "player1")

        expect(@game.seats["player1"].held.cards[0]).to.equal(card0)
        expect(@game.seats["player1"].held.cards[2]).to.equal(card2)
        expect(@game.seats["player1"].held.cards[3]).to.equal(card3)
        expect(@game.seats["player1"].held.cards[4]).to.equal(card4)

        expect(@game.seats["player1"].held.cards[1]).to.equal(nextCard)

      it "increases the number of available hints", ->
        @game.hints = 6
        new actions.Discard(1).execute(@game, "player1")
        expect(@game.hints).to.equal(7)

      it "adds it to the messages list for the other player", ->
        expect(@game.seats["player2"].messages).to.be.empty
        new actions.Discard(1).execute(@game, "player1")
        expect(@game.seats["player2"].messages).to.have.length(1)


    describe "#validate", ->
      beforeEach ->
        @game.hints = 4

      it "returns null for valid actions", ->
        action = new actions.Discard(2)

        expect(action.validate(@game, "player1")).to.not.exist

      it "requires that a card is still in that slot"

      it "requires that there are still cards to draw", ->
        @game.deck.cards = []

        action = new actions.Discard(2)

        error = action.validate(@game, "player1")
        expect(error.type).to.equal "invalidMove"
        expect(error.message).to.equal "Out of cards to draw."

      it "requires that the maximum hints has not been reached", ->
        @game.hints = 8

        action = new actions.Discard(2)

        error = action.validate(@game, "player1")
        expect(error.type).to.equal "invalidMove"
        expect(error.message).to.equal "Out of hints to receive."

  describe "Hint", ->
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

