Validator = require "../../../src/server/hearts/validator"
Pile = require "../../../src/server/hearts/pile"
Builder = require "../../../src/server/hearts/builder"

describe "Validator", ->
  beforeEach ->
    @validator = new Validator()

  it "maps to the builder", ->
    expect(new Builder().agentEvents).to.eql(Object.keys(@validator.schemas))

  describe "#validate", ->
    describe "passCards", ->
      it "does not allow foo", (done) ->
        @validator.validate "passCards", foo: 123, (err) ->
          expect(err).to.not.be.empty
          done()

      it "allows cards", (done) ->
        deck = Pile.createDeck()
        cards = new Pile()
        deck.moveCardsTo(3, cards)
        @validator.validate "passCards", cards: cards.toJSON(), (err) ->
          expect(err).to.be.null
          done()
