Validator = require "../../../lib/fireworks/validator"
Pile = require "../../../lib/fireworks/pile"
Builder = require "../../../lib/fireworks/builder"

describe "Validator", ->
  beforeEach ->
    @validator = new Validator()

  it "maps to the builder", ->
    expect(new Builder().agentEvents).to.eql(Object.keys(@validator.schemas))

  describe "#validate", ->
    describe "move", ->
      it "does not allow foo", (done) ->
        @validator.validate "move", foo: 123, (err) ->
          expect(err).to.not.be.empty
          done()

      it "allows a rank hint", (done) ->
        @validator.validate "move", hint: { position: "player2", rank: 1 }, (err) ->
          expect(err).to.be.null
          done()

      it "allows a suit hint", (done) ->
        @validator.validate "move", hint: { position: "player2", suit: "blue" }, (err) ->
          expect(err).to.be.null
          done()

      it "allows a discard", (done) ->
        @validator.validate "move", discard: 0, (err) ->
          expect(err).to.be.null
          done()

      it "doesn't allow a hint and discard", (done) ->
        @validator.validate "move", hint: {}, discard: 0, (err) ->
          expect(err).to.not.be.empty
          done()
