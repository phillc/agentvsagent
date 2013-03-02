Suit = require("../../../hearts/game/suit")
require("chai").should()

describe "Suit", ->
  beforeEach ->
    @suit = new Suit('foo')

  it "has a name", ->
    @suit.name.should.equal('foo')

  describe "::all", ->
    beforeEach ->
      @suits = Suit.all()

    it "returns the four suits", ->
      names = @suits.map (suit) ->
        suit.name

      names.indexOf('clubs').should.not.equal(-1)
      names.indexOf('diamonds').should.not.equal(-1)
      names.indexOf('spades').should.not.equal(-1)
      names.indexOf('hearts').should.not.equal(-1)
