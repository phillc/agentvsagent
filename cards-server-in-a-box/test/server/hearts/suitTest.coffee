Suit = require "../../../src/server/hearts/suit"

describe "Suit", ->
  beforeEach ->
    @suit = new Suit('foo')

  it "has a name", ->
    expect(@suit.name).to.equal('foo')

  describe "::all", ->
    beforeEach ->
      @suits = Suit.all()

    it "returns the four suits", ->
      names = @suits.map (suit) ->
        suit.name

      expect(names.indexOf('clubs')).to.not.equal(-1)
      expect(names.indexOf('diamonds')).to.not.equal(-1)
      expect(names.indexOf('spades')).to.not.equal(-1)
      expect(names.indexOf('hearts')).to.not.equal(-1)
