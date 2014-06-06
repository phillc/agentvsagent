Rank = require "../../../lib/hearts/rank"

describe "Rank", ->
  beforeEach ->
    @rank = new Rank(1, 'A')

  it "has an order", ->
    expect(@rank.order).to.equal(1)

  it "has a name", ->
    expect(@rank.name).to.equal('A')

  describe "::all", ->
    beforeEach ->
      @ranks = Rank.all()

    it "returns all the possible ranks", ->
      expect(@ranks).to.have.length(13)

    it "orders the 2 the lowest", ->
      expect(@ranks[0].name).to.equal('two')
      expect(@ranks[0].order).to.equal(1)

    it "orders the ace the highest", ->
      expect(@ranks[12].name).to.equal('ace')
      expect(@ranks[12].order).to.equal(13)

