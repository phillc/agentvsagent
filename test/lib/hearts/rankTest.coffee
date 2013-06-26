Rank = require "../../../lib/hearts/rank"
require("should")

describe "Rank", ->
  beforeEach ->
    @rank = new Rank(1, 'A')

  it "has an order", ->
    @rank.order.should.equal(1)

  it "has a name", ->
    @rank.name.should.equal('A')

  describe "::all", ->
    beforeEach ->
      @ranks = Rank.all()

    it "returns all the possible ranks", ->
      @ranks.length.should.equal(13)

    it "orders the 2 the lowest", ->
      @ranks[0].name.should.equal(2)
      @ranks[0].order.should.equal(1)

    it "orders the ace the highest", ->
      @ranks[12].name.should.equal('A')
      @ranks[12].order.should.equal(13)

