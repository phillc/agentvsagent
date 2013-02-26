Card = require("../../../app/hearts/models/card")
require("chai").should()

describe "Card", ->
  it "has a suit", ->
    new Card(4, 'spades').suit.should.equal('spades')

  it "has a rank", ->
    new Card(4, 'spades').rank.should.equal(4)

