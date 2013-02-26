Card = require("../../../app/hearts/models/card")
require("chai").should()

describe "Card", ->
  it "has a suit", ->
    new Card('spades', 4).suit.should.equal('spades')

  it "has a rank", ->
    new Card('spades', 4).rank.should.equal(4)

