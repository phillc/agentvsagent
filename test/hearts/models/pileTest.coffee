Pile = require("../../../app/hearts/models/pile")
require("chai").should()

describe "Pile", ->
  it "starts with no cards", ->
    new Pile().cards.should.eql([])

