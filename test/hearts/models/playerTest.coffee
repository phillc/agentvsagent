Player = require("../../../app/hearts/models/player")
require("chai").should()

describe "Player", ->
  it "has an empty held pile", ->
    new Player().held.cards.should.eql([])

  it "has an empty taken hands", ->
    new Player().taken.should.eql([])
