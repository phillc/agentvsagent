Player = require("../../../app/hearts/models/player")
require("chai").should()

describe "Player", ->
  it "has a held pile", ->
    new Player().held.cards.should.equal([])

  it "has a taken pile", ->
    new Player().taken.cards.should.equal([])
