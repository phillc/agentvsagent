Player = require "../../../lib/tic_tac_toe/player"
should = require("should")

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    @player.should.have.property('id')

