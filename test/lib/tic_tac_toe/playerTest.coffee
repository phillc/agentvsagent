Player = require "../../../lib/tic_tac_toe/player"

describe "Player", ->
  beforeEach ->
    @player = new Player()

  it "has an id", ->
    expect(@player).to.have.property('id')

  describe "#forward", ->
    it "returns a promise that will be resolved on notify", (done) ->
      expect(@player.forward()).to.become("foo").notify(done)

      @player.send("foo")

    it "moves to waiting for server", ->
      @player.forward()
      expect(@player.state.state).to.equal("waitingForServer")

    it "returns an error if called twice", (done) ->
      @player.forward()
      expect(@player.forward()).to.be.rejected.with("unexpectedMessage").notify(done)

  describe "#notify", ->
