Agent = require "../../lib/agent"

describe "Agent", ->
  beforeEach ->
    @agent = new Agent()

  describe "#forward", ->
    it "returns a promise that will be resolved on send", (done) ->
      expect(@agent.forward()).to.eventually.eql(message: "foo", data: {foo: "bar"}).notify(done)

      @agent.send("foo", {foo: "bar"})

    it "returns a promise that will be resolved with no data on send", (done) ->
      expect(@agent.forward()).to.eventually.eql(message: "foo", data: {}).notify(done)

      @agent.send("foo")

    it "emits the message", (done) ->
      @agent.on 'foo', (data) ->
        expect(data).to.eql({foo: "bar"})
        done()

      @agent.forward("foo", {foo: "bar"})

    it "moves to waiting for server", ->
      @agent.forward()
      expect(@agent.state.state).to.equal("waitingForServer")

    it "returns an error if called twice", (done) ->
      @agent.forward()
      expect(@agent.forward()).to.be.rejected.with("unexpectedMessage").notify(done)

  describe "#send", ->
    beforeEach ->
      @agent.forward()
      expect(@agent.state.state).to.equal("waitingForServer")

    it "moves to waiting for client", ->
      @agent.send("foo")
      expect(@agent.state.state).to.equal("waitingForClient")
