Agent = require "../../lib/agent"

describe "Agent", ->
  beforeEach ->
    @agent = new Agent()

  describe "states", ->
    describe "waitingForClient", ->
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
          @agent.forward().fail (error) ->
            expect(error).to.equal("unexpectedMessage")
            done()
          .done()

      describe "#send", ->
        beforeEach ->
          @agent.forward()
          expect(@agent.state.state).to.equal("waitingForServer")

        it "moves to waiting for client", ->
          @agent.send("foo")
          expect(@agent.state.state).to.equal("waitingForClient")

      it "times out if taking longer than the timeout time", (done) ->
        agent = new Agent(timeout: 10)
        expect(agent.state.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.state.state).to.equal("timedOut")
          agent.forward()
          expect(agent.state.state).to.equal("timedOut")
          done()
        , 25

      it "does not timeout if there is no timeout time", (done) ->
        agent = new Agent()
        expect(agent.state.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.state.state).to.equal("waitingForClient")
          agent.forward()
          expect(agent.state.state).to.equal("waitingForServer")
          done()
        , 25

      it "does not timeout if timeout time is not exceeded", (done) ->
        agent = new Agent(timeout: 100)
        expect(agent.state.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.state.state).to.equal("waitingForClient")
          agent.forward()
          expect(agent.state.state).to.equal("waitingForServer")
          done()
        , 25

    describe "waitingForServer", ->
      beforeEach ->
        @promise = @agent.forward()
        expect(@agent.state.state).to.equal("waitingForServer")

      it "returns an error for any forwards", (done) ->
        @agent.forward().fail (error) ->
          expect(error).to.equal("unexpectedMessage")
          done()
        .done()

      it "sends something", (done) ->
        @agent.send()
        @promise.then ->
          done()

      it "sends and transitions to finished", (done) ->
        @agent.send("end")
        expect(@agent.state.state).to.equal("finished")
        @promise.then ->
          done()

      it "sends and transitions to finished on error", (done) ->
        @agent.send("error")
        expect(@agent.state.state).to.equal("finished")

        @promise.fail (message) ->
          expect(message.message).to.equal("error")

          done()

    describe "timedout", ->
      it "emits timeout", (done) ->
        @agent.on "timeout", ->
          done()
        @agent.state.transition("timedOut")


