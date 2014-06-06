Agent = require "../../lib/agent"

describe "Agent", ->
  beforeEach ->
    @agent = new Agent()

  describe "states", ->
    describe "waitingForClient", ->
      describe "#forward", ->
        it "emits the message", (done) ->
          @agent.in.on 'foo', (data) ->
            expect(data).to.eql({foo: "bar"})
            done()

          @agent.forward("foo", {foo: "bar"})

        it "moves to waiting for server", ->
          @agent.forward()
          expect(@agent.connectionState.state).to.equal("waitingForServer")

        it "returns an error if called twice", (done) ->
          @agent.forward()

          @agent.out.on "failure", (error) ->
            expect(error.message).to.equal("unexpectedMessage")
            done()

          @agent.forward()

      describe "#send", ->
        beforeEach ->
          @agent.forward()
          expect(@agent.connectionState.state).to.equal("waitingForServer")

        it "moves to waiting for client", ->
          @agent.send("foo")
          expect(@agent.connectionState.state).to.equal("waitingForClient")

      it "times out if taking longer than the timeout time", (done) ->
        agent = new Agent(timeout: 10)
        expect(agent.connectionState.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.connectionState.state).to.equal("timedOut")
          agent.forward()
          expect(agent.connectionState.state).to.equal("timedOut")
          done()
        , 25

      it "does not timeout if there is no timeout time", (done) ->
        agent = new Agent()
        expect(agent.connectionState.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.connectionState.state).to.equal("waitingForClient")
          agent.forward()
          expect(agent.connectionState.state).to.equal("waitingForServer")
          done()
        , 25

      it "does not timeout if timeout time is not exceeded", (done) ->
        agent = new Agent(timeout: 100)
        expect(agent.connectionState.state).to.equal("waitingForClient")
        setTimeout ->
          expect(agent.connectionState.state).to.equal("waitingForClient")
          agent.forward()
          expect(agent.connectionState.state).to.equal("waitingForServer")
          done()
        , 25

    describe "waitingForServer", ->
      beforeEach ->
        @agent.forward()
        expect(@agent.connectionState.state).to.equal("waitingForServer")

      it "returns an error for any forwards", (done) ->
        @agent.out.on "failure", (error) ->
          expect(error.message).to.equal("unexpectedMessage")
          done()

        @agent.forward()

      it "sends something", (done) ->
        @agent.out.on "success", ->
          done()

        @agent.send()

      it "sends and transitions to finished", (done) ->
        @agent.out.on "success", ->
          done()

        @agent.send("end")
        expect(@agent.connectionState.state).to.equal("finished")


      it "sends and transitions to finished on error", (done) ->
        @agent.out.on "failure", (message) ->
          expect(message.message).to.equal("error")

          done()

        @agent.send("error")
        expect(@agent.connectionState.state).to.equal("finished")


    describe "timedout", ->
      it "emits timeout", (done) ->
        @agent.in.on "timeout", ->
          done()
        @agent.connectionState.transition("timedOut")


