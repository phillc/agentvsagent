Queue = require "../../lib/queue"
should = require("should")

describe "Queue", ->
  beforeEach ->
    @queue = new Queue(["dealt"])

  describe "#process", ->
    it "returns waiting messages", (done) ->
      @queue.sendDealt "foo"
      @queue.process "dealt", (err, result) ->
        should.not.exist(err)
        result.should.eql "foo"
        done()

    it "returns outOfSequence errors", (done) ->
      @queue.sendDealt "foo"
      @queue.process "passed", (err, result) ->
        err.type.should.eql "outOfSequence"
        err.message.should.eql "Method call out of sequence"
        should.not.exist(result)
        done()

