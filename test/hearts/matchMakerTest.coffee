MatchMaker = require '../../hearts/matchmaker'
ThriftAgent = require '../../hearts/thriftagent'
require("chai").should()

describe "MatchMaker", ->
  beforeEach ->
    @match_maker = new MatchMaker()

  describe "#addAgent", ->
    it "adds agents", ->
      agent = new ThriftAgent()
      @match_maker.waitingAgents.length.should.equal(0)
      @match_maker.addAgent agent
      @match_maker.waitingAgents.length.should.equal(1)


