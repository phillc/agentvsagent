MatchMaker = require '../../lib/matchMaker'

describe "MatchMaker", ->
  beforeEach ->
    @arena = Factory.createArena()
    @matchMaker = new MatchMaker(@arena)

  describe "#findMatch", ->
    it "does nothing if there are only three players", ->
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())

      @matchMaker.findMatch()

      expect(Object.keys(@arena.runningMatches)).to.have.length(0)
      expect(@arena.waitingRoom).to.have.length(3)

    it "creates a game if there are four players", ->
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())

      @matchMaker.findMatch()

      expect(Object.keys(@arena.runningMatches)).to.have.length(1)
      expect(@arena.waitingRoom).to.have.length(0)

    it "creates a game if there are five players", ->
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())

      @matchMaker.findMatch()

      expect(Object.keys(@arena.runningMatches)).to.have.length(1)
      expect(@arena.waitingRoom).to.have.length(1)

    it "creates a game if there are two players for a game of two players", ->
      @arena.numberOfAgents = 2
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())

      @matchMaker.findMatch()

      expect(Object.keys(@arena.runningMatches)).to.have.length(1)
      expect(@arena.waitingRoom).to.have.length(0)

  describe "#start", ->
    it "automatically finds matches as players join", ->
      expect(Object.keys(@arena.runningMatches)).to.have.length(0)
      @matchMaker.start()
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      @arena.addAgent(Factory.createAgent())
      expect(Object.keys(@arena.runningMatches)).to.have.length(1)


