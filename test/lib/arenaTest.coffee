Arena = require '../../lib/arena'

describe "Arena", ->
  beforeEach ->
    builder =
      positions: -> ["A", "B"]
      createGame: -> {start: ->}
    @arena = new Arena(builder, [])

  describe "#addAgent", ->
    it "adds agent", ->
      expect(@arena.waitingRoom).to.have.length(0)
      @arena.addAgent({})
      expect(@arena.waitingRoom).to.have.length(1)

    it "emits an event", (done) ->
      @arena.on 'newAgent', done
      @arena.addAgent({})

  describe "#removeAgent", ->
    beforeEach ->
      @agent1 = {}
      @arena.addAgent(@agent1)
      @agent2 = {}
      @arena.addAgent(@agent2)
      @agent3 = {}
      @arena.addAgent(@agent3)
      expect(@arena.waitingRoom).to.have.length(3)

    it "removes agents", ->
      @arena.removeAgent @agent2
      expect(@arena.waitingRoom).to.have.length(2)
      expect(@arena.waitingRoom[0]).to.equal(@agent1)
      expect(@arena.waitingRoom[1]).to.equal(@agent3)

  describe "#createGame", ->
    beforeEach ->
      for _ in [1..10]
        @arena.addAgent({})
      expect(@arena.waitingRoom).to.have.length(10)
      expect(Object.keys(@arena.runningMatches)).to.have.length(0)
      @matchedAgents = @arena.waitingRoom[2..5]

    it "removes the agents from the waiting room", ->
      @arena.createGame @matchedAgents
      expect(@arena.waitingRoom).to.have.length(6)

    it "creates a game out of the agents", ->
      @arena.createGame @matchedAgents
      expect(Object.keys(@arena.runningMatches)).to.have.length(1)

    it "returns the game", ->
      game = @arena.createGame @matchedAgents
      expect(game).to.equal(@arena.runningMatches[0])

    # it "adds a listener that cleans up the game after some period of time", (done) ->
    #   @arena.lingerTime = 50
    #   game = @arena.createGame @matchedAgents
    #   Object.keys(@arena.runningMatches).length.should.equal(1)
    #   game.emit 'gameEnded'
    #   setTimeout =>
    #     Object.keys(@arena.runningMatches).length.should.equal(0)
    #     done()
    #   , 75

  #   it "adds doesn't clean up until after linger time", (done) ->
  #     @arena.lingerTime = 50
  #     game = @arena.createGame @matchedAgents
  #     Object.keys(@arena.runningMatches).length.should.equal(1)
  #     game.emit 'gameEnded'
  #     setTimeout =>
  #       Object.keys(@arena.runningMatches).length.should.equal(1)
  #       done()
  #     , 25

  # describe "#removeGame", ->
  #   it "removes the game from the running matches list", ->
  #     gameId = Factory.createGame(arena: @arena).id

  #     game = @arena.getGame gameId
  #     game.id.should.equal(gameId)

  #     @arena.removeGame(gameId)
  #     should.not.exist(@arena.getGame(gameId))

  # describe "#getGame", ->
  #   it "returns the game", ->
  #     gameId = Factory.createGame(arena: @arena).id
  #     game = @arena.getGame gameId
  #     game.id.should.equal(gameId)

  #   it "returns nothing if there is no game", ->
  #     should.not.exist(@arena.getGame("foo"))

