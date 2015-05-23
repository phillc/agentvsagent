Game = require "../../../lib/skull/game"
actions = require "../../../lib/skull/actions"

describe "Game", ->
  beforeEach ->
    @game = new Game()

  # describe "#scores", ->
  #   it "returns the scores", ->
  #     @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})
  #     @game.state.rounds.push({ scores: -> { north: 0, east: 0, south: 15, west: 11 }})
  #     @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})
  #     @game.state.rounds.push({ scores: -> { north: 10, east: 0, south: 15, west: 1 }})

  #     scores = @game.scores()
  #     expect(scores.north).to.equal(30)
  #     expect(scores.east).to.equal(0)
  #     expect(scores.south).to.equal(60)
  #     expect(scores.west).to.equal(14)

  # describe "states", ->
  #   describe "startingRound", ->
  #     it "moves to the next state once all have checked in", ->
  #       @game.engine.transition("startingRound")
  #       @game.handle "readyForRound.player1"
  #       expect(@game.engine.state).to.equal("startingRound")
  #       @game.handle "readyForRound.player2"
  #       expect(@game.engine.state).to.equal("startingRound")
  #       @game.handle "readyForRound.player3"
  #       expect(@game.engine.state).to.equal("passing")

  #     it "response to other events with outOfSequence", (done) ->
  #       @game.engine.transition("startingRound")

  #       @game.on "north.error", (error) ->
  #         expect(error.type).to.equal("outOfSequence")
  #         done()

  #       @game.handle "passCards.north"
  #       expect(@game.engine.state).to.equal("aborted")

