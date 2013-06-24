{EventEmitter} = require 'events'
IdGenerator = require '../../idGenerator'
logger = require '../../logger'
states = require './states'

module.exports = class Game extends EventEmitter
  constructor: (player1, player2, player3, player4, options={}) ->
    @maxPenalty = options.heartsMaxPoints || 100
    @turnTime = options.turnTime || 1000
    @id = IdGenerator.generate()
    @players = [player1, player2, player3, player4]

    # STATES
    @stack = []
    @states =
      startingGame: new states.StartingGame(this)
      startingRound: new states.StartingRound(this)
      startingTrick: new states.StartingTrick(this)
      dealing: new states.Dealing(this)
      passingLeft: new states.Passing(this, "left")
      passingRight: new states.Passing(this, "right")
      passingAcross: new states.Passing(this, "across")
      waitingForCardFromNorth: new states.WaitingForCard(this, "north")
      waitingForCardFromEast: new states.WaitingForCard(this, "east")
      waitingForCardFromSouth: new states.WaitingForCard(this, "south")
      waitingForCardFromWest: new states.WaitingForCard(this, "west")
      endingTrick: new states.EndingTrick(this)
      endingRound: new states.EndingRound(this)
      endingGame: new states.EndingGame(this)
      gameEnded: new states.GameEnded(this)

    # DATA
    @positions = {}
    @rounds = []

  currentRound: ->
    @rounds[@rounds.length - 1]

  positionOf: (player) ->
    if @positions.north == player
      "north"
    else if @positions.east == player
      "east"
    else if @positions.south == player
      "south"
    else if @positions.west == player
      "west"

  getPlayer: (playerId) ->
    for player in @players
      return player if player.id == playerId

  nextState: ->
    logger.verbose "nextState:: stack",  @stack
    @currentState = @states[@stack.pop()]
    @currentState.run()

  start: ->
    @stack.push("startingGame")
    @nextState()

  handleAction: (action) ->
    @currentState.handleAction(action)

  scores: ->
    @rounds.map((round) -> round.scores()).reduce (memo, scores) ->
      memo.north += scores.north
      memo.east += scores.east
      memo.south += scores.south
      memo.west += scores.west
      memo

  maxPenaltyReached: ->
    scores = @scores()

    [scores.north, scores.east, scores.south, scores.west].some (score) =>
      score >= @maxPenalty

  abort: (culprit, error) ->
    #what if game already aborted/gameEnded?
    logger.warn "Game has been aborted: #{error.type} :: #{error.message}."
    culprit.raiseError(error)
    for player in @players when player isnt culprit
      player.raiseError type: "gameAborted", message: "Game ended due to an invalid action by another agent."
    @stack.splice(0, @stack.length)
    @stack.push("gameEnded")
    @nextState()

