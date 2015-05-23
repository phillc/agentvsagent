{EventEmitter} = require 'events'
AbstractGame = require '../abstractGame'
logger = require '../logger'
machina = require('machina')()
und = require 'underscore'
Round = require './round'
actions = require './actions'

module.exports = class Game extends AbstractGame
  @availablePositions = -> ["north", "east", "south", "west"]

  constructor: (options={}) ->
    @maxPenalty = options.heartsMaxPoints || 100
    @engine = new Engine(game: this)
    @emitter = new EventEmitter()

    @state =
      rounds: []

  positions: ->
    Game.availablePositions()

  currentRound: ->
    @state.rounds[@state.rounds.length - 1]

  start: ->
    logger.info "Starting a #{@maxPenalty} point game of hearts"
    for position in @positions()
      @emitPosition position, "roundStarted", position: position
    @engine.transition("startingRound")

  scores: ->
    @state.rounds.map((round) -> round.scores()).reduce (memo, scores) ->
      memo.north += scores.north
      memo.east += scores.east
      memo.south += scores.south
      memo.west += scores.west
      memo

  maxPenaltyReached: ->
    scores = @scores()

    [scores.north, scores.east, scores.south, scores.west].some (score) =>
      score >= @maxPenalty

  startRound: ->
    logger.verbose "starting a round"
    round = @newRound()
    round.deal()
    for position in @positions()
      @emitPosition position, "dealt", cards: round.seats[position].dealt.cards

    if round.passing
      @engine.transition("passing")
    else
      @engine.transition("startingTrick")

  finishPassing: ->
    @currentRound().exchange()
    for position in @positions()
      @emitPosition position, "received", cards: @currentRound().seats[position].received.cards
    @engine.transition("startingTrick")

  newRound: ->
    direction = ["left", "right", "across"][(@state.rounds.length) % 4]
    round = new Round(direction)
    @state.rounds.push(round)
    round

  startTrick: ->
    trick = @currentRound().newTrick()
    @waitingForCardFrom(trick.positionsMissing()[0])

  waitingForCardFrom: (position) ->
    @emitPosition position, "turn", @currentRound().currentTrick()
    @engine.transition("waitingForCardFrom" + position.charAt(0).toUpperCase() + position.slice(1))

  finishTrick: ->
    @emitAll "finishedTrick", @currentRound().currentTrick()

    if @currentRound().tricks.length < 13
      @engine.transition("startingTrick")
    else
      @engine.transition("endingRound")

  finishRound: ->
    scores = @currentRound().scores()
    logger.verbose "round ended", scores
    if scores.shooter
      logger.info "#{scores.shooter} HAS SHOT THE MOON!"

    if @maxPenaltyReached()
      @engine.transition("endingGame")
      @emitAll("roundFinished", roundScores: scores, status: 'endGame')
    else
      @engine.transition("startingRound")
      @emitAll("roundFinished", roundScores: scores, status: 'nextRound')

  finish: ->
    scores = @scores()
    logger.info "Game ending with scores:", scores
    @emitAll("end", gameScores: scores)
    @engine.transition("finished")

  abort: (culprit, error) ->
    logger.warn "Game has been aborted: #{error.type} :: #{error.message}."
    @emitPosition(culprit, "error", error)
    for position in @positions() when position isnt culprit
      @emitPosition(position, "error", type: "gameAborted", message: "Game ended due to an invalid action by another agent.")
    @engine.transition("aborted")



Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      # console.log "engine changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"
      logger.verbose "*ENGINE*: changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  handleReadyForRound: (position) ->
    @readyForRound.push(position)
    if @readyForRound.length == 4
      @game.startRound()

  handlePassCards: (position, data) ->
    action = actions.PassCards.build(data)
    @passedCards[position] = action
    if und.size(@passedCards) == 4
      errors = {}
      for position, action of @passedCards
        do (errors, position, action) =>
          error = action.validate(@game, position)
          if !error
            action.execute(@game, position)
          else
            errors[position] = error
      # if @game.currentRound().allHavePassed()
      if und.isEmpty(errors)
        @game.finishPassing()
      else
        errorPosition = Object.keys(errors)[0]
        @game.abort(errorPosition, errors[errorPosition])

  handlePlayCard: (position, data) ->
    action = actions.PlayCard.build(data)
    error = action.validate(@game, position)
    if !error
      action.execute(@game, position)
      if nextPosition = @game.currentRound().currentTrick().positionsMissing()[0]
        @game.waitingForCardFrom(nextPosition)
      else
        @game.finishTrick()
    else
      @game.abort(position, error)

  handleReadyForTrick: (position) ->
    @readyForTrick.push(position)
    if @readyForTrick.length == 4
      @game.startTrick()

  handleFinishedRound: (position) ->
    @finishedRound.push(position)
    if @finishedRound.length == 4
      @game.finishRound()

  handleFinishedGame: (position) ->
    @finishedGame.push(position)
    if @finishedGame.length == 4
      @game.finish()

  "*": (message, args...) ->
    [event, position] = message.split(".")
    if event == "timeout"
      logger.error "timeout received", position
      @game.abort(position, {type: "timeout", message: "Your action took longer than allowed"})
    else
      logger.error "Unexpected event", arguments
      @game.abort(position, {type: "outOfSequence", message: "Method call out of sequence"})

  initialState: "initialized"

  states:
    initialized: {}
    startingRound:
      Game.buildPositionHandlers "readyForRound",
        (position) -> @handleReadyForRound(position)
        _onEnter: ->
          @readyForRound = []

    passing:
      Game.buildPositionHandlers "passCards",
        (position, data) -> @handlePassCards(position, data)
        _onEnter: ->
          @passedCards = {}

    startingTrick:
      Game.buildPositionHandlers "readyForTrick",
        (position, data) -> @handleReadyForTrick(position, data)
        _onEnter: ->
          @readyForTrick = []

    waitingForCardFromNorth:
      "playCard.north": (data) ->
        @handlePlayCard("north", data)

    waitingForCardFromEast:
      "playCard.east": (data) ->
        @handlePlayCard("east", data)

    waitingForCardFromSouth:
      "playCard.south": (data) ->
        @handlePlayCard("south", data)

    waitingForCardFromWest:
      "playCard.west": (data) ->
        @handlePlayCard("west", data)

    endingRound:
      Game.buildPositionHandlers "finishedRound",
        (position, data) -> @handleFinishedRound(position, data)
        _onEnter: ->
          @finishedRound = []

    endingGame:
      Game.buildPositionHandlers "finishedGame",
        (position, data) -> @handleFinishedGame(position, data)
        _onEnter: ->
          @finishedGame = []
    aborted: {}
    finished: {}



