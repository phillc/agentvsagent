{EventEmitter} = require 'events'
logger = require '../logger'
machina = require('machina')()
und = require 'underscore'
Round = require './round'
actions = require './actions'

module.exports = class Game
  #TODO: rename to availablePositions to distinguish between what can be filled, vs instance level is what IS filled.
  @positions = -> ["north", "east", "south", "west"]

  constructor: (options={}) ->
    @maxPenalty = options.heartsMaxPoints || 100
    @engine = new Engine(game: this)
    @emitter = new EventEmitter()

    @state =
      rounds: []

  on: (args...) ->
    @emitter.on(args...)

  emitAll: (message, data) ->
    for position in @positions()
      do (position, message, data) =>
        @emitPosition(position, message, data)

  emitPosition: (position, message, data) ->
    logger.verbose "GAME emitting to #{position} - #{message} with #{data}"
    heard = @emitter.emit([position, message].join("."), data)
    if !heard
      logger.error "no one was listening to #{message}"

  positions: ->
    Game.positions()

  currentRound: ->
    @state.rounds[@state.rounds.length - 1]

  start: ->
    logger.info "Starting a #{@maxPenalty} point game of hearts"
    for position in @positions()
      @emitPosition position, "roundStarted", position: position
    @engine.transition("startingRound")

  handle: (args...) ->
    @engine.handle(args...)

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

  handlePassCards: (data, position) ->
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

  handlePlayCard: (data, position) ->
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
      _onEnter: ->
        @readyForRound = []
      "readyForRound.north": -> @handleReadyForRound("north")
      "readyForRound.east": -> @handleReadyForRound("east")
      "readyForRound.south": -> @handleReadyForRound("south")
      "readyForRound.west": -> @handleReadyForRound("west")

    passing:
      _onEnter: ->
        @passedCards = {}
      "passCards.north": (data) ->
        @handlePassCards(data, "north")
      "passCards.east": (data) ->
        @handlePassCards(data, "east")
      "passCards.south": (data) ->
        @handlePassCards(data, "south")
      "passCards.west": (data) ->
        @handlePassCards(data, "west")

    startingTrick:
      _onEnter: ->
        @readyForTrick = []
      "readyForTrick.north": ->
        @handleReadyForTrick("north")
      "readyForTrick.east": ->
        @handleReadyForTrick("east")
      "readyForTrick.south": ->
        @handleReadyForTrick("south")
      "readyForTrick.west": ->
        @handleReadyForTrick("west")

    waitingForCardFromNorth:
      "playCard.north": (data) ->
        @handlePlayCard(data, "north")

    waitingForCardFromEast:
      "playCard.east": (data) ->
        @handlePlayCard(data, "east")

    waitingForCardFromSouth:
      "playCard.south": (data) ->
        @handlePlayCard(data, "south")

    waitingForCardFromWest:
      "playCard.west": (data) ->
        @handlePlayCard(data, "west")

    endingRound:
      _onEnter: ->
        @finishedRound = []
      "finishedRound.north": ->
        @handleFinishedRound("north")
      "finishedRound.east": ->
        @handleFinishedRound("east")
      "finishedRound.south": ->
        @handleFinishedRound("south")
      "finishedRound.west": ->
        @handleFinishedRound("west")

    endingGame:
      _onEnter: ->
        @finishedGame = []
      "finishedGame.north": ->
        @handleFinishedGame("north")
      "finishedGame.east": ->
        @handleFinishedGame("east")
      "finishedGame.south": ->
        @handleFinishedGame("south")
      "finishedGame.west": ->
        @handleFinishedGame("west")
    aborted: {}
    finished: {}



