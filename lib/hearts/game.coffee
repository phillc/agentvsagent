{EventEmitter} = require 'events'
logger = require '../logger'
machina = require('machina')()
und = require 'underscore'
Round = require './round'

module.exports = class Game
  @EVENTS = ["roundStarted", "dealt", "received", "turn", "finishedTrick", "roundFinished", "gameFinished"]
  @positions = -> ["north", "east", "south", "west"]

  constructor: (options={}) ->
    @maxPenalty = options.heartsMaxPoints || 100
    @engine = new Engine(game: this)
    @emitter = new EventEmitter()
    # @turnTime = options.turnTime || 1000

    @state =
      rounds: []

  on: (args...) ->
    @emitter.on(args...)

  emitAll: (message, data) ->
    for position in @positions()
      do (position, message, data) =>
        @emitPosition(position, message, data)

  emitPosition: (position, message, data) ->
    # maybe, change to logger, or only in dev, or do this in tests?
    if Game.EVENTS.indexOf(message) < 0
      throw new Error("Unexpected event #{message}")

    logger.verbose "GAME emitting to #{position} - #{message} with #{data}"
    heard = @emitter.emit([position, message].join("."), data)
    if !heard
      logger.error "no one was listening to #{message}"

  positions: ->
    Game.positions()

  currentRound: ->
    @state.rounds[@state.rounds.length - 1]

  start: ->
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
    @emitPosition position, "turn", trick: @currentRound().currentTrick()
    @engine.transition("waitingForCardFrom" + position.charAt(0).toUpperCase() + position.slice(1))

  finishTrick: ->
    @emitAll "finishedTrick", trick: @currentRound().currentTrick()

    if @currentRound().tricks.length < 13
      @engine.transition("startingTrick")
    else
      @engine.transition("endingRound")

  finishRound: ->
    logger.verbose "round ended", @currentRound().scores()
    if @maxPenaltyReached()
      @engine.transition("endingGame")
      @emitAll("roundFinished", roundScores: @currentRound().scores(), status: 'endGame')
    else
      @engine.transition("startingRound")
      @emitAll("roundFinished", roundScores: @currentRound().scores(), status: 'nextRound')

  finish: ->
    @emitAll("gameFinished", gameScores: @scores())
    @engine.transition("done")


  # abort: (culprit, error) ->
  #   #what if game already aborted/gameEnded?
  #   logger.warn "Game has been aborted: #{error.type} :: #{error.message}."
  #   culprit.raiseError(error)
  #   for player in @players when player isnt culprit
  #     player.raiseError type: "gameAborted", message: "Game ended due to an invalid action by another agent."
  #   @stack.splice(0, @stack.length)
  #   @stack.push("gameEnded")
  #   @nextState()

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

  handlePassCards: (action, position) ->
    # error = action.validate(@game)
    # if !error
    action.execute(@game, position)
    # else
    #   @game.abort(action.player, error)
    if @game.currentRound().allHavePassed()
      @game.finishPassing()

  handlePlayCard: (action, position) ->
    action.execute(@game, position)
    if nextPosition = @game.currentRound().currentTrick().positionsMissing()[0]
      @game.waitingForCardFrom(nextPosition)
    else
      @game.finishTrick()

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

  "*": ->
    logger.error "Unhandled event", arguments

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
      "passCards.north": (action) ->
        @handlePassCards(action, "north")
      "passCards.east": (action) ->
        @handlePassCards(action, "east")
      "passCards.south": (action) ->
        @handlePassCards(action, "south")
      "passCards.west": (action) ->
        @handlePassCards(action, "west")

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
      "playCard.north": (action) ->
        @handlePlayCard(action, "north")

    waitingForCardFromEast:
      "playCard.east": (action) ->
        @handlePlayCard(action, "east")

    waitingForCardFromSouth:
      "playCard.south": (action) ->
        @handlePlayCard(action, "south")

    waitingForCardFromWest:
      "playCard.west": (action) ->
        @handlePlayCard(action, "west")

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
    # go to done
    # aborted: (all checked in, go to done)
    done: {}


