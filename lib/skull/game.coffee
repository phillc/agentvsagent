{EventEmitter} = require 'events'
AbstractGame = require '../abstractGame'
logger = require '../logger'
machina = require('machina')
und = require 'underscore'
actions = require './actions'

module.exports = class Game extends AbstractGame
  @availablePositions = -> ["player1", "player2", "player3", "player4", "player5"]

  constructor: (options={}) ->
    @engine = new Engine(game: this)
    @emitter = new EventEmitter()

    @state =
      rounds: []

  positions: ->
    Game.availablePositions()

  start: ->
    logger.info "Starting a game of Skull"
    for position in @positions()
      @emitPosition position, "roundStarted", position: position
    @engine.transition("startingRound")

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

  # handlePassCards: (position, data) ->
  #   action = actions.PassCards.build(data)
  #   @passedCards[position] = action
  #   if und.size(@passedCards) == 4
  #     errors = {}
  #     for position, action of @passedCards
  #       do (errors, position, action) =>
  #         error = action.validate(@game, position)
  #         if !error
  #           action.execute(@game, position)
  #         else
  #           errors[position] = error
  #     # if @game.currentRound().allHavePassed()
  #     if und.isEmpty(errors)
  #       @game.finishPassing()
  #     else
  #       errorPosition = Object.keys(errors)[0]
  #       @game.abort(errorPosition, errors[errorPosition])

  # handlePlayCard: (position, data) ->
  #   action = actions.PlayCard.build(data)
  #   error = action.validate(@game, position)
  #   if !error
  #     action.execute(@game, position)
  #     if nextPosition = @game.currentRound().currentTrick().positionsMissing()[0]
  #       @game.waitingForCardFrom(nextPosition)
  #     else
  #       @game.finishTrick()
  #   else
  #     @game.abort(position, error)

  # handleReadyForTrick: (position) ->
  #   @readyForTrick.push(position)
  #   if @readyForTrick.length == 4
  #     @game.startTrick()

  # handleFinishedRound: (position) ->
  #   @finishedRound.push(position)
  #   if @finishedRound.length == 4
  #     @game.finishRound()

  # handleFinishedGame: (position) ->
  #   @finishedGame.push(position)
  #   if @finishedGame.length == 4
  #     @game.finish()

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

    # passing:
    #   _onEnter: ->
    #     @passedCards = {}
    #   "passCards.north": (data) ->
    #     @handlePassCards("north", data)
    #   "passCards.east": (data) ->
    #     @handlePassCards("east", data)
    #   "passCards.south": (data) ->
    #     @handlePassCards("south", data)
    #   "passCards.west": (data) ->
    #     @handlePassCards("west", data)

    # startingTrick:
    #   _onEnter: ->
    #     @readyForTrick = []
    #   "readyForTrick.north": ->
    #     @handleReadyForTrick("north")
    #   "readyForTrick.east": ->
    #     @handleReadyForTrick("east")
    #   "readyForTrick.south": ->
    #     @handleReadyForTrick("south")
    #   "readyForTrick.west": ->
    #     @handleReadyForTrick("west")

    # waitingForCardFromNorth:
    #   "playCard.north": (data) ->
    #     @handlePlayCard("north", data)

    # waitingForCardFromEast:
    #   "playCard.east": (data) ->
    #     @handlePlayCard("east", data)

    # waitingForCardFromSouth:
    #   "playCard.south": (data) ->
    #     @handlePlayCard("south", data)

    # waitingForCardFromWest:
    #   "playCard.west": (data) ->
    #     @handlePlayCard("west", data)

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



