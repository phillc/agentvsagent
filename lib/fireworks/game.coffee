{EventEmitter} = require 'events'
logger = require '../logger'
machina = require('machina')()
und = require 'underscore'
Pile = require './pile'

module.exports = class Game
  @availablePositions = -> ["player1", "player2", "player3", "player4", "player5"]

  constructor: ({@positions}) ->
    @engine = new Engine(game: this)
    @emitter = new EventEmitter()
    @seats = und.reduce @positions, (memo, position) ->
      memo[position] = new Pile()
      memo
    , {}
    @deck = Pile.createDeck()
    # @hints = 0
    # @lives = 0

  on: (args...) ->
    @emitter.on(args...)

  emitAll: (message, data) ->
    for position in @positions
      do (position, message, data) =>
        @emitPosition(position, message, data)

  emitPosition: (position, message, data) ->
    logger.verbose "GAME emitting to #{position} - #{message} with #{data}"
    heard = @emitter.emit([position, message].join("."), data)
    if !heard
      logger.error "no one was listening to #{message}"

  handle: (args...) ->
    @engine.handle(args...)

  deal: ->
    @deck.shuffle()
    handSize = if (@positions.length >= 4) then 4 else 5
    for position in @positions
      @deck.moveCardsTo(handSize, @seats[position])

  start: ->
    logger.info "Starting a game of fireworks", @positions
    @deal()
    for position in @positions
      @emitPosition position, "starting", position: position, cards: und.omit(@seats, position)
    @engine.transition("starting")

  waitingFor: (position) ->
    @emitPosition position, "turn", moves: []
    @engine.transition("waitingFor" + position.charAt(0).toUpperCase() + position.slice(1))

  abort: (culprit, error) ->
    logger.warn "Game has been aborted: #{error.type} :: #{error.message}."
    @emitPosition(culprit, "error", error)
    for position in @positions when position isnt culprit
      @emitPosition(position, "error", type: "gameAborted", message: "Game ended due to an invalid action by another agent.")
    @engine.transition("aborted")

Engine = machina.Fsm.extend
  initialize: (options) ->
    @game = options.game
    @on "transition", (details) ->
      logger.verbose "*ENGINE*: changed state from #{details.fromState} to #{details.toState}, because of #{details.action}"

  handleStartingReady: (position) ->
    @startingReady.push(position)
    if @startingReady.length == @game.positions.length
      @game.waitingFor(@game.positions[0])

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
    starting:
      _onEnter: ->
        @startingReady = []
      "ready.player1": -> @handleStartingReady("player1")
      "ready.player2": -> @handleStartingReady("player2")
      "ready.player3": -> @handleStartingReady("player3")
    waitingForPlayer1: {}

